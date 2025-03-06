library(tuneR)  
library(rPraat)
library(tidyverse)
library(xgboost)    
library(caret)   

### A. Define a processing function for vowels (for both MFCC & formant data)
process_vowels <- function(df) {
  df %>%
    filter(!str_starts(label, "xx")) %>%
    mutate(vowel = case_when(
      str_starts(label, "ah") ~ "ɑ",
      str_starts(label, "a") & !str_starts(label, "ah") ~ "æ",
      str_starts(label, "i") ~ "i",
      str_starts(label, "e") & !str_starts(label, "ex") ~ "ɛ",
      str_starts(label, "ex") ~ "ə",
      str_starts(label, "o") ~ "ø",
      str_starts(label, "u") ~ "y",
      TRUE ~ NA_character_
    )) %>%
    mutate(emphasis = case_when(
      grepl("22", label) ~ "emphatic",
      grepl("13$", label) ~ "mixed",
      grepl("33", label) ~ "mixed",
      TRUE ~ "plain"
    )) %>%
    mutate(syllable_status = case_when(
      grepl("13$", label) ~ "plain_mixed",
      grepl("33$", label) ~ "mixed",
      grepl("22$", label) ~ "emphatic",
      grepl("11$", label) ~ "plain",
      TRUE ~ NA_character_
    )) %>%
    filter(!grepl("j", vowel)) %>%
    filter(vowel != "ə")
}

### B. Define a function to process MFCC data for a speaker
process_speaker_mfcc <- function(data_dir) {
  wav_files <- list.files(path = data_dir, pattern = "\\.wav$", full.names = TRUE)
  mfcc_results <- list()
  
  for (wav_path in wav_files) {
    base_name <- tools::file_path_sans_ext(basename(wav_path))
    tg_path <- file.path(data_dir, paste0(base_name, ".TextGrid"))
    if (!file.exists(tg_path)) next
    
    wave_obj <- readWave(wav_path)
    tg <- tg.read(tg_path)
    # Assume the first tier is vowels
    vowels_tier <- tg[[1]]
    num_intervals <- length(vowels_tier$t1)
    
    for (i in seq_len(num_intervals)) {
      token_label <- trimws(vowels_tier$label[i])
      if (nchar(token_label) == 0) next
      
      start_time <- vowels_tier$t1[i]
      end_time <- vowels_tier$t2[i]
      
      segment <- extractWave(wave_obj, from = start_time, to = end_time, xunit = "time")
      
      mfcc_features <- tryCatch({
        melfcc(segment, sr = wave_obj@samp.rate, numcep = 14, nbands = 20)
      }, warning = function(w) {
        invokeRestart("muffleWarning")
      }, error = function(e) {
        return(NULL)
      })
      if (is.null(mfcc_features)) next
      
      mfcc_results[[length(mfcc_results) + 1]] <- list(
        file   = basename(wav_path),
        label  = token_label,
        start  = start_time,
        end    = end_time,
        mfcc   = mfcc_features
      )
    }
  }
  
  df <- tibble(
    file  = sapply(mfcc_results, function(x) x$file),
    label = sapply(mfcc_results, function(x) x$label),
    start = sapply(mfcc_results, function(x) x$start),
    end   = sapply(mfcc_results, function(x) x$end),
    mfcc  = lapply(mfcc_results, function(x) x$mfcc)
  )
  
  # Use a 3-frame window around the midpoint to summarize MFCCs
  df <- df %>%
    mutate(mid_window_mfcc = map(mfcc, function(mat) {
      if (is.matrix(mat)) {
        mid_index <- ceiling(ncol(mat) / 2)
        indices <- (mid_index - 1):(mid_index + 1)
        indices <- indices[indices >= 1 & indices <= ncol(mat)]
        mfcc_window <- apply(mat[, indices, drop = FALSE], 1, mean)
        if (length(mfcc_window) < 14) {
          mfcc_window <- c(mfcc_window, rep(NA, 14 - length(mfcc_window)))
        } else if (length(mfcc_window) > 14) {
          mfcc_window <- mfcc_window[1:14]
        }
        mfcc_window
      } else {
        mat
      }
    })) %>%
    unnest_wider(mid_window_mfcc, names_sep = "_")
  
  colnames(df)[grepl("^mid_window_mfcc_", colnames(df))] <- paste0("MFCC_", 1:14)
  return(df)
}

# Process MFCC data for Speaker 1 and Speaker 2
df_mfcc_s1 <- process_speaker_mfcc("/Users/noahkhaloo/Desktop/Urmi_fieldwork/s1/vowels/vowels_arpabet")
df_mfcc_s2 <- process_speaker_mfcc("/Users/noahkhaloo/Desktop/Urmi_fieldwork/s2/vowels/soundfiles_textgrids")

# Combine MFCC data from both speakers
df_mfcc_all <- bind_rows(df_mfcc_s1, df_mfcc_s2)

# Clean and label the combined MFCC data
df_mfcc_all <- process_vowels(df_mfcc_all)

### C. Process Formant Data

process_formants <- function(file_path) {
  df <- read_csv(file_path) %>%
    rename(label = Segment)
  df <- process_vowels(df)
  return(df)
}

# Process formant data for Speaker 1 and Speaker 2
df_formants_s1 <- process_formants("/Users/noahkhaloo/Desktop/Urmi_fieldwork/s1/vowels/results_praat.csv")
df_formants_s2 <- process_formants("/Users/noahkhaloo/Desktop/Urmi_fieldwork/s2/vowels/results_praat.csv")

# Combine formant data from both speakers
df_formants_all <- bind_rows(df_formants_s1, df_formants_s2)


# Check distribution of plain and emphatic
df_counts <- as_tibble(df_formants_all %>%
                         filter(emphasis != "mixed") %>%
  count(vowel, emphasis))

# For example, if you want to merge based on vowel labels:
combined_df <- left_join(df_formants_all, df_mfcc_all, by = "vowel")


#### D. Balance the Data ####

# For MFCC data
balance_by_vowel <- function(df, vowel_col = "vowel", emphasis_col = "emphasis", groups = c("plain", "emphatic")) {
  df %>%
    filter((!!sym(emphasis_col)) %in% groups) %>%
    group_by(!!sym(vowel_col)) %>%
    group_modify(~ {
      min_count <- min(table(.x[[emphasis_col]]))
      .x %>% 
        group_by(!!sym(emphasis_col)) %>%
        slice_sample(n = min_count) %>% 
        ungroup()
    }) %>%
    ungroup()
}

# Join df's 
df_formants_all <- df_formants_all %>%
  group_by(Filename, label) %>%
  mutate(token_index = row_number()) %>%
  ungroup() %>%
  mutate(token_id = paste(Filename, label, token_index, sep = "_"))



# MFCC balanced 
df_balanced_mfcc <- balance_by_vowel(df_mfcc_all, vowel_col = "vowel", emphasis_col = "emphasis")


# Formant balanced
df_balanced_formants <- balance_by_vowel(df_formants_all, vowel_col = "vowel", emphasis_col = "emphasis")





### E. XGBoost Modeling Function (Same as before)
run_xgb <- function(df, 
                    vowel_col = "vowel", 
                    label_col = "emphasis", 
                    pos_label = "emphatic", 
                    neg_label = "plain",
                    feature_cols = tidyselect::starts_with("MFCC_"),
                    min_tokens = 10,
                    seed = 123,
                    nrounds = 50,
                    nfold = 5,
                    early_stopping_rounds = 10) {
  df_final <- df %>%
    filter((!!sym(label_col)) %in% c(neg_label, pos_label)) %>%
    select(!!sym(vowel_col), !!sym(label_col), {{feature_cols}})
  
  set.seed(seed)
  
  results <- list()
  vowels <- unique(df_final[[vowel_col]])
  
  for (v in vowels) {
    df_subset <- df_final %>% filter((!!sym(vowel_col)) == v)
    if (nrow(df_subset) < min_tokens) next
    
    X <- df_subset %>%
      select({{feature_cols}}) %>%
      mutate(across(everything(), as.numeric)) %>%
      as.matrix()
    
    y <- as.numeric(ifelse(df_subset[[label_col]] == pos_label, 1, 0))
    
    dtrain <- xgb.DMatrix(data = X, label = y)
    
    params <- list(
      objective = "binary:logistic",
      eval_metric = "error",
      max_depth = 3,
      eta = 0.1,
      verbosity = 0
    )
    
    cv <- xgb.cv(
      params = params,
      data = dtrain,
      nrounds = nrounds,
      nfold = nfold,
      verbose = 0,
      early_stopping_rounds = early_stopping_rounds,
      maximize = FALSE
    )
    
    best_error <- min(cv$evaluation_log$test_error_mean)
    best_accuracy <- 1 - best_error
    
    results[[v]] <- list(
      vowel = v,
      n_tokens = nrow(df_subset),
      best_error = best_error,
      best_accuracy = best_accuracy
    )
  }
  results_df <- bind_rows(results)
  return(results_df)
}



### F. Run Models
# Model using only MFCC features (from combined speakers)
df_final_mfcc <- df_mfcc_all %>%
  filter(emphasis %in% c("plain", "emphatic")) %>%
  select(vowel, emphasis, starts_with("MFCC_"))

results_mfcc_df <- run_xgb(df_final_mfcc,
                           vowel_col = "vowel",
                           label_col = "emphasis",
                           pos_label = "emphatic",
                           neg_label = "plain",
                           feature_cols = tidyselect::starts_with("MFCC_"))

print(results_mfcc_df)

# Model using only formant features (F1 and F2) from combined speakers
results_formants_df <- run_xgb(df_formants_all,
                               vowel_col = "vowel",
                               label_col = "emphasis",
                               pos_label = "emphatic",
                               neg_label = "plain",
                               feature_cols = c("F1", "F2"))
print(results_formants_df)

