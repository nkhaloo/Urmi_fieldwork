library(tidyverse)
library(lme4)
library(lmerTest)
library(rlang)

#import csv's
df_s1_raw <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/s1/vowels/results_praat.csv")
df_s2_raw <- read_delim("/Users/ritalavi/Desktop/Urmi_fieldwork/s2/vowels/results_praat.csv")

#create function that processes df's
process_vowels <- function(df) {
  if ("Segment" %in% colnames(df)) {
    df <- df %>%
      # Remove anything following "-" in Filename
      mutate(Filename = sub("-.*", "", Filename)) %>%
      
      # Filter out rows where Segment starts with "xx"
      filter(!str_starts(Segment, "xx")) %>%
      
      # Assign vowel values based on Segment
      mutate(vowel = case_when(
        str_starts(Segment, "ah") ~ "ɑ",
        str_starts(Segment, "a") & !str_starts(Segment, "ah") ~ "æ",
        str_starts(Segment, "i") ~ "i",
        str_starts(Segment, "e") & !str_starts(Segment, "ex") ~ "ɛ",
        str_starts(Segment, "ex") ~ "ə",
        str_starts(Segment, "o") ~ "ø",
        str_starts(Segment, "u") ~ "y",
        TRUE ~ NA_character_
      )) %>%
      
      # Create emphasis column
      mutate(emphasis = case_when(
        grepl("22", Segment) ~ "emphatic",
        grepl("13$", Segment) ~ "mixed",
        grepl("33", Segment) ~ "mixed",
        TRUE ~ "plain"
      )) %>%
      
      # Add syllable status column
      mutate(syllable_status = case_when(
        grepl("13$", Segment) ~ "plain_mixed",
        grepl("33$", Segment) ~ "mixed",
        grepl("22$", Segment) ~ "emphatic",
        grepl("11$", Segment) ~ "plain",
        TRUE ~ NA_character_
      )) %>%
      
      
      # Filter out diphthongs (e.g., vowels starting with "j")
      filter(!grepl("j", vowel)) %>%
      
    
    return(df)
  } else {
    stop("The dataframe does not have a 'Segment' column.")
  }
}


#call the function
df_s1 <- process_vowels(df_s1_raw) %>%
  mutate(speaker = "s1")
df_s2 <- process_vowels(df_s2_raw) %>%
  mutate(speaker = "s2")

#combine the two dataframes 
df <- rbind(df_s1, df_s2)

#add stress column from different df 
stress <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/metadata/stress.csv")

#join them 
# Adding row IDs to help with the join
df <- df %>%
  mutate(row_id = row_number())
stress <- stress %>% 
  mutate(row_id = row_number()) %>%
  select(stress, row_id)

# Perform the join using both Filename and row_id
result <- left_join(df, stress, by =  "row_id")

# drop the row_id column after the join
result <- result %>%
  select(-row_id)

# create phonetic categories 
result <- result %>%
  mutate(phonetic_vowel = case_when(
    vowel == "ɑ" & emphasis == "emphatic" ~ "ɑˠ",
    vowel == "æ" & emphasis == "emphatic" ~ "æˠ",
    vowel == "ø" & emphasis == "emphatic" ~ "øˠ",
    vowel == "y" & emphasis == "emphatic" ~ "yˠ",
    vowel == "ɛ" & emphasis == "emphatic" ~ "ɛˠ",
    vowel == "ə" & emphasis == "emphatic" ~ "əˠ",
    vowel == "i" & emphasis == "emphatic" ~ "iˠ",
    TRUE ~ vowel  
  ))

#filter out na
result <- result %>%
  filter(!is.na(vowel))

#### mahalanobis distances #### 
# Function to calculate Mahalanobis distance for F1, F2
vmahalanobis_f1f2 = function (dat) {
  if (nrow(dat) < 25) {
    dat$zF1F2 = NA
    return(dat)
  }
  means = c(mean(dat$F1, na.rm=T), mean(dat$F2, na.rm=T))
  cov = cov(cbind(dat$F1, dat$F2))
  
  dat$zF1F2 = mahalanobis(cbind(dat$F1, dat$F2),
                          center=means, cov=cov)
  dat
}

# Set the outlier threshold
distance_cutoff <- 6

# Perform Mahalanobis on separate dataset for speaker 
df_s1_filtered <-  result %>%
  filter(speaker == "s1")

df_s1_filtered <- df_s1_filtered %>%                 
  group_by(phonetic_vowel) %>%
  do(vmahalanobis_f1f2(.)) %>%
  ungroup() %>%
  mutate(formant_outlier = NA)


df_s2_filtered <- result %>%
  filter(speaker == "s2")

df_s2_filtered <- df_s2_filtered %>%                 
  group_by(phonetic_vowel) %>%
  do(vmahalanobis_f1f2(.)) %>%
  ungroup() %>%
  mutate(formant_outlier = NA)


# Remove flagged values
for (i in 1:nrow(df_s1_filtered)) {
  if (!is.na(df_s1_filtered$zF1F2[i])) {
    if (df_s1_filtered$zF1F2[i] > distance_cutoff){
      df_s1_filtered$formant_outlier[i] = "outlier"
    }
  }
  
}

for (i in 1:nrow(df_s2_filtered)) {
  if (!is.na(df_s2_filtered$zF1F2[i])) {
    if (df_s2_filtered$zF1F2[i] > distance_cutoff){
      df_s2_filtered$formant_outlier[i] = "outlier"
    }
  }
  
}

df_s1 <- df_s1_filtered %>%
  filter(formant_outlier != "outlier" | is.na(formant_outlier))
df_s2 <- df_s2_filtered %>%
  filter(formant_outlier != "outlier" | is.na(formant_outlier))



#####plotting#####

#plain vs emphatic 
plot_vowel_means <- function(df) {
  vowels_nm <- df %>% 
    filter(emphasis != "mixed") 
  
  vowels_nm_means <- vowels_nm %>%
    group_by(vowel, emphasis) %>%
    summarise(mean_f1 = mean(F1), 
              mean_f2 = mean(F2))
  
  plot <- ggplot(vowels_nm, aes(x = F2, y = F1, color = emphasis, label = vowel)) + 
    geom_text(aes(label = vowel), alpha = 0.3, size = 4) + 
    geom_label(data = vowels_nm_means, 
               aes(x = mean_f2, y = mean_f1, label = vowel, color = emphasis), 
               size = 5, 
               fill = "white",  
               fontface = "bold") + 
    scale_x_reverse() + 
    scale_y_reverse() + 
    scale_color_manual(values = c("plain" = "blue", "emphatic" = "red")) +
    stat_ellipse(aes(group = Segment), alpha = 0.3) +  
    theme_classic() + 
    theme(legend.position = "right") + 
    xlab("Mean F2 (Hz)") + 
    ylab("Mean F1 (Hz)") + 
    coord_fixed(ratio = 10/6) +
    guides(color = guide_legend(title = "Emphasis"))
  
  return(plot)
}

# call the fucntion
vowel_space_s1 <- plot_vowel_means(df_s1)
vowel_space_s2 <- plot_vowel_means(df_s2)


#mixed words 
plot_mixed <- function(df) {
  # Filter vowels_nm and compute means
  vowels_nm <- df %>% 
    filter(emphasis != "mixed")
  
  vowels_nm_means <- vowels_nm %>%
    group_by(vowel, emphasis) %>%
    summarise(mean_f1 = mean(F1), 
              mean_f2 = mean(F2))
  
  # Filter vowels_np and compute means
  vowels_np <- df %>% 
    filter(syllable_status == "mixed")
  
  vowels_np_means <- vowels_np %>%
    group_by(vowel, syllable_status) %>%
    summarise(mean_f1 = mean(F1), 
              mean_f2 = mean(F2))
  
  # Create the plot
  ggplot(vowels_nm, aes(x = F2, y = F1, color = emphasis, label = vowel)) + 
    # Vowel symbols as points with increased transparency
    geom_text(aes(label = vowel), alpha = 0.3, size = 4) +  
    # Mean labels from vowels_nm_means
    geom_label(data = vowels_nm_means, 
               aes(x = mean_f2, y = mean_f1, label = vowel, color = emphasis), 
               size = 5, 
               fill = "white",  
               fontface = "bold") + 
    # Semi-transparent mean labels from vowels_np_means for mixed vowels
    geom_label(data = filter(vowels_np_means, syllable_status == "mixed"), 
               aes(x = mean_f2, y = mean_f1, label = vowel),  # No color mapping
               size = 7, 
               fill = "lightgrey",  
               fontface = "bold",
               alpha = 0.5, 
               color = "black") +  # Fixed color for mixed vowel labels
    scale_x_reverse() + 
    scale_y_reverse() + 
    scale_color_manual(values = c("plain" = "blue", "emphatic" = "red")) +
    stat_ellipse(aes(group = Segment), alpha = 0.3) +  
    theme_classic() + 
    theme(legend.position = "right") + 
    xlab("Mean F2 (Hz)") + 
    ylab("Mean F1 (Hz)") + 
    coord_fixed(ratio = 10/6) +
    guides(color = guide_legend(title = "Word Status"))
}

mixed_vowels_s1 <- plot_mixed(df_s1)
mixed_vowels_s2 <- plot_mixed(df_s2)
  

##### morphologically complex words ####
#load csv's 
mc_s1_raw <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/s1/morphologically_complex_words/results_praat.csv")
mc_s2_raw <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/s2/morphologically_complex_words/results_praat.csv")

#updated process_vowels function
mc_vowels <- function(df) {
  if ("Segment" %in% colnames(df)) {
    df <- df %>%
      # Create affix column based on last character of Segment
      mutate(affix = case_when(
        str_ends(Segment, "r") ~ "root",
        str_ends(Segment, "s") ~ "suffix",
        str_ends(Segment, "p") ~ "prefix",
        TRUE ~ NA_character_
      )) %>%
      
      # Remove final r, s, or p from Segment
      mutate(Segment = str_replace(Segment, "[rsp]$", "")) %>%
      
      # Remove anything following "-" in Filename
      mutate(Filename = sub("-.*", "", Filename)) %>%
      
      # Filter out rows where Segment starts with "xx"
      filter(!str_starts(Segment, "xx")) %>%
      
      # Assign vowel values based on Segment
      mutate(vowel = case_when(
        str_starts(Segment, "ah") ~ "ɑ",
        str_starts(Segment, "a") & !str_starts(Segment, "ah") ~ "æ",
        str_starts(Segment, "i") ~ "i",
        str_starts(Segment, "e") & !str_starts(Segment, "ex") ~ "ɛ",
        str_starts(Segment, "ex") ~ "ə",
        str_starts(Segment, "o") ~ "ø",
        str_starts(Segment, "u") ~ "y",
        TRUE ~ NA_character_
      )) %>%
      
      # Create emphasis column
      mutate(emphasis = case_when(
        grepl("22", Segment) ~ "emphatic",
        grepl("13$", Segment) ~ "mixed",
        grepl("33", Segment) ~ "mixed",
        TRUE ~ "plain"
      )) %>%
      
      # Add syllable status column
      mutate(syllable_status = case_when(
        grepl("13$", Segment) ~ "plain_mixed",
        grepl("33$", Segment) ~ "mixed",
        grepl("22$", Segment) ~ "emphatic",
        grepl("11$", Segment) ~ "plain",
        TRUE ~ NA_character_
      )) %>%
      
      # Filter out diphthongs (e.g., vowels starting with "j")
      filter(!grepl("j", vowel)) 
    
    return(df)
  } else {
    stop("The dataframe does not have a 'Segment' column.")
  }
}

#run function 
mc_s1 <- mc_vowels(mc_s1_raw)
mc_s2 <- mc_vowels(mc_s2_raw)

#new function for plotting mc words 
mc_plots <- function(vowel_df, affix_df, morph_type) {
  # Filter vowels excluding "mixed" emphasis
  vowels_nm <- vowel_df %>% 
    filter(emphasis != "mixed") 
  
  # Compute mean F1 and F2 values
  vowels_nm_means <- vowels_nm %>%
    group_by(vowel, emphasis) %>%
    summarise(mean_f1 = mean(F1), 
              mean_f2 = mean(F2), 
              .groups = "drop")
  
  # Filter for specified morph_type in affix_df
  mc <- affix_df %>%
    filter(affix == morph_type, emphasis %in% c("plain", "emphatic"))
  
  # Compute mean F1 and F2 values for suffixes
  suffix_means <- mc %>%
    group_by(vowel, emphasis) %>%
    summarise(mean_f1 = mean(F1), 
              mean_f2 = mean(F2), 
              .groups = "drop")
  
  # Generate vowel plot
  ggplot(vowels_nm, aes(x = F2, y = F1, color = emphasis, label = vowel)) + 
    geom_text(aes(label = vowel), alpha = 0.3, size = 4) +  
    geom_label(data = vowels_nm_means, 
               aes(x = mean_f2, y = mean_f1, label = vowel, color = emphasis), 
               size = 5, fill = "white", fontface = "bold") + 
    geom_label(data = filter(suffix_means, emphasis == emphasis), 
               aes(x = mean_f2, y = mean_f1, label = vowel, color = emphasis),  
               size = 7, fontface = "bold", alpha = 0.5,
               fill = ifelse(suffix_means$emphasis == "plain", "blue", "red"),  # Color inside the label
               colour = "black",  # Black border
               label.padding = unit(0.5, "lines"), 
               label.r = unit(0.15, "lines")) +  
    scale_x_reverse() + 
    scale_y_reverse() + 
    scale_color_manual(values = c("plain" = "blue", "emphatic" = "red")) +
    stat_ellipse(aes(group = Segment), alpha = 0.3) +  
    theme_classic() + 
    theme(legend.position = "right") + 
    xlab("Mean F2 (Hz)") + 
    ylab("Mean F1 (Hz)") + 
    coord_fixed(ratio = 10/6) +
    guides(color = guide_legend(title = "Word Status"))
}

#suffixes
s1_suffixes <- mc_plots(df_s1, mc_s1, "suffix")
s2_suffixes <- mc_plots(df_s2, mc_s2, "suffix")

#prefixes
mc_s1_filtered <- mc_s1 %>% filter(vowel != "y")
s1_prefixes <- mc_plots(df_s1, mc_s1_filtered, "prefix")
s2_prefixes <- mc_plots(df_s2, mc_s2, "prefix")






