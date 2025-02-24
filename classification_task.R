# Load required packages
library(tuneR)    # for audio file reading and processing
library(rPraat)   # for reading Praat TextGrid files
# Make sure the package that provides melfcc() (e.g., phonTools or similar) is installed and loaded
library(tidyverse)
# Define the directory containing your audio and TextGrid files
data_dir <- "/Users/noahkhaloo/Desktop/Urmi_fieldwork/s1/vowels/vowels_arpabet"

# List all .wav files in the directory
wav_files <- list.files(path = data_dir, pattern = "\\.wav$", full.names = TRUE)
cat("Found", length(wav_files), "wav files.\n")

# Initialize a list to store all MFCC results
mfcc_results <- list()

# Loop over each WAV file
for (wav_path in wav_files) {
  
  cat("\nProcessing file:", wav_path, "\n")
  
  # Construct the corresponding TextGrid file path (assumes same base filename)
  base_name <- tools::file_path_sans_ext(basename(wav_path))
  tg_path <- file.path(data_dir, paste0(base_name, ".TextGrid"))
  
  # Check if the TextGrid exists; if not, skip this file
  if (!file.exists(tg_path)) {
    cat("  No corresponding TextGrid file for", wav_path, "\n")
    next
  }
  
  # Read in the audio file and TextGrid
  wave_obj <- readWave(wav_path)
  tg <- tg.read(tg_path)
  
  # Get the tier names and check for the "vowels" tier
  tier_names <- sapply(tg, function(x) x$name)
  if (!("vowels" %in% tier_names)) {
    cat("  Tier 'vowels' not found in", tg_path, "\n")
    next
  }
  
  # Extract the "vowels" tier
  vowels_tier <- tg[[which(tier_names == "vowels")]]
  
  # Determine the number of intervals from the length of t1 (assuming parallel vectors)
  num_intervals <- length(vowels_tier$t1)
  cat("  Number of intervals in the 'vowels' tier:", num_intervals, "\n")
  
  # Loop through each interval by index
  for (i in seq_len(num_intervals)) {
    
    # Trim the label (to remove any extra whitespace)
    token_label <- trimws(vowels_tier$label[i])
    
    # Skip intervals with empty labels
    if (nchar(token_label) == 0) {
      cat("    Skipping interval", i, "with empty label.\n")
      next
    }
    
    # Get start and end times
    start_time <- vowels_tier$t1[i]
    end_time   <- vowels_tier$t2[i]
    duration   <- end_time - start_time
    if (duration <= 0) {
      cat("    Skipping interval", i, "with non-positive duration.\n")
      next
    }
    
    cat("    Processing interval", i, "with label", token_label, "from", start_time, "to", end_time, "\n")
    
    # Extract the audio segment for this interval (times in seconds)
    segment <- extractWave(wave_obj, from = start_time, to = end_time, xunit = "time")
    
    # Compute MFCCs for the segment; wrapped in tryCatch to handle any errors/warnings
    mfcc_features <- tryCatch({
      melfcc(segment, sr = wave_obj@samp.rate)
    }, warning = function(w) {
      cat("      Warning computing MFCC for", token_label, ":", w$message, "\n")
      invokeRestart("muffleWarning")
    }, error = function(e) {
      cat("      Error computing MFCC for", token_label, ":", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(mfcc_features)) next  # skip if MFCC computation failed
    
    # Save the result in our results list
    mfcc_results[[length(mfcc_results) + 1]] <- list(
      file   = basename(wav_path),
      label  = token_label,
      start  = start_time,
      end    = end_time,
      mfcc   = mfcc_features
    )
  }
}

# Create a tibble with each element of mfcc_results as a row.
df_mfcc <- tibble(
  file  = sapply(mfcc_results, function(x) x$file),
  label = sapply(mfcc_results, function(x) x$label),
  start = sapply(mfcc_results, function(x) x$start),
  end   = sapply(mfcc_results, function(x) x$end),
  mfcc  = lapply(mfcc_results, function(x) x$mfcc)  # keep as list column
)

# Print the resulting data frame
print(df_mfcc)

# Convert the list column to separate columns
df_expanded <- df_mfcc %>%
  mutate(mfcc = lapply(mfcc, function(x) if(is.matrix(x)) as.vector(x) else x)) %>% 
  unnest_wider(mfcc, names_sep = "_")

# Inspect the expanded data frame
print(df_expanded)


