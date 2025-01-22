library(tidyverse)
library(lme4)
library(lmerTest)

#import csv's
df_s1_raw <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/s1/vowels/results_praat.csv")
df_s2_raw <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/s2/vowels/results_praat.csv")

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
      filter(!grepl("j", vowel))
    
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
df <- df %>% mutate(row_id = row_number())
stress <- stress %>% mutate(row_id = row_number())

# Perform the join using both Filename and row_id
result <- left_join(df, stress, by =  "row_id")

# drop the row_id column after the join
result <- result %>%
  select(-row_id, -Filename.y) %>%  # Drop row_id and Filename.y
  rename(Filename = Filename.x)  
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

# Perform Mahalanobis on dataset
result =  result %>%                 #MG: this was cut from a dataset called "tot_fin"
  group_by(phonetic_vowel) %>%
  do(vmahalanobis_f1f2(.)) %>%
  ungroup() %>%
  mutate(formant_outlier = NA)



# Remove flagged values
for (i in 1:nrow(result)) {
  if (!is.na(result$zF1F2[i])) {
    if (result$zF1F2[i] > distance_cutoff){
      result$formant_outlier[i] = "outlier"
    }
  }
  
}

#filter out outliers 
result <- result %>%
  filter(formant_outlier != "outlier" | is.na(formant_outlier))


