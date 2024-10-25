# Load necessary packages
library(dplyr)
library(readr)
library(tidyverse)

# # Read the CSV file
# results_df <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/data/morphologically_complex_words/mc_results_s1.csv")
# 
# # Remove the Filename column and create new columns based on the conditions
# results_df <- results_df %>%
#   mutate(
#     word_status = case_when(
#       substr(Segment, 2, 3) == "11" ~ "plain",
#       substr(Segment, 2, 3) == "22" ~ "emphatic",
#       substr(Segment, 2, 3) == "13" ~ "mixed",
#       substr(Segment, 2, 3) == "33" ~ "mixed",
#       TRUE ~ NA_character_ # Default case
#     ),
#     syllable_status = case_when(
#       substr(Segment, 2, 3) == "11" ~ "plain",
#       substr(Segment, 2, 3) == "22" ~ "emphatic",
#       substr(Segment, 2, 3) == "13" ~ "plain",
#       substr(Segment, 2, 3) == "33" ~ "mixed",
#       TRUE ~ NA_character_ # Default case
#     ),
#     morph = case_when(
#       endsWith(Segment, "r") ~ "root",
#       endsWith(Segment, "s") ~ "suffix",
#       endsWith(Segment, "p") ~ "prefix",
#       TRUE ~ NA_character_ # Default case
#     )
#   ) %>%
#   # Replace NaN values with "mixed"
#   mutate(
#     word_status = ifelse(is.na(word_status), "mixed", word_status),
#     syllable_status = ifelse(is.na(syllable_status), "mixed", syllable_status),
#     morph = ifelse(is.na(morph), "mixed", morph)
#   ) %>%
#   # Replace specified vowels in the Segment column
#   mutate(Segment = gsub("e", "ɛ", Segment),
#          Segment = gsub("ej", "ɛj", Segment),
#          Segment = gsub("o", "ø", Segment),
#          Segment = gsub("u", "ʉ", Segment))
# 
# # Add a new column "vowel" based on the first character of each string in "Segment"
# results_df <- results_df %>%
#   mutate(
#     vowel = str_sub(Segment, 1, 1)  # Extracts just the first character
#   )

# Save the modified DataFrame to a new CSV file
#write.csv(results_df, "/Users/ritalavi/Desktop/Urmi_fieldwork/data/morphologically_complex_words/edited_results.csv", row.names = FALSE)


###I have a weird habit of saving df's without renaming them. Ignore the text above unless you have a raw results file###

#load df
mc_vowels <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/data/morphologically_complex_words/mc_results.csv")

#create plain_mixed syllable status
mc_vowels <- mc_vowels %>%
  mutate(syllable_status = case_when(
    grepl("13", Segment) ~ "plain_mixed",
    TRUE ~ syllable_status  # Keep the existing value if no condition is met
  ))


####plain vs emphatic suffixes###
mc_suffix <- mc_vowels %>%
  filter(morph == "suffix", word_status %in% c("plain", "emphatic"))

suffix_means <- mc_suffix %>%
  group_by(vowel, word_status) %>%
  summarise(mean_f1 = mean(F1), 
            mean_f2 = mean(F2))

ggplot(mc_suffix, aes(x = F2, y = F1, color = word_status, label = vowel)) + 
  geom_label(data = suffix_means, aes(x = mean_f2, y = mean_f1)) + 
  scale_x_reverse() + scale_y_reverse() + 
  scale_color_manual(values = c("plain" = "blue", "emphatic" = "red")) +
  theme_classic() + 
  theme(legend.position="right") + 
  xlab("Mean F2 (Hz)") + 
  ylab("Mean F1 (Hz)") 


###plain vs emphatic prefixes### 
mc_prefix <- mc_vowels %>%
  filter(morph == "prefix", vowel != "ʉ", word_status %in% c("plain", "emphatic"))

prefix_means <- mc_prefix %>%
  group_by(vowel, word_status) %>%
  summarise(mean_f1 = mean(F1), 
            mean_f2 = mean(F2))

ggplot(mc_prefix, aes(x = F2, y = F1, color = word_status, label = vowel)) + 
  geom_label(data = prefix_means, aes(x = mean_f2, y = mean_f1)) + 
  scale_x_reverse() + scale_y_reverse() + 
  scale_color_manual(values = c("plain" = "blue", "emphatic" = "red")) +
  theme_classic() + 
  theme(legend.position="right") + 
  xlab("Mean F2 (Hz)") + 
  ylab("Mean F1 (Hz)") 


###plain vs. mixed suffixes###
mixed_suffix <- mc_vowels %>%
  filter(morph == "suffix", syllable_status %in% c("plain", "mixed"))

ms_means <- mixed_suffix %>%
  group_by(vowel, syllable_status) %>%
  summarise(mean_f1 = mean(F1), 
            mean_f2 = mean(F2))

ggplot(mixed_suffix, aes(x = F2, y = F1, color = syllable_status, label = vowel)) + 
  geom_label(data = ms_means, aes(x = mean_f2, y = mean_f1)) + 
  scale_x_reverse() + scale_y_reverse() + 
  scale_color_manual(values = c("plain" = "blue", "mixed" = "green")) +
  theme_classic() + 
  theme(legend.position="right") + 
  xlab("Mean F2 (Hz)") + 
  ylab("Mean F1 (Hz)") 


###mixed vs. emphatic suffixes### 
me_suffix <- mc_vowels %>%
  filter(morph == "suffix", syllable_status %in% c("mixed", "emphatic"))

me_means <- me_suffix %>%
  group_by(vowel, syllable_status) %>%
  summarise(mean_f1 = mean(F1), 
            mean_f2 = mean(F2))

ggplot(me_suffix, aes(x = F2, y = F1, color = syllable_status, label = vowel)) + 
  geom_label(data = me_means, aes(x = mean_f2, y = mean_f1)) + 
  scale_x_reverse() + scale_y_reverse() + 
  scale_color_manual(values = c("emphatic" = "red", "mixed" = "green4")) +
  theme_classic() + 
  theme(legend.position="right") + 
  xlab("Mean F2 (Hz)") + 
  ylab("Mean F1 (Hz)") 




