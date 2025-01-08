library(tidyverse)

#import csv 
vowels_total <- read_delim(
  "/Users/ritalavi/Desktop/Urmi_fieldwork/s1/vowels/VS_s1.csv",
  delim = "\t",
  show_col_types = FALSE
)

#select columns that we need 
vowels <- vowels_total%>%
  select(Filename, Label, seg_Start, seg_End, sF1, sF2, sF3)

#create duration column 
vowels <- vowels %>%
  mutate(duration = seg_End - seg_Start)


#filter out na's
vowels <- vowels %>%
  filter(!is.na(sF1) & !is.na(sF2) & !is.na(sF3))


#remove any non-numeric values 
vowels$sF1 <- as.numeric(gsub("[^0-9.-]", "", vowels$sF1))
vowels$sF2 <- as.numeric(gsub("[^0-9.-]", "", vowels$sF2))
vowels$sF3 <- as.numeric(gsub("[^0-9.-]", "", vowels$sF3))


#get mean value for formants
vowel_means <- vowels %>%
  group_by(duration) %>%
  summarise(
    mean_sF1 = mean(sF1, na.rm = TRUE),
    mean_sF2 = mean(sF2, na.rm = TRUE),
    mean_sF3 = mean(sF3, na.rm = TRUE)
  )


# Join the calculated means back to the original dataset based on 'duration'
vowels <- vowels %>%
  left_join(vowel_means, by = "duration") %>%
  distinct(duration, .keep_all = TRUE)  # Keep only one row per 'duration'


#re-select 
vowels <- vowels %>%
  select(Filename, Label, duration, mean_sF1, mean_sF2, mean_sF3)


#create new vowel column
vowels <- vowels %>%
  mutate(vowel = substr(Label, 1, 1))


vowels <- vowels %>%
  filter(!str_starts(Label, "xx")) %>%  # Filter out rows where Label starts with "xx"
  mutate(vowel = case_when(
    str_starts(Label, "ah") ~ "ɑ",   # If label starts with "ah", assign "ɑ"
    str_starts(Label, "a") & !str_starts(Label, "ah") ~ "æ",  # If label starts with "a" but not "ah", assign "æ"
    str_starts(Label, "i") ~ "i",
    str_starts(Label, "e") ~ "ɛ",
    str_starts(Label, "o") ~ "ø",
    str_starts(Label, "u") ~ "y",
    str_starts(Label, "ex") ~ "ə",
    TRUE ~ NA_character_  # This ensures that if no condition is met, 'vowel' will be NA
  ))

#create emphasis column 
vowels <- vowels %>%
  mutate(emphasis = case_when(
    grepl("22", Label) ~ "emphatic",
    grepl("13$", Label) ~ "mixed",
    grepl("33", Label) ~ "mixed",
    TRUE ~ "plain"
  ))


#add syllable status column
vowels <- vowels %>%
  mutate(syllable_status = case_when(
    grepl("13$", Label) ~ "plain_mixed",
    grepl("33$", Label) ~ "mixed",
    grepl("22$", Label) ~ "emphatic",
    grepl("11$", Label) ~ "plain",
    TRUE ~ NA_character_  # For cases that don't match any condition
  ))


#rename
vowels <- vowels %>%
  rename( 
    F1 = mean_sF1, 
    F2 = mean_sF2, 
    F3 = mean_sF3)

#filter out NA
vowels <- vowels %>%
  filter(!is.na(vowel))

#calculate malanobis distance for formants 
vmahalanobis = function (dat) {
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

# Distance larger than 6 is considered as outlier    #MG: smaller numbers = more outliers. 
distance_cutoff = 6

# Perform Mahalanobis on dataset
vowels_filtered =  vowels %>%                
  group_by(vowel) %>%
  do(vmahalanobis(.)) %>%
  ungroup() %>%
  mutate(formant_outlier = NA)

# Visualize the formants with flagged values
vowels_filtered %>%
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = F2, y = F1, color = zF1F2 > distance_cutoff)) +       
  geom_point(size = 0.6) +
  facet_wrap(.~vowel)+
  scale_y_reverse(limits = c(2000,0),position = "right") +
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()






