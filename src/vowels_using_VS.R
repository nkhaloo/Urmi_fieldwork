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
    str_starts(Label, "i") ~ "i",  # If label starts with "i", assign "i"
    str_starts(Label, "e") & !str_starts(Label, "ex") ~ "ɛ",  # If label starts with "e" but not "ex", assign "ɛ"
    str_starts(Label, "ex") ~ "ə",  # If label starts with "ex", assign "ə"
    str_starts(Label, "o") ~ "ø",  # If label starts with "o", assign "ø"
    str_starts(Label, "u") ~ "y",  # If label starts with "u", assign "y"
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


#####outlier detection######
#just f1 and f2
vowels_f1f2 <- vowels %>%
  select(-F3)

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
vowels_f1f2 =  vowels_f1f2 %>%                 #MG: this was cut from a dataset called "tot_fin"
  group_by(vowel) %>%
  do(vmahalanobis_f1f2(.)) %>%
  ungroup() %>%
  mutate(formant_outlier = NA)


# Visualize the formants with flagged values
vowels_f1f2 %>%
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = F2, y = F1, color = zF1F2 > distance_cutoff)) +       #MG: sF2 and sF1 = Snack values from VS
  geom_point(size = 0.6) +
  facet_wrap(.~vowel)+
  scale_y_reverse(limits = c(2000,0),position = "right") +
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()

# Remove flagged values
for (i in 1:nrow(vowels_f1f2)) {
  if (!is.na(vowels_f1f2$zF1F2[i])) {
    if (vowels_f1f2$zF1F2[i] > distance_cutoff){
      vowels_f1f2$formant_outlier[i] = "outlier"
    }
  }
  
}

# Visualize the vowel formant after exclusion
vowels_f1f2 %>%
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = F2, y = F1)) +
  geom_point(size = 0.6) +
  #geom_text()+
  facet_wrap(.~vowel)+
  #geom_density_2d() +
  #  scale_color_manual(values=c('#a6611a','#dfc27d','#018571'))+
  scale_y_reverse(limits = c(2000,0),position = "right") +
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()


#filter out outliers 
vowels_f1f2_filtered <- vowels_f1f2 %>%
  filter(formant_outlier != "outlier" | is.na(formant_outlier))

#filter out crazy outlier for /y/
vowels_f1f2_filtered <- vowels_f1f2_filtered %>%
  filter(!(vowel == "y" & F2 > 2000))


#####vowel plot with plain vs. emphatic##### 
vowels_nm <- vowels_f1f2_filtered %>% 
  filter(emphasis != "mixed") 

vowels_nm_means <- vowels_nm %>%
  group_by(vowel, emphasis) %>%
  summarise(mean_f1 = mean(F1), 
            mean_f2 = mean(F2))

# plot
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
  stat_ellipse(aes(group = Label), alpha = 0.3) +  
  theme_classic() + 
  theme(legend.position = "right") + 
  xlab("Mean F2 (Hz)") + 
  ylab("Mean F1 (Hz)") + 
  coord_fixed(ratio = 10/6) +
  guides(color = guide_legend(title = "Word Status"))




