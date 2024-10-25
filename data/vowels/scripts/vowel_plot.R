library(tidyverse)


#import csv 
vowels <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/data/vowels/results.csv")

#check to see all of the unique segment names 
unique(vowels$Segment)


# Add "11" to tokens in "Segment" that don't contain any numbers
# vowels$Segment <- ifelse(!grepl("[0-9]", vowels$Segment), 
#                          paste0(vowels$Segment, "11"), 
#                          vowels$Segment)

#create new vowel column
vowels <- vowels %>%
  mutate(vowel = substr(Segment, 1, 1))


#create emphatic column
vowels <- vowels %>%
  mutate(emphasis = case_when(
    grepl("22", Segment) ~ "emphatic",
    grepl("13$", Segment) ~ "mixed",
    grepl("33", Segment) ~ "mixed",
    TRUE ~ "plain"
  ))


#filter out dipthongs
vowels <- vowels %>%
  filter(!grepl("j", vowel))


#add syllable status column
vowels <- vowels %>%
  mutate(syllable_status = case_when(
    grepl("13$", Segment) ~ "plain_mixed",
    grepl("33$", Segment) ~ "mixed",
    grepl("22$", Segment) ~ "emphatic",
    grepl("11$", Segment) ~ "plain",
    TRUE ~ NA_character_  # For cases that don't match any condition
  ))


###plots###

#filter out epenthetic vowel 
vowel_no_epenthesis <- vowels %>%
  filter(!vowel %in% c("ē", "ū"))
  

#####vowel plot with plain vs. emphatic##### 
vowels_nm <- vowel_no_epenthesis %>% 
  filter(emphasis != "mixed") 

vowels_nm_means <- vowels_nm %>%
  group_by(vowel, emphasis) %>%
  summarise(mean_f1 = mean(F1), 
            mean_f2 = mean(F2))

ggplot(vowels_nm, aes(x = F2, y = F1, color = emphasis, label = vowel)) + 
  # Vowel symbols as points with increased transparency
  geom_text(aes(label = vowel), alpha = 0.3, size = 4) +  # Use vowel symbols instead of points
  # Mean labels
  geom_label(data = vowels_nm_means, 
             aes(x = mean_f2, y = mean_f1, label = vowel, color = emphasis), 
             size = 5, 
             fill = "white",  # White background for labels
             fontface = "bold") + 
  scale_x_reverse() + 
  scale_y_reverse() + 
  scale_color_manual(values = c("plain" = "blue", "emphatic" = "red")) +
  stat_ellipse(aes(group = Segment), alpha = 0.3) +  # Lighter ellipses
  theme_classic() + 
  theme(legend.position = "right") + 
  xlab("Mean F2 (Hz)") + 
  ylab("Mean F1 (Hz)") + 
  coord_fixed(ratio = 10/6)



######vowel plot with plain syllable vs. mixed_plain syllable#####
vowels_ne <- vowel_no_epenthesis %>% 
  filter(syllable_status %in% c("plain", "plain_mixed"))

vowels_ne_means <- vowels_ne %>%
  group_by(vowel, syllable_status) %>%
  summarise(mean_f1 = mean(F1), 
            mean_f2 = mean(F2))

ggplot(vowels_ne, aes(x = F2, y = F1, color = syllable_status, label = vowel)) + 
  geom_label(data = vowels_ne_means, aes(x = mean_f2, y = mean_f1)) + 
  scale_x_reverse() + scale_y_reverse() + 
  scale_color_manual(values = c("plain" = "blue", "plain_mixed" = "orange")) +
  theme_classic() + 
  theme(legend.position="right") + 
  xlab("Mean F2 (Hz)") + 
  ylab("Mean F1 (Hz)") 


######vowel plot with plain syllable vs. mixed syllable#####
vowels_m <- vowel_no_epenthesis %>% 
  filter(syllable_status %in% c("plain", "mixed"))

vowels_m_means <- vowels_m %>%
  group_by(vowel, syllable_status) %>%
  summarise(mean_f1 = mean(F1), 
            mean_f2 = mean(F2))

ggplot(vowels_m, aes(x = F2, y = F1, color = syllable_status, label = vowel)) + 
  geom_label(data = vowels_m_means, aes(x = mean_f2, y = mean_f1)) + 
  scale_x_reverse() + scale_y_reverse() + 
  scale_color_manual(values = c("plain" = "blue", "mixed" = "green3")) +
  theme_classic() + 
  theme(legend.position="right") + 
  xlab("Mean F2 (Hz)") + 
  ylab("Mean F1 (Hz)") 



######vowel plot with emphatic vs. mixed syllable#####
vowels_np <- vowel_no_epenthesis %>% 
  filter(!syllable_status %in% c("plain", "plain_mixed")) 

vowels_np_means <- vowels_np %>%
  group_by(vowel, emphasis) %>%
  summarise(mean_f1 = mean(F1), 
            mean_f2 = mean(F2))

ggplot(vowels_np, aes(x = F2, y = F1, color = emphasis, label = vowel)) + 
  geom_label(data = vowels_np_means, aes(x = mean_f2, y = mean_f1)) + 
  scale_color_manual(values = c("emphatic" = "red", "mixed" = "green3")) +
  scale_x_reverse() + scale_y_reverse() + 
  theme_classic() + 
  theme(legend.position="right") + 
  xlab("Mean F2 (Hz)") + 
  ylab("Mean F1 (Hz)") 


###plot to show that there is a plain/emphatic distinction in epenthetic vowels too### 
###make the f1 axis smaller###
epenthetic <- vowels %>%
  filter(vowel == "ē")


ep_means <- epenthetic %>%
  group_by(vowel, emphasis) %>%
  summarise(mean_f1 = mean(F1), 
            mean_f2 = mean(F2))

ggplot(epenthetic, aes(x = F2, y = F1, color = emphasis, label = vowel)) + 
  geom_label(data = ep_means, aes(x = mean_f2, y = mean_f1)) + 
  scale_x_reverse() + scale_y_reverse() + 
  scale_color_manual(values = c("plain" = "blue", "emphatic" = "red")) +
  theme_classic() + 
  theme(legend.position="right") + 
  xlab("Mean F2 (Hz)") + 
  ylab("Mean F1 (Hz)") 







