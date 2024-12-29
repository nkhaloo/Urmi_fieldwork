library(tidyverse)


#import csv 
vowels <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/data/vowels/vowel_results_s1.csv")

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


#get count of mixed tokens 
foo <- vowels %>%
  filter(syllable_status == "mixed")

nrow(foo)


###plots###

#filter out epenthetic vowel and /a/
vowels <- vowels %>%
  filter(!vowel %in% c("ē", "ū")) %>%
  filter((vowel != "ɑ"))
  

#####vowel plot with plain vs. emphatic##### 
vowels_nm <- vowels %>% 
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
  stat_ellipse(aes(group = Segment), alpha = 0.3) +  
  theme_classic() + 
  theme(legend.position = "right") + 
  xlab("Mean F2 (Hz)") + 
  ylab("Mean F1 (Hz)") + 
  coord_fixed(ratio = 10/6) +
  guides(color = guide_legend(title = "Word Status"))

# Save the plot
ggsave(filename = "/Users/ritalavi/Desktop/Urmi_fieldwork/data/figures/plain_vs_emphatic_vowels.png", 
       plot = plot, 
       width = 8, height = 6, dpi = 300)


###mixed versus emphatic words###
vowels_np <- vowels %>% 
  filter(!syllable_status %in% c("plain_mixed", "plain", "emphatic"))

vowels_np_means <- vowels_np %>%
  group_by(vowel, syllable_status) %>%
  summarise(mean_f1 = mean(F1), 
            mean_f2 = mean(F2))

#plot mixed vowels superimposed onto original vowel plot
plot_2 <- ggplot(vowels_nm, aes(x = F2, y = F1, color = emphasis, label = vowel)) + 
  # Vowel symbols as points with increased transparency
  geom_text(aes(label = vowel), alpha = 0.3, size = 4) +  
  # Mean labels from vowels_nm_means
  geom_label(data = vowels_nm_means, 
             aes(x = mean_f2, y = mean_f1, label = vowel, color = emphasis), 
             size = 5, 
             fill = "white",  
             fontface = "bold") + 
  # Semi-transparent mean labels from suffix_means without color mapping
  geom_label(data = filter(vowels_np_means, syllable_status == "mixed"), 
             aes(x = mean_f2, y = mean_f1, label = vowel),  # No color mapping
             size = 8, 
             fill = "lightgrey",  
             fontface = "bold",
             alpha = 0.5, 
             color = "black") +  # Fixed color for suffix_means labels
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


#plain_mixed
vowels_pm <- vowels %>% 
  filter(syllable_status == "plain_mixed")

vowels_pm_means <- vowels_pm %>%
  group_by(vowel, syllable_status) %>%
  summarise(mean_f1 = mean(F1), 
            mean_f2 = mean(F2))

#plot mixed vowels superimposed onto original vowel plot
plot_4 <- ggplot(vowels_nm, aes(x = F2, y = F1, color = emphasis, label = vowel)) + 
  # Vowel symbols as points with increased transparency
  geom_text(aes(label = vowel), alpha = 0.3, size = 4) +  
  # Mean labels from vowels_nm_means
  geom_label(data = vowels_nm_means, 
             aes(x = mean_f2, y = mean_f1, label = vowel, color = emphasis), 
             size = 5, 
             fill = "white",  
             fontface = "bold") + 
  # Semi-transparent mean labels from suffix_means without color mapping
  geom_label(data = filter(vowels_pm_means, syllable_status == "plain_mixed"), 
             aes(x = mean_f2, y = mean_f1, label = vowel),  # No color mapping
             size = 8, 
             fill = "lightgrey",  
             fontface = "bold",
             alpha = 0.5, 
             color = "black") +  # Fixed color for suffix_means labels
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


ggsave(filename = "/Users/ritalavi/Desktop/Urmi_fieldwork/data/figures/plain_mixed.png", 
       plot = plot_4, 
       width = 8, height = 6, dpi = 300)


#load morphological complex vowels
mc_vowels <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/data/morphologically_complex_words/mc_results_s1.csv")

#create plain_mixed syllable status
mc_vowels <- mc_vowels %>%
  mutate(syllable_status = case_when(
    grepl("13", Segment) ~ "plain_mixed",
    TRUE ~ syllable_status  # Keep the existing value if no condition is met
  ))

#get suffix means 
mc_suffix <- mc_vowels %>%
  filter(morph == "suffix", word_status %in% c("plain", "emphatic"))

suffix_means <- mc_suffix %>%
  group_by(vowel, word_status) %>%
  summarise(mean_f1 = mean(F1), 
            mean_f2 = mean(F2))


#vowel plot with suffixes superimposed 
#plain suffixes
plot_3 <- ggplot(vowels_nm, aes(x = F2, y = F1, color = emphasis, label = vowel)) + 
  # Vowel symbols as points with increased transparency
  geom_text(aes(label = vowel), alpha = 0.3, size = 4) +  
  # Mean labels from vowels_nm_means
  geom_label(data = vowels_nm_means, 
             aes(x = mean_f2, y = mean_f1, label = vowel, color = emphasis), 
             size = 5, 
             fill = "white",  
             fontface = "bold") + 
  # Semi-transparent mean labels from suffix_means without color mapping
  geom_label(data = filter(suffix_means, word_status == "plain"), 
             aes(x = mean_f2, y = mean_f1, label = vowel),  # No color mapping
             size = 8, 
             fill = "lightgrey",  
             fontface = "bold",
             alpha = 0.5, 
             color = "black") +  # Fixed color for suffix_means labels
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

ggsave(filename = "/Users/ritalavi/Desktop/Urmi_fieldwork/data/figures/plain_suffixes.png", 
       plot = plot_3, 
       width = 8, height = 6, dpi = 300)


#emphatic suffixes
plot_4 <- ggplot(vowels_nm, aes(x = F2, y = F1, color = emphasis, label = vowel)) + 
  # Vowel symbols as points with increased transparency
  geom_text(aes(label = vowel), alpha = 0.3, size = 4) +  
  # Mean labels from vowels_nm_means
  geom_label(data = vowels_nm_means, 
             aes(x = mean_f2, y = mean_f1, label = vowel, color = emphasis), 
             size = 5, 
             fill = "white",  
             fontface = "bold") + 
  # Semi-transparent mean labels from suffix_means (filtered for emphatic)
  geom_label(data = filter(suffix_means, word_status == "emphatic"), 
             aes(x = mean_f2, y = mean_f1, label = vowel),  
             size = 8, 
             fill = "lightgrey",  
             fontface = "bold",
             alpha = 0.5, 
             color = "black") +  
  scale_x_reverse() + 
  scale_y_reverse() + 
  scale_color_manual(values = c("plain" = "blue", "emphatic" = "red"), 
                     labels = c("Plain" = "plain", "Emphatic" = "emphatic")) +  # Set custom legend labels
  stat_ellipse(aes(group = Segment), alpha = 0.3) +  
  theme_classic() + 
  theme(legend.position = "right") + 
  xlab("Mean F2 (Hz)") + 
  ylab("Mean F1 (Hz)") + 
  coord_fixed(ratio = 10/6) + 
  guides(color = guide_legend(title = "Word Status"))  # Custom legend title

ggsave(filename = "/Users/ritalavi/Desktop/Urmi_fieldwork/data/figures/emphatic_suffixes.png", 
       plot = plot_4, 
       width = 8, height = 6, dpi = 300)

#prefixes
mc_prefix <- mc_vowels %>%
  filter(morph == "prefix", vowel != "ʉ", word_status %in% c("plain", "emphatic"))

prefix_means <- mc_prefix %>%
  group_by(vowel, word_status) %>%
  summarise(mean_f1 = mean(F1), 
            mean_f2 = mean(F2))

#emphatic prefixes
plot_5 <- ggplot(vowels_nm, aes(x = F2, y = F1, color = emphasis, label = vowel)) + 
  # Vowel symbols as points with increased transparency
  geom_text(aes(label = vowel), alpha = 0.3, size = 4) +  
  # Mean labels from vowels_nm_means
  geom_label(data = vowels_nm_means, 
             aes(x = mean_f2, y = mean_f1, label = vowel, color = emphasis), 
             size = 5, 
             fill = "white",  
             fontface = "bold") + 
  # Semi-transparent mean labels from suffix_means (filtered for emphatic)
  geom_label(data = filter(prefix_means, word_status == "emphatic"), 
             aes(x = mean_f2, y = mean_f1, label = vowel),  
             size = 8, 
             fill = "lightgrey",  
             fontface = "bold",
             alpha = 0.5, 
             color = "black") +  
  scale_x_reverse() + 
  scale_y_reverse() + 
  scale_color_manual(values = c("plain" = "blue", "emphatic" = "red"), 
                     labels = c("Plain" = "plain", "Emphatic" = "emphatic")) +  # Set custom legend labels
  stat_ellipse(aes(group = Segment), alpha = 0.3) +  
  theme_classic() + 
  theme(legend.position = "right") + 
  xlab("Mean F2 (Hz)") + 
  ylab("Mean F1 (Hz)") + 
  coord_fixed(ratio = 10/6) + 
  guides(color = guide_legend(title = "Word Status")) 


ggsave(filename = "/Users/ritalavi/Desktop/Urmi_fieldwork/data/figures/emphatic_prefixes.png", 
       plot = plot_5, 
       width = 8, height = 6, dpi = 300)


#plain prefixes
plot_6 <- ggplot(vowels_nm, aes(x = F2, y = F1, color = emphasis, label = vowel)) + 
  # Vowel symbols as points with increased transparency
  geom_text(aes(label = vowel), alpha = 0.3, size = 4) +  
  # Mean labels from vowels_nm_means
  geom_label(data = vowels_nm_means, 
             aes(x = mean_f2, y = mean_f1, label = vowel, color = emphasis), 
             size = 5, 
             fill = "white",  
             fontface = "bold") + 
  # Semi-transparent mean labels from suffix_means (filtered for emphatic)
  geom_label(data = filter(prefix_means, word_status == "plain"), 
             aes(x = mean_f2, y = mean_f1, label = vowel),  
             size = 8, 
             fill = "lightgrey",  
             fontface = "bold",
             alpha = 0.5, 
             color = "black") +  
  scale_x_reverse() + 
  scale_y_reverse() + 
  scale_color_manual(values = c("plain" = "blue", "emphatic" = "red"), 
                     labels = c("Plain" = "plain", "Emphatic" = "emphatic")) +  # Set custom legend labels
  stat_ellipse(aes(group = Segment), alpha = 0.3) +  
  theme_classic() + 
  theme(legend.position = "right") + 
  xlab("Mean F2 (Hz)") + 
  ylab("Mean F1 (Hz)") + 
  coord_fixed(ratio = 10/6) + 
  guides(color = guide_legend(title = "Word Status")) 

ggsave(filename = "/Users/ritalavi/Desktop/Urmi_fieldwork/data/figures/plain_prefixes.png", 
       plot = plot_6, 
       width = 8, height = 6, dpi = 300)




#####could be useful for combinign data for s1 and s2#####
##see inclass ggplot excercise from psych 201a
# combine the plots
#p1 + (p2 + p3) + 
  #plot_layout(ncol = 1) &
  #plot_annotation(tag_levels = "A") & 
  #theme_classic() &
  #theme(plot.tag = element_text(face = "bold", size = 20))







