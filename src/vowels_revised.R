library(tidyverse)
library(lme4)
library(lmerTest)
library(rlang)
library(patchwork)
library(broom)
library(sjPlot)


#import csv's
df_s1_raw <- read_csv("/Users/noahkhaloo/Desktop/Urmi_fieldwork/s1/vowels/results_praat.csv")
df_s2_raw <- read_delim("/Users/noahkhaloo/Desktop/Urmi_fieldwork/s2/vowels/results_praat.csv")

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
df_s1_raw <- process_vowels(df_s1_raw) %>%
  mutate(speaker = "s1")
df_s2_raw <- process_vowels(df_s2_raw) %>%
  mutate(speaker = "s2")

#combine the two dataframes 
df <- rbind(df_s1_raw, df_s2_raw)

#add stress column from different df 
stress <- read_csv("/Users/noahkhaloo/Desktop/Urmi_fieldwork/metadata/stress.csv")

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
    vowel == "ɑ" & syllable_status %in% c("emphatic", "mixed") ~ "ɑˠ",
    vowel == "æ" & syllable_status %in% c("emphatic", "mixed")  ~ "æˠ",
    vowel == "ø" & syllable_status %in% c("emphatic", "mixed")  ~ "øˠ",
    vowel == "y" & syllable_status %in% c("emphatic", "mixed")  ~ "yˠ",
    vowel == "ɛ" & syllable_status %in% c("emphatic", "mixed")  ~ "ɛˠ",
    vowel == "ə" & syllable_status %in% c("emphatic", "mixed")  ~ "əˠ",
    vowel == "i" & syllable_status %in% c("emphatic", "mixed")  ~ "iˠ",
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
plot_vowel_means <- function(df, plot_title = "Vowel Plot") {
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
    theme(legend.position = "right",
          plot.margin = margin(0, 0, 0, 0),
          plot.title = element_text(face = "bold", size = 20, hjust = 0)) +  # Left-aligned title
    ggtitle(plot_title) +  # Add title
    xlab("Mean F2 (Hz)") + 
    ylab("Mean F1 (Hz)") + 
    coord_fixed(ratio = 10/6) +
    guides(color = guide_legend(title = "Emphasis"))
  
  # Save the plot to the specified folder with the title as the filename
  # ggsave(filename = paste0("/Users/ritalavi/Desktop/Urmi_fieldwork/figures/", plot_title, ".png"), 
  #        plot = plot, 
  #        width = 8, height = 6, dpi = 300)  # You can adjust size and resolution as needed
  
  return(plot)
}

# call the function
vowel_space_s1 <- plot_vowel_means(df_s1, "Speaker 1")
vowel_space_s2 <- plot_vowel_means(df_s2, "Speaker 2")



#######statistical model##### 
#speaker 1
#filter out mixed
df_s1_nm <- df_s1 %>%
  filter(emphasis != "mixed")

#dummy code emphasis variable 
df_s1_nm$emphasis_binary <- ifelse(df_s1_nm$emphasis == "emphatic", 1, 0)

#turn vowel into a factor
df_s1$vowel <- factor(df_s1$vowel)

#scale F2/F2
df_s1_nm$F2_scaled <- scale(df_s1_nm$F2)
df_s1_nm$F1_scaled <- scale(df_s1_nm$F1)

#set ref level to /i/
df_s1_nm$vowel <- relevel(factor(df_s1_nm$vowel), ref = "i")

#model
mod_s1 <- glm(emphasis_binary ~ F2_scaled*vowel + F1_scaled, df_s1_nm, 
              family = "binomial")

summary_s1 <- summary(mod_s1)
conf_int_s1 <- confint(mod_s1, method = "Wald")


#speaker 2
#filter out mixed
df_s2_nm <- df_s2 %>%
  filter(emphasis != "mixed")

#dummy code emphasis variable 
df_s2_nm$emphasis_binary <- ifelse(df_s2_nm$emphasis == "emphatic", 1, 0)

#turn vowel into a factor
df_s2_nm$vowel <- relevel(factor(df_s2_nm$vowel), ref = "i")

#scale F2/F2
df_s2_nm$F2_scaled <- scale(df_s2_nm$F2)
df_s2_nm$F1_scaled <- scale(df_s2_nm$F1)


#model
mod_s2 <- glm(emphasis_binary ~ F2_scaled*vowel + F1_scaled, df_s2_nm, 
              family = "binomial")

summary(mod_s2)

#get 95 CI 
confint(mod_s2, method = "Wald")

#create single df for model 
df_bs <- rbind(df_s1_nm, df_s2_nm)

#model 
mod_bs <- glmer(emphasis_binary ~ F2_scaled*vowel + F1_scaled + (1|speaker), df_bs, 
                family = "binomial")
summary(mod_bs)


#######mixed words########
plot_mixed <- function(df, syllable_status_filter = NULL, plot_title = NULL) {
  # Filter vowels_nm and compute means
  vowels_nm <- df %>% 
    filter(emphasis != "mixed")
  
  vowels_nm_means <- vowels_nm %>%
    group_by(vowel, emphasis) %>%
    summarise(mean_f1 = mean(F1), 
              mean_f2 = mean(F2))
  
  # Filter vowels_np and compute means
  vowels_np <- df %>% 
    filter(syllable_status == syllable_status_filter)
  
  vowels_np_means <- vowels_np %>%
    group_by(vowel, syllable_status) %>%
    summarise(mean_f1 = mean(F1), 
              mean_f2 = mean(F2))
  
  # Create the plot
  ggplot(vowels_nm, aes(x = F2, y = F1, color = emphasis, label = vowel)) + 
    # Vowel symbols as points with increased transparency
    #geom_text(aes(label = vowel), alpha = 0.3, size = 4) +  
    # Mean labels from vowels_nm_means
    geom_label(data = vowels_nm_means, 
               aes(x = mean_f2, y = mean_f1, label = vowel, color = emphasis), 
               size = 5, 
               fill = "white",  
               fontface = "bold") + 
    # Semi-transparent mean labels from vowels_np_means for mixed vowels
    geom_label(data = filter(vowels_np_means, syllable_status == syllable_status_filter), 
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
    theme(legend.position = "right",
          plot.margin = margin(0, 0, 0, 0)) + 
    xlab("Mean F2 (Hz)") + 
    ylab("Mean F1 (Hz)") + 
    coord_fixed(ratio = 10/6) +
    guides(color = guide_legend(title = "Word Status"))
  
}

#mixed vowels
mixed_vowels_s1 <- plot_mixed(df_s1, "mixed")
mixed_vowels_s2 <- plot_mixed(df_s2, "mixed")

#plain mixed vowels 
plain_mixed_vowels_s1 <- plot_mixed(df_s1, "plain_mixed")
plain_mixed_vowels_s2 <- plot_mixed(df_s2, "plain_mixed")

#combine the plots 
s1_mixed <- plain_mixed_vowels_s1 + mixed_vowels_s1 + 
  plot_layout(ncol = 2, guides = "collect") +  # Collects all legends into one
  plot_annotation(
    title = "Speaker 1", 
    subtitle = NULL
  ) &
  theme(
    legend.position = "bottom",  # Moves the legend to the bottom of the entire plot
    axis.title.y = element_text(size = 14),  # Ensures the y-axis title appears
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold")
  )

s2_mixed <- plain_mixed_vowels_s2 + mixed_vowels_s2 + 
  plot_layout(ncol = 2, guides = "collect") +  # Collects all legends into one
  plot_annotation(
    title = "Speaker 2", 
    subtitle = NULL
  ) &
  theme(
    legend.position = "bottom",  # Moves the legend to the bottom of the entire plot
    axis.title.y = element_text(size = 14),  # Ensures the y-axis title appears
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold")
  )

# #ggsave("/Users/ritalavi/Desktop/Urmi_fieldwork/figures/s1_mixed_plot.png", 
#        plot = s1_mixed + theme(plot.margin = margin(0, 0, 0, 0)), 
#        width = 14, height = 10, dpi = 300, units = "in", 
#        bg = "transparent", device = "png")
# 
# 
# #ggsave("/Users/ritalavi/Desktop/Urmi_fieldwork/figures/s2_mixed_plot.png", 
#        plot = s2_mixed + theme(plot.margin = margin(0, 0, 0, 0)), 
#        width = 14, height = 10, dpi = 300, units = "in", 
#        bg = "transparent", device = "png")




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
mc_plots <- function(vowel_df, affix_df, morph_type, plot_title = NULL) {
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
  p <- ggplot(vowels_nm, aes(x = F2, y = F1, color = emphasis, label = vowel)) + 
    #geom_text(aes(label = vowel), alpha = 0.3, size = 4) +  
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
    theme( legend.position = "right",  # Keeps the legend on the right
           legend.title = element_text(size = 16, face = "bold"),  # Increases legend title size
           legend.text = element_text(size = 14) ) + 
    xlab("Mean F2 (Hz)") + 
    ylab("Mean F1 (Hz)") + 
    coord_fixed(ratio = 10/6) +
    guides(color = guide_legend(title = "Word Status"))
  
  # Add title if specified
  if (!is.null(plot_title)) {
    p <- p + ggtitle(plot_title) +
      theme(plot.title = element_text(size = 16, face = "bold"))
  }
  return(p)  # Return the plot object explicitly
}

#suffixes
s1_suffixes <- mc_plots(df_s1, mc_s1, "suffix", "Speaker 1 suffixes")
#ggsave(filename = "/Users/ritalavi/Desktop/Urmi_fieldwork/figures/s1_suffixes.png", 
       plot = s1_suffixes, 
       width = 8, height = 6, dpi = 300) 

s2_suffixes <- mc_plots(df_s2, mc_s2, "suffix", "Speaker 2 suffixes")
#ggsave(filename = "/Users/ritalavi/Desktop/Urmi_fieldwork/figures/s2_suffixes.png", 
       plot = s2_suffixes, 
       width = 8, height = 6, dpi = 300) 

#prefixes
s1_prefixes <- mc_plots(df_s1, mc_s1, "prefix", "Speaker 1 prefixes")
#ggsave(filename = "/Users/ritalavi/Desktop/Urmi_fieldwork/figures/s1_prefixes.png", 
       plot = s1_prefixes, 
       width = 8, height = 6, dpi = 300) 

s2_prefixes <- mc_plots(df_s2, mc_s2, "prefix", "Speaker 2 prefixes")
#ggsave(filename = "/Users/ritalavi/Desktop/Urmi_fieldwork/figures/s2_prefixes.png", 
       plot = s2_prefixes, 
       width = 8, height = 6, dpi = 300) 








