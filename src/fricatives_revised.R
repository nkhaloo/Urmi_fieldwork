library(tidyverse) 
library(lme4)
library(emmeans)
library(patchwork)
library(cowplot)

#load csv's 
raw_data_s1 <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/s1/fricatives/results_s1.csv", col_names = FALSE)
raw_data_s2 <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/s2/fricatives/results_s2.csv", col_names = FALSE)


# Use the first row as the actual column names
column_names <- as.character(unlist(raw_data_s1[1, ]))

# Remove any rows that are duplicates of the column names
cleaned_data_s1 <- raw_data_s1 %>%
  filter(!apply(., 1, function(row) all(row == column_names)))

cleaned_data_s2 <- raw_data_s2 %>%
  filter(!apply(., 1, function(row) all(row == column_names)))

# Assign proper column names
colnames(cleaned_data_s1) <- column_names
colnames(cleaned_data_s2) <- column_names


#add s1 and s2 columns 
df_s1 <- cleaned_data_s1 %>%
  mutate(source = "s1")

df_s2 <- cleaned_data_s2 %>%
  mutate(source = "s2")

#function for fixing up df's 
process_fricative_data <- function(df) {
  df <- df %>%
    # Create fricative column
    mutate(fricative = ifelse(substr(label, 1, 2) == "sh", "sh", substr(label, 1, 1))) %>%
    mutate(fricative = ifelse(fricative == "sh", "ʃ", fricative)) %>%
    
    # Create emphatic column
    mutate(emphasis = case_when(
      grepl("22", label) ~ "emphatic",
      TRUE ~ "plain"
    )) %>%
    
    # Create vowel column
    mutate(vowel = case_when(
      grepl("(ex|ə)$", label) ~ "ə",
      grepl("ah$", label) ~ "ɑ",
      grepl("(a|æ)$", label) & !grepl("ah$", label) ~ "æ",
      grepl("(o|ø)$", label) ~ "ø",
      grepl("i$", label) ~ "i",
      grepl("(u|ʉ)$", label) ~ "ʉ",
      grepl("xx$", label) ~ "əx",  # Updated here
      grepl("(e|ɛ)$", label) ~ "ɛ",
      grepl("ɑ$", label) ~ "ɑ",
      TRUE ~ NA_character_
    )) %>%
    
    # Select COG and necessary columns
    select(fricative, vowel, source, cog, emphasis)
  
  return(df)
}

#run function of s1 data 
df_s1 <- process_fricative_data(df_s1)
df_s2 <- process_fricative_data(df_s2)


# Model for Speaker 1
df_s1$cog <- as.numeric(df_s1$cog)
model_s1 <- lmer(cog ~ fricative + emphasis + (1 |vowel), data = df_s1)
summary(model_s1)

# Model for Speaker 2
df_s2$cog <- as.numeric(df_s2$cog)
model_s2 <- lmer(cog ~ fricative + emphasis + (1| vowel), data = df_s2)
summary(model_s2)


#plot model adjusted results
#speaker 1 

#####change to standard error to keep consistent####### 

em_means_s1 <- emmeans(model_s1, ~ fricative + emphasis)

em_means_s1_df <- as.data.frame(em_means_s1)

s1_plot <- ggplot(em_means_s1_df, aes(x = fricative, y = emmean, color = emphasis)) +
  geom_point(position = position_dodge(width = 0.8), size = 4) +  # Points
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                position = position_dodge(width = 0.8), width = 0.2) +  # Error bars
  labs(
    x = "Fricative",
    y = "COG", 
    title = "Speaker 1"
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.x = element_text(size = 14))

#speaker 2
em_means_s2 <- emmeans(model_s2, ~ fricative + emphasis)

em_means_s2_df <- as.data.frame(em_means_s2)

s2_plot <- ggplot(em_means_s2_df, aes(x = fricative, y = emmean, color = emphasis)) +
  geom_point(position = position_dodge(width = 0.8), size = 4) +  # Points
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                position = position_dodge(width = 0.8), width = 0.2) +  # Error bars
  labs(
    x = "Fricative",
    y = "",
    title = "Speaker 2"
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_blank())


final_plot <- s1_plot + s2_plot + 
  plot_layout(ncol = 2, guides = "collect") +  # Collects all legends into one
  plot_annotation(
    title = "Model Adjusted Center of Gravity (COG) (Hz)", 
    subtitle = NULL
  ) &
  theme(
    legend.position = "bottom",  # Moves the legend to the bottom of the entire plot
    axis.title.y = element_text(size = 14),  # Ensures the y-axis title appears
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  ) &
  labs(color = "Emphasis")


print(final_plot)






