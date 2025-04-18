library(tidyverse) 
library(lme4)
library(patchwork)
library(cowplot)
library(lmerTest)
library(sjPlot)

#load csv's 
raw_data_s1 <- read_csv("/Users/noahkhaloo/Desktop/Urmi_fieldwork/s1/fricatives/results_s1.csv", col_names = FALSE)
raw_data_s2 <- read_csv("/Users/noahkhaloo/Desktop/Urmi_fieldwork/s2/fricatives/results_s2.csv", col_names = FALSE)


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


####statistical analysis#####
# Model for Speaker 1
#dummy code emphasis 
df_s1$emphasis_binary <- ifelse(df_s1$emphasis == "emphatic", 1, 0)
#scale cog
df_s1$cog <- as.numeric(df_s1$cog)
df_s1$cog_scaled <- scale(df_s1$cog)

mod_s1 <- glmer(emphasis_binary ~ cog_scaled + fricative + (1|vowel), df_s1, "binomial")
summary(mod_s1)

confint(mod_s1, method = "Wald")

# Model for Speaker 2
#dummy code emphasis 
df_s2$emphasis_binary <- ifelse(df_s2$emphasis == "emphatic", 1, 0)
#scale cog
df_s2$cog <- as.numeric(df_s2$cog)
df_s2$cog_scaled <- scale(df_s2$cog)

mod_s2 <- glmer(emphasis_binary ~ cog_scaled + fricative + (1|vowel), df_s2, "binomial")
summary(mod_s2)

confint(mod_s2, method = "Wald")


#plotting  cog
df_s1_summary <- df_s1 %>%
  group_by(fricative, emphasis) %>%
  summarise(
    mean_cog = mean(cog),
    SE = sd(cog) / sqrt(n())  # Standard Error
  ) %>%
  ungroup()

# Plot
# Calculate y-limits from both dataframes (df_s1_summary and df_s2_summary)
s1 <- ggplot(df_s1_summary, aes(x = fricative, y = mean_cog, fill = emphasis)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_cog - SE, ymax = mean_cog + SE), 
                position = position_dodge(width = 0.8), width = 0.2) +  # Error bars
  labs(
    x = "Fricative",
    y = "COG (dB)",
    title = "Speaker 1",
    color = "Emphasis"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 14),
    #coord_cartesian(ylim = y_limits) 
  )



df_s2_summary <- df_s2 %>%
  group_by(fricative, emphasis) %>%
  summarise(
    mean_cog = mean(cog),
    SE = sd(cog) / sqrt(n())  # Standard Error
  ) %>%
  ungroup()


s2 <- ggplot(df_s2_summary, aes(x = fricative, y = mean_cog, fill = emphasis)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_cog - SE, ymax = mean_cog + SE), 
                position = position_dodge(width = 0.8), width = 0.2) +  # Error bars
  labs(
    x = "Fricative",
    y = "COG (dB)",
    title = "Speaker 1",
    color = "Emphasis"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 14),
    #coord_cartesian(ylim = y_limits) 
  )

fricative_plot <- s1 + s2 +
  plot_layout(ncol = 2, guides = "collect") +  # Collects all legends into one
  plot_annotation(
    title = "Center of Gravity (dB)", 
    subtitle = NULL
  ) &
  theme(
    legend.position = "bottom",  # Moves the legend to the bottom of the entire plot
    axis.title.y = element_text(size = 20),  # Ensures the y-axis title appears
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    plot.title = element_text(size = 20),
    legend.title = element_text(size = 16),  # Increase the size of the legend title
    legend.text = element_text(size = 14),   
    legend.key.size = unit(1.5, "cm"),
    aspect.ratio = 0.6
)
   

ggsave("/Users/noahkhaloo/Desktop/Urmi_fieldwork/figures/fricative_plot.png", 
       plot = fricative_plot + theme(plot.margin = margin(0, 0, 0, 0)), 
       width = 14, height = 7, dpi = 300, units = "in", 
       bg = "transparent", device = "png")

