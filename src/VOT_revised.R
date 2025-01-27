library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(patchwork)

#load csv's
raw_data_s1 <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/s1/stops/results.csv", 
                        locale = locale(encoding = "UTF-16"))
raw_data_s2 <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/s2/stops/results.csv",
                         locale = locale(encoding = "UTF-16"))

#make function to process data 
process_VOT <- function(df, speaker_value = "s1") {
  df %>%
    mutate(
      stop = case_when(
        grepl("^ch|^CH|^č|^Č", Word) ~ "t͡ʃ",       # Words starting with "ch", "CH", "č", or "Č"
        grepl("^j|^J", Word) ~ "d͡ʒ",               # Words starting with "j" or "J"
        TRUE ~ tolower(substr(Word, 1, 1))          # All other cases: lowercase first letter
      ),
      emphasis = ifelse(grepl("^[A-Z]", Word), "emphatic", "plain"), # Capitalized = "emphatic"
      speaker = speaker_value,                       # Use the specified speaker value
      voice = case_when(
        stop %in% c("b", "d", "g", "d͡ʒ") ~ "voiced",  # Voiced stops
        TRUE ~ "voiceless"                           # All other stops
      )
    )
}


#apply function to df's
df_s1 <- process_VOT(raw_data_s1, speaker_value = "s1")
df_s2 <- process_VOT(raw_data_s2, "s2")

#combine them
df <- rbind(df_s1, df_s2)

#filter 1 obvious outlier 
df <- df %>%
  filter(VOT >= -2)

#plot VOT 
# Calculate the range of the VOT values for both plots
y_limits <- c(min(df$VOT, na.rm = TRUE), max(df$VOT, na.rm = TRUE))

# Voiced plot
voiced <- ggplot(df %>% filter(stop %in% c("b", "d", "g", "d͡ʒ")), 
                 aes(x = factor(stop, levels = c("b", "d", "g", "d͡ʒ")),
                                y = VOT, fill = emphasis)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = 0.9), alpha = 1) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", 
               position = position_dodge(width = 0.75), width = 0.2) +
  labs(x = "Stop", y = "Mean VOT (ms)", title = "Voiced Stops") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14)) +
  coord_cartesian(ylim = y_limits)  # Set the same y-axis limits

# Voiceless plot
voiceless <- ggplot(df %>% filter(stop %in% c("p", "t", "k", "q", "t͡ʃ")), 
                    aes(x = factor(stop, levels = c("p", "t", "k", "q", "t͡ʃ")), 
                        y = VOT, fill = emphasis)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = 0.9), alpha = 1) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", 
               position = position_dodge(width = 0.75), width = 0.2) +
  labs(x = "Stop", y = "", title = "Voiceless Stops") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_blank()) +
  coord_cartesian(ylim = y_limits)  # Set the same y-axis limits

# Combine the plots
final_plot <- voiced + voiceless + 
  plot_layout(ncol = 2, guides = "collect") +  # Collects all legends into one
  plot_annotation(
    title = "VOT(Ms) by Emphasis", 
    subtitle = NULL
  ) &
  theme(
    legend.position = "bottom",  # Moves the legend to the bottom of the entire plot
    axis.title.y = element_text(size = 14),  # Ensures the y-axis title appears
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

print(final_plot)

#model
model <- lmer(VOT ~ voice + emphasis + (1 | speaker) + (1| stop), data = df)
summary(model)


