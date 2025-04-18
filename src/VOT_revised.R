library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(patchwork)

#load csv's
raw_data_s1 <- read_csv("/Users/noahkhaloo/Desktop/Urmi_fieldwork/s1/stops/results.csv", 
                        locale = locale(encoding = "UTF-16"))
raw_data_s2 <- read_csv("/Users/noahkhaloo/Desktop/Urmi_fieldwork/s2/stops/results.csv",
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
      ),
      VOT = VOT * 1000
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
  coord_cartesian(ylim = c(0, 200)) 

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
  coord_cartesian(ylim = c(0, 200))  # Set the same y-axis limits

# Combine the plots
# Combine the plots without legend title
final_plot <- voiced + voiceless + 
  plot_layout(ncol = 2, guides = "collect") +  
  plot_annotation(title = NULL, subtitle = NULL) &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) &
  guides(fill = guide_legend(title = NULL))  


print(final_plot)

ggsave(filename = "/Users/noahkhaloo/Desktop/Urmi_fieldwork/figures/VOT_by_Emphasis.png", 
       plot = final_plot, 
       width = 8, height = 6, dpi = 300) 


#statistical analysis
#dummy code emphasis
df$emphasis_binary <- ifelse(df$emphasis == "emphatic", 1, 0)
df$voicing_binary <- ifelse(df$voice == "voiced", 1, 0)

mod_1 <- glm(emphasis_binary ~ VOT*voicing_binary, 
               data = df, family = "binomial")

summary(mod_1)
confint(mod_1, method = "Wald")

