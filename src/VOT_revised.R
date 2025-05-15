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
        grepl("^ch|^CH|^č|^Č", Word) ~ "tʃ",       # Words starting with "ch", "CH", "č", or "Č"
        grepl("^j|^J", Word) ~ "dʒ",               # Words starting with "j" or "J"
        TRUE ~ tolower(substr(Word, 1, 1))          # All other cases: lowercase first letter
      ),
      emphasis = ifelse(grepl("^[A-Z]", Word), "emphatic", "plain"), # Capitalized = "emphatic"
      speaker = speaker_value,                       # Use the specified speaker value
      voice = case_when(
        stop %in% c("b", "d", "g", "dʒ") ~ "voiced",  # Voiced stops
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

# First, summarize data to get N per category
df_summary <- df %>%
  group_by(stop, emphasis) %>%
  summarise(
    mean_VOT = mean(VOT),
    SE = sd(VOT)/sqrt(n()),
    N = n(),
    .groups = 'drop'
  )

# Voiced Stops plot with N labels
voiced <- ggplot(df_summary %>% filter(stop %in% c("b", "d", "g", "dʒ")),
                 aes(x = factor(stop, levels = c("b", "d", "g", "dʒ")),
                     y = mean_VOT, fill = emphasis)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_VOT - SE, ymax = mean_VOT + SE),
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_text(aes(y = mean_VOT + SE + 8, label = N),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4.5) +
  labs(x = "Stop", y = "Mean VOT (ms)", title = "Voiced Stops") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),
        panel.grid = element_blank()) +
  scale_fill_manual(values = c("plain" = "blue", "emphatic" = "red")) + 
  coord_cartesian(ylim = c(0, 200))

# Voiceless Stops plot with N labels
voiceless <- ggplot(df_summary %>% filter(stop %in% c("p", "t", "k", "q", "tʃ")),
                    aes(x = factor(stop, levels = c("p", "t", "k", "q", "tʃ")),
                        y = mean_VOT, fill = emphasis)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_VOT - SE, ymax = mean_VOT + SE),
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_text(aes(y = mean_VOT + SE + 8, label = N),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4.5) +
  labs(x = "Stop", y = "", title = "Voiceless Stops") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),
        panel.grid = element_blank(),
        axis.text.y = element_blank()) +
  scale_fill_manual(values = c("plain" = "blue", "emphatic" = "red")) + 
  coord_cartesian(ylim = c(0, 200)) 

# Combine plots with shared legend
final_plot <- voiced + voiceless +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(title = NULL) &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) &
  guides(fill = guide_legend(title = NULL))

# Display the plot
print(final_plot)

# Save the plot
ggsave(filename = "/Users/noahkhaloo/Desktop/Urmi_fieldwork/figures/VOT_by_Emphasis.png",
       plot = final_plot,
       width = 8, height = 6, dpi = 300)



### Poster plot
# Voiced Stops (no axis titles, bigger text)
voiced_poster <- ggplot(df_summary %>% filter(stop %in% c("b", "d", "g", "dʒ")),
                        aes(x = factor(stop, levels = c("b", "d", "g", "dʒ")),
                            y = mean_VOT, fill = emphasis)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_VOT - SE, ymax = mean_VOT + SE),
                position = position_dodge(width = 0.9), width = 0.4, linewidth = 1.5) +
  geom_text(aes(y = mean_VOT + SE + 8, label = N),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 10, fontface = "bold") +
  labs(x = NULL, y = NULL, title = "Voiced Stops") +
  theme_classic(base_size = 28) +
  theme(
    axis.text = element_text(size = 45, face = "bold"),
    plot.title = element_text(size = 55, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 45, face = "bold"),
    plot.margin = margin(10, 40, 10, 40)
  ) +
  coord_cartesian(ylim = c(0, 230), expand = FALSE) +
  scale_fill_manual(values = c("plain" = "blue", "emphatic" = "red")) + 
  guides(fill = guide_legend(title = NULL))

# Voiceless Stops (no axis titles, bigger text)
voiceless_poster <- ggplot(df_summary %>% filter(stop %in% c("p", "t", "k", "q", "tʃ")),
                           aes(x = factor(stop, levels = c("p", "t", "k", "q", "tʃ")),
                               y = mean_VOT, fill = emphasis)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_VOT - SE, ymax = mean_VOT + SE),
                position = position_dodge(width = 0.9), width = 0.4, linewidth = 1.5) +
  geom_text(aes(y = mean_VOT + SE + 8, label = N),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 10, fontface = "bold") +
  labs(x = NULL, y = NULL, title = "Voiceless Stops") +
  theme_classic(base_size = 28) +
  theme(
    axis.text = element_text(size = 45, face = "bold"),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 55, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 45, face = "bold"),
    plot.margin = margin(10, 40, 10, 40)
  ) +
  coord_cartesian(ylim = c(0, 230), expand = FALSE) +
  scale_fill_manual(values = c("plain" = "blue", "emphatic" = "red")) + 
  guides(fill = guide_legend(title = NULL))

# Combine plots vertically, compressed vertically
final_plot_poster <- (voiced_poster / voiceless_poster) +
  plot_layout(guides = "collect", heights = c(1, 1)) +
  plot_annotation(
    theme = theme(
      legend.position = "bottom",
      legend.text = element_text(size = 45, face = "bold"),
      plot.margin = margin(10,10,10,10)
    )
  )

# Add shared y-axis title
final_plot_poster <- wrap_elements(final_plot_poster) +
  labs(tag = "Mean VOT (ms)") + 
  theme(
    plot.tag = element_text(angle = 90, size = 55, face = "bold", vjust = 0.5, hjust = 0.5),
    plot.tag.position = "left"
  )

# Display final combined plot
print(final_plot_poster)

# Save final adjusted plot clearly wider and shorter
ggsave(filename = "/Users/noahkhaloo/Desktop/Urmi_fieldwork/figures/VOT_combined_poster.png",
       plot = final_plot_poster,
       width = 20, height = 10, dpi = 300)


#statistical analysis
#dummy code emphasis
df$emphasis_binary <- ifelse(df$emphasis == "emphatic", 1, 0)
df$voicing_binary <- ifelse(df$voice == "voiced", 1, 0)

mod_1 <- glm(emphasis_binary ~ VOT*voicing_binary, 
               data = df, family = "binomial")

summary(mod_1)
confint(mod_1, method = "Wald")

