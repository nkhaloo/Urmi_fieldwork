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


# Summarize data including N
df_s1_summary <- df_s1 %>%
  group_by(fricative, emphasis) %>%
  summarise(mean_cog = mean(cog), SE = sd(cog)/sqrt(n()), N = n(), .groups = 'drop')

df_s2_summary <- df_s2 %>%
  group_by(fricative, emphasis) %>%
  summarise(mean_cog = mean(cog), SE = sd(cog)/sqrt(n()), N = n(), .groups = 'drop')
# Explicit dodge width
pd <- position_dodge(width = 0.8)

# Determine max limits for y-axis
y_max_s1 <- max(df_s1_summary$mean_cog + df_s1_summary$SE) + 600
y_max_s2 <- max(df_s2_summary$mean_cog + df_s2_summary$SE) + 400

# Speaker 1 plot (title adjusted upwards)
s1 <- ggplot(df_s1_summary, aes(x = fricative, y = mean_cog, fill = emphasis)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_cog - SE, ymax = mean_cog + SE), position = pd, width = 0.2) +
  geom_text(aes(label = N, y = mean_cog + SE + 300), position = pd, size = 5) +
  labs(x = "Fricative", y = "COG (dB)", title = "Speaker 1") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, y_max_s1)) +
  scale_fill_manual(values = c("plain" = "blue", "emphatic" = "red")) + 
  theme(axis.text.x = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0, margin = margin(b = 20)))

# Speaker 2 plot (title adjusted upwards)
s2 <- ggplot(df_s2_summary, aes(x = fricative, y = mean_cog, fill = emphasis)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_cog - SE, ymax = mean_cog + SE), position = pd, width = 0.2) +
  geom_text(aes(label = N, y = mean_cog + SE + 175), position = pd, size = 5) +
  labs(x = "Fricative", y = "COG (dB)", title = "Speaker 2") +
  scale_fill_manual(values = c("plain" = "blue", "emphatic" = "red")) + 
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, y_max_s2)) +
  theme(axis.text.x = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0, margin = margin(b = 20)))

# Combine plots
fricative_plot <- s1 + s2 +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(title = NULL) &
  theme(legend.position = "bottom",
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm"),
        aspect.ratio = 0.6) &
  guides(fill = guide_legend(title = NULL))

# Save plot
ggsave("/Users/noahkhaloo/Desktop/Urmi_fieldwork/figures/fricative_plot.png",
       plot = fricative_plot + theme(plot.margin = margin(0, 0, 0, 0)),
       width = 14, height = 7, dpi = 300, units = "in",
       bg = "transparent", device = "png")


# Poster plots 
y_max_s1 <- 12000

# Define position dodge
pd <- position_dodge(width = 0.7)

# Speaker 1 plot
s1_poster <- ggplot(df_s1_summary, aes(x = fricative, y = mean_cog, fill = emphasis)) +
  geom_bar(stat = "identity", position = pd, width = 0.7) +
  geom_errorbar(aes(ymin = mean_cog - SE, ymax = mean_cog + SE),
                position = pd, width = 0.4, linewidth = 1.5) +
  geom_text(aes(label = N, y = mean_cog + SE + 1300),
            position = pd, size = 13, fontface = "bold") +
  labs(x = NULL, y = NULL, title = "Speaker 1") +
  theme_classic(base_size = 34) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, y_max_s1)) +
  scale_fill_manual(values = c("plain" = "blue", "emphatic" = "red")) + 
  theme(
    axis.text.x = element_text(size = 55, face = "bold"),
    axis.text.y = element_text(size = 45, face = "bold"),
    plot.title = element_text(size = 55, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 38, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Speaker 2 plot
s2_poster <- ggplot(df_s2_summary, aes(x = fricative, y = mean_cog, fill = emphasis)) +
  geom_bar(stat = "identity", position = pd, width = 0.7) +
  geom_errorbar(aes(ymin = mean_cog - SE, ymax = mean_cog + SE),
                position = pd, width = 0.4, linewidth = 1.5) +
  geom_text(aes(label = N, y = mean_cog + SE + 1300),
            position = pd, size = 13, fontface = "bold") +
  labs(x = NULL, y = NULL, title = "Speaker 2") +
  theme_classic(base_size = 34) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, y_max_s1)) +
  scale_fill_manual(values = c("plain" = "blue", "emphatic" = "red")) + 
  theme(
    axis.text.x = element_text(size = 55, face = "bold"),
    axis.text.y = element_text(size = 45, face = "bold"),
    plot.title = element_text(size = 55, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 38, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Combine vertically and collect legend
fricative_plot_poster <- s1_poster / s2_poster +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 45, face = "bold"),
    legend.title = element_blank()
  )

# Add shared y-axis label
fricative_plot_poster <- wrap_elements(fricative_plot_poster) +
  labs(tag = "Mean COG (Hz)") + 
  theme(
    plot.tag = element_text(angle = 90, size = 55, face = "bold", vjust = 0.5, hjust = 0.5),
    plot.tag.position = "left"
  )


ggsave(filename = "/Users/noahkhaloo/Desktop/Urmi_fieldwork/figures/fricatives_poster.png",
       plot = fricative_plot_poster,
       width = 20, height = 10, dpi = 300)



