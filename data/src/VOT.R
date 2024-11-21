library(tidyverse)

#load csv
vot <- read_csv("/Users/ritalavi/Desktop/Urmi_fieldwork/data/stops/VOT_results.csv", locale = locale(encoding = "UTF-16"))

#convert VOT to s
vot <- vot%>%
  mutate(VOT = VOT *1000)

#create segment column 
vot <- vot %>%
  mutate(segment = substr(Word, 1, 1))

#create emphasis column 
vot <- vot %>%
  mutate(emphasis = ifelse(segment %in% c("B", "D", "G", "J", "K", "P", "Q" ,"T"), "emphatic", "plain"))

#create means df 
means <- vot %>%
  group_by(segment, emphasis) %>%
  summarise(mean_vot = mean(VOT, na.rm = TRUE), 
            count = n(), 
            sd = sd(VOT), 
            se = sd/sqrt(count))


#plot vot for voiced
plot_1 <- means %>%
  filter(segment %in% c("b", "B", "d", "D", "g", "G")) %>%
  mutate(segment = tolower(segment)) %>%
  ggplot(aes(x = segment, y = mean_vot, color = emphasis, fill = emphasis)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(x = segment, ymin=mean_vot-se, ymax=mean_vot+se), stat = "identity",
                position = "dodge") +
  ylab("Mean VOT (s)") + 
  theme(
    axis.text.x = element_text(size = 20),      # X-axis text size
    axis.text.y = element_text(size = 14),      # Y-axis text size
    axis.title.x = element_text(size = 14),     # X-axis title size
    axis.title.y = element_text(size = 16),     # Y-axis title size
    legend.text = element_text(size = 14),      # Legend text size
    legend.title = element_text(size = 16)      # Legend title size
  )


ggsave(filename = "/Users/ritalavi/Desktop/Urmi_fieldwork/data/figures/voiced_stops.png", 
       plot = plot_1, 
       width = 8, height = 6, dpi = 300)


#plot VOT for voiceless 
# Your plot code
plot_2 <- means %>%
  filter(segment %in% c("t", "T", "q", "Q", "k", "K", "p", "P")) %>%
  mutate(segment = tolower(segment), 
         segment = factor(segment, levels = c("p", "t", "k", "q"))) %>%
  ggplot(aes(x = segment, y = mean_vot, color = emphasis, fill = emphasis)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(x = segment, ymin = mean_vot - se, ymax = mean_vot + se), 
                stat = "identity", position = position_dodge(width = 0.9)) +
  ylab("Mean VOT (s)") +
  theme(
    axis.text.x = element_text(size = 20),      # X-axis text size
    axis.text.y = element_text(size = 14),      # Y-axis text size
    axis.title.x = element_text(size = 14),     # X-axis title size
    axis.title.y = element_text(size = 16),     # Y-axis title size
    legend.text = element_text(size = 14),      # Legend text size
    legend.title = element_text(size = 16)      # Legend title size
  )

# Save the plot
ggsave(filename = "/Users/ritalavi/Desktop/Urmi_fieldwork/data/figures/voiceless_stops.png", 
       plot = plot_2, 
       width = 8, height = 6, dpi = 300)




#####better way to plot VOT#####
vot%>%
  filter(segment %in% c("b", "B", "d", "D", "g", "G")) %>%
  ggplot(data = vot,
         mapping = aes(x = segment,
                       y = VOT,
                       color = emphasis, 
                       fill = emphasis)) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = "pointrange",
               shape = 21,
               size = 1) + 
  # individual data points (jittered horizontally)
  geom_point(alpha = 0.2,
             color = "blue",
             position = position_jitter(width = 0.1,
                                        height = 0),
             size = 2)



