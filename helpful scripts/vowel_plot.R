library(tidyverse)


#vowels 
vowels <- read_csv("/Users/ritalavi/Desktop/vowel_pilot/results.csv") 

#get rid of long diacritic
vowels$Segment <- gsub("ː", "", vowels$Segment)
vowels$Segment <- gsub(":", "", vowels$Segment)

vowel_means <- vowels %>%
  group_by(Segment) %>%
  summarise(mean_F1 = mean(F1), 
            mean_F2 = mean(F2))

#vowel chart
ggplot(vowels, aes(x = F2, y = F1, color = Segment, label = Segment)) + 
  geom_label(data = vowel_means, aes(x = mean_F2, y = mean_F1)) + 
  scale_x_reverse() + scale_y_reverse() + 
  scale_color_discrete(breaks = c("a", "æ", "e", "i", "ʉ", "ø", "ė")) +
  theme_classic() + 
  theme(legend.position = "none") + 
  xlab("Mean F2 (Hz)") + 
  ylab("Mean F1 (Hz)")
