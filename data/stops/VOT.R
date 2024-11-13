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
means %>%
  filter(segment %in% c("b", "B", "d", "D", "g", "G")) %>%
  mutate(segment = tolower(segment)) %>%
  ggplot(aes(x = segment, y = mean_vot, color = emphasis, fill = emphasis)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(x = segment, ymin=mean_vot-se, ymax=mean_vot+se), stat = "identity",
                position = "dodge") +
  ylab("Mean VOT (s)") + 
  ggtitle("VOT(s) for Voiced Stops")

#plot VOT for voiceless 
means %>%
  filter(segment %in% c("t", "T", "q", "Q", "k", "K", "p", "P")) %>%
  mutate(segment = tolower(segment), 
         segment = factor(segment, levels = c("p", "t", "k", "q")) ) %>%
  ggplot(aes(x = segment, y = mean_vot, color = emphasis, fill = emphasis)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(x = segment, ymin=mean_vot-se, ymax=mean_vot+se), stat = "identity",
                position = "dodge") +
  ylab("Mean VOT (s)") + 
  ggtitle("VOT(s) for Voiceless Stops")





