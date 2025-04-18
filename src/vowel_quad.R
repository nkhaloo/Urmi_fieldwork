#create empty quadrilateral
x <- c(200, 1200, 2000, 200)
y <- c(1800, 1800, 200, 200)
quad <- as.data.frame(cbind(x, y))

quad_plot <- ggplot(quad)+scale_x_reverse()+scale_y_reverse()+geom_polygon(aes(x, y), colour="black", fill=NA)
quad_plot

#front vowels (black text)
f2 <- c(2000, 1700, 1630, 1550, 1350, 850)
f1 <- c(350, 350, 700, 1200, 1600, 1000)
vowel <- c("i", "y", "ø", "ɛ", "æ", "ə")
data <- as.data.frame(cbind(f2, f1, vowel))

data$f2 <- as.numeric(as.character(data$f2))
data$f1 <- as.numeric(as.character(data$f1))


quad2 <- quad_plot + theme_classic()
quad2


quad3 <- quad_plot + 
  geom_text(data = data, aes(x = f2, y = f1, label = vowel), nudge_x = -60, vjust = -0.7, size = 5) +
  theme_void() +  
  theme(panel.border = element_blank()) 
