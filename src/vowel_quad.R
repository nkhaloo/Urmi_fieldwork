library(tidyverse)

# Create empty quadrilateral
x <- c(100, 1200, 2100, 100)
y <- c(1800, 1800, 200, 200)
quad <- data.frame(x, y)

quad_plot <- ggplot(quad) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_polygon(aes(x, y), colour = "black", fill = NA)

# Vowel data
f2 <- c(2100, 1900, 1630, 1650, 1450, 850, -30, -30, 100, 120)
f1 <- c(350, 350, 700, 1200, 1600, 1000, 350, 700, 1700, 350)
vowel <- c("/i/", "/y/", "/ø/", "/ɛ/", "/æ/", "/ə/", "[u]", "[o]", "[ɑ]", "[ɯ]")

data <- data.frame(f2, f1, vowel)

# Convert to numeric
data$f2 <- as.numeric(as.character(data$f2))
data$f1 <- as.numeric(as.character(data$f1))

# Add a column to distinguish underlying vs. surface vowels
data$type <- ifelse(grepl("^/", data$vowel), "underlying", "surface")

# Final plot with color
quad3 <- quad_plot +
  geom_text(data = data, aes(x = f2, y = f1, label = vowel, color = type),
            nudge_x = -60, vjust = -0.7, size = 5) +
  scale_color_manual(values = c("underlying" = "blue", "surface" = "red")) +
  theme_void() +
  theme(panel.border = element_blank(), legend.position = "none")

# Save the plot
ggsave(filename = "underlying vowels.png",
       plot = quad3,
       path = "/Users/noahkhaloo/Desktop/Urmi_fieldwork/figures",
       width = 6, height = 5, dpi = 300,
       bg = "white")  # optional: no grey background


# Create empty quadrilateral
x <- c(100, 1200, 2100, 100)
y <- c(1800, 1800, 200, 200)
quad <- data.frame(x, y)

quad_plot <- ggplot(quad) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_polygon(aes(x, y), colour = "black", fill = NA)

# Vowel data
f2 <- c(2100, 1900, 1630, 1650, 1450, 850, -30, -30, 100, 800, 850)
f1 <- c(350, 350, 700, 1200, 1600, 1000, 350, 700, 1700, 1600, 1200 )
vowel <- c("/i/", "/y/", "/ø/", "/ɛ/", "/æ/", "/ə/", "/u/", "/o/", "/ɑ/", "/ɐ/", "/ɜ/")

data_2 <- data.frame(f2, f1, vowel)

# Color blue only for these vowels
blue_vowels <- c("/ø/", "/y/", "/æ/", "/ɛ/")
data_2$type <- ifelse(data_2$vowel %in% blue_vowels, "underlying", "surface")

# Final plot
quad4 <- quad_plot +
  geom_text(data = data_2, aes(x = f2, y = f1, label = vowel, color = type),
            nudge_x = -60, vjust = -0.7, size = 5) +
  scale_color_manual(values = c("underlying" = "blue", "surface" = "red")) +
  theme_void() +
  theme(panel.border = element_blank(), legend.position = "none") 

quad4

# Save the plot
ggsave(filename = "new_vowels.png",
       plot = quad4,
       path = "/Users/noahkhaloo/Desktop/Urmi_fieldwork/figures",
       width = 6, height = 5, dpi = 300,
       bg = "white")  # optional: no grey background
