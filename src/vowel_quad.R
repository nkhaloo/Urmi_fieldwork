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


# poster plot 
poster <- quad_plot +
  geom_text(data = data, aes(x = f2, y = f1, label = vowel, color = type),
            nudge_x = -60, vjust = -0.7, size = 5) +
  scale_color_manual(values = c("underlying" = "blue", "surface" = "red")) +
  theme_void() +
  theme(panel.border = element_blank(), legend.position = "none") + 
  coord_fixed(xlim = c(2200, -100))


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

# Additional vowel plot matching screenshot layout

# Quadrilateral data (same as previous)
x_quad <- c(100, 1200, 2100, 100)
y_quad <- c(1800, 1800, 200, 200)
quad_df <- data.frame(x_quad, y_quad)

quad_base <- ggplot(quad_df) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_polygon(aes(x = x_quad, y = y_quad), color = "black", fill = NA)

# New vowel data matching your screenshot
f2_new <- c(1750, 1630, 1450, 1650, 850, 2080, 100, -30, -30, 320)
f1_new <- c(450, 760, 1600, 1200, 1000, 450, 1700, 550, 700, 650)
vowel_new <- c("/y/", "/ø/", "/æ/", "/ɛ/", "/ə/", "/i/", "[ɑ]", "[u]", "[o]", "[ɯ]")

new_data <- data.frame(f2_new, f1_new, vowel_new)
new_data$type <- ifelse(grepl("^/", new_data$vowel_new), "underlying", "surface")

# Extended quadrilateral coordinates (making plot physically wider)
x_quad_extended <- c(-1500, 1200, 3500, -1500)  # Significantly extended left and right
y_quad <- c(1800, 1800, 200, 200)
quad_df_extended <- data.frame(x_quad_extended, y_quad)

quad_base_extended <- ggplot(quad_df_extended) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_polygon(aes(x = x_quad_extended, y = y_quad), color = "black", fill = NA)

# New vowel data remains the same
f2_new <- c(2900, 2950, 1650, 2270, 350, 3500, -1800, -1800, -1800, -1400)
f1_new <- c(500, 900, 1800, 1400, 1000, 500, 1800, 550, 800, 550)
vowel_new <- c("/y/", "/ø/", "/æ/", "/ɛ/", "/ə/", "/i/", "[ɑ]", "[u]", "[o]", "[ɯ]")
new_data <- data.frame(f2_new, f1_new, vowel_new)
new_data$type <- ifelse(grepl("^/", new_data$vowel_new), "underlying", "surface")

# Create the vowel plot with extended quadrilateral and adjusted size
quad5_extended <- quad_base_extended +
  geom_text(data = new_data, aes(x = f2_new, y = f1_new, label = vowel_new, color = type),
            nudge_x = -120, vjust = -0.7, size = 20) +  # Moderate text size
  scale_color_manual(values = c("underlying" = "blue", "surface" = "red")) +
  coord_fixed(xlim = c(3800, -1800), ylim = c(1900, 100), expand = FALSE) +
  theme_void() +
  theme(panel.border = element_blank(), legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "cm"))

quad5_extended

quad5_extended <- quad5_extended + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

ggsave(
  filename = "poster.png",
  plot = quad5_extended,
  path = "/Users/noahkhaloo/Desktop/Urmi_fieldwork/figures",
  width = 23,        # Increased width for a wider x-axis
  height = 6,        # Adjusted height to maintain aspect ratio
  units = "in",      # Units in inches
  dpi = 300,        
  bg = "white"       
)
