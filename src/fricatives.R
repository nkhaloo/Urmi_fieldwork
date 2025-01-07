library(tidyverse) 
library(ggdist)

#load csv
raw_data <- read.csv("/Users/ritalavi/Desktop/Urmi_fieldwork/s1/fricatives/fricative_results_s1.csv", header = FALSE)

# Use the first row as the actual column names
column_names <- as.character(unlist(raw_data[1, ]))

# Remove any rows that are duplicates of the column names
cleaned_data <- raw_data %>%
  filter(!apply(., 1, function(row) all(row == column_names)))

# Assign proper column names
colnames(cleaned_data) <- column_names

#create vowel column 
df <- cleaned_data %>%
  mutate(vowel = substr(label, 4, 4))


#create emphatic column
df <- df %>%
  mutate(emphasis = case_when(
    grepl("22", label) ~ "emphatic",
    grepl("13$", label) ~ "mixed",
    grepl("33", label) ~ "mixed",
    TRUE ~ "plain"
  ))

#create fricative column 
df <- df %>%
  mutate(fricative = substr(label, 1, 1))

#filter out epenthetic vowels 
df <- df %>%
  filter(vowel != "ē")

#select COG
cog_df <- df %>%
  select(fricative, vowel, emphasis, cog) %>%
  filter(cog>=1000)

#make COG numeric
cog_df$cog <- as.numeric(cog_df$cog)

# Remove rows with non-finite cog values
cog_df <- cog_df %>%
  filter(is.finite(cog))  # Keeps only rows where cog is a finite number

# Optionally, remove groups with fewer than two data points
cog_df <- cog_df %>%
  group_by(fricative, emphasis) %>%
  filter(n() >= 2) %>%
  ungroup()


#filter out "sh" and "je" 
cog_df_filtered <- cog_df %>%
  filter(!fricative %in% c("ʃ", "ʒ"))

####visualizaton###
# Define the plotting function
plot_fricative <- function(fricative, df) {
  # Filter the data for the specified fricative
  filtered_df <- df %>%
    filter(fricative == !!fricative)
  
  # Generate the plot
  plot <- ggplot(filtered_df, aes(x = fricative, y = cog, fill = emphasis)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.5) +
    scale_y_continuous(name = "COG (Hz)",
                       breaks = seq(1000, 10000, by = 2000)) + 
    scale_fill_brewer(palette = "Dark2", name = "Emphasis") +
    scale_x_discrete(name = NULL) +  # Remove x-axis title
    theme_minimal() +
    theme(panel.grid.major.y = element_line(color = "gray80"),
          panel.grid.minor.y = element_line(color = "gray90"),  
          panel.grid.major.x = element_line(color = "gray80"), 
          panel.grid.minor.x = element_line(color = "gray90"),
          legend.position = "top",
          legend.background = element_rect(fill = "white", color = "white"),
          legend.key.size = unit(0.8, "cm"),
          legend.text = element_text(size = 22),
          legend.title = element_text(size = 22),
          axis.text.x = element_text(size = 22),
          axis.text.y = element_text(size =18),
          axis.title.y = element_text(size = 22),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_rect(fill = "white", color = "white"))
  
  # Save the plot as a PNG file
  png_path <- paste0("/Users/ritalavi/Desktop/Urmi_fieldwork/data/figures/", fricative, "_COG.png")
  ggsave(png_path, plot, device = "png", width = 8, height = 6, dpi = 300)
}


plot_fricative("s", cog_df_filtered)
plot_fricative("f", cog_df_filtered)
plot_fricative("x", cog_df_filtered)
plot_fricative("h", cog_df_filtered)



###could be a nice plot if I get more data###
#ridge plot
ggplot(data = df.diamonds,
       mapping = aes(x = price,
                     y = color)) +
  ggridges::geom_density_ridges(scale = 1.5)
