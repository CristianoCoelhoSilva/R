# Install necessary packages if you haven't already
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyr") # For pivot_wider

library(ggplot2)
library(dplyr)
library(tidyr) # Used for pivot_wider if needed, though not strictly necessary for this approach

# --- 1. Create a sample data frame similar to yours ---
# Replace this with your actual data loading
df <- validacao

# Ensure 'Ano' is treated as an integer or factor for better plotting
df$Ano <- as.numeric(as.character(df$Ano))
df$DC_NOME <- as.factor(df$DC_NOME) # Treat city names as factors

# --- 2. Prepare the data for plotting ---

# Calculate total 'n' per city and year for heatmap color
df_total_n <- df %>%
  group_by(DC_NOME, Ano) %>%
  summarise(total_n = sum(n), .groups = 'drop')

# Get 'n' for "Extrema" intensity heatwaves for dot size
df_extreme_n <- df %>%
  filter(Intensidade_HW == "Extrema") %>%
  group_by(DC_NOME, Ano) %>%
  summarise(extreme_n = sum(n), .groups = 'drop')

# Join the two dataframes to have both total_n and extreme_n in one
plot_data <- left_join(df_total_n, df_extreme_n, by = c("DC_NOME", "Ano")) %>%
  # Replace NA extreme_n with 0 where no "Extrema" heatwaves occurred
  mutate(extreme_n = replace_na(extreme_n, 0))

# --- 3. Create the heatmap ---

ggplot(plot_data, aes(x = Ano, y = DC_NOME)) +
  geom_tile(aes(fill = total_n), color = "white", linewidth = 0.5) +
  geom_point(aes(size = extreme_n), color = "purple", alpha = 0.7) +
  # Set the color scale for the heatmap (shades of light red to dark red with a midpoint)
  scale_fill_gradient2(
 
    low = "#FFE0E0",       # Um vermelho muito claro, quase branco-rosado
    mid = "indianred",      # Um vermelho médio para o ponto central
    high = "darkred",       # ALTERADO AQUI: Um vermelho escuro e saturado (darkred é #8B0000)
    
    midpoint = mean(plot_data$total_n, na.rm = TRUE), # O ponto médio dos seus dados
    name = "Total Heatwave Counts"
  ) +
  # Set the size scale for the dots
  scale_size_continuous(range = c(0.5, 5),
                        breaks = c(1, 2, 3, 4, 5, 6, 7, 8),
                        name = "Number of severe and extreme HWs per year") +
  # Set the x-axis breaks to match the example (every few years, or specific years)
  scale_x_continuous(breaks = seq(min(plot_data$Ano), max(plot_data$Ano), by = 3),
                     expand = c(0, 0)) +
  # Labels and titles
  labs(title = "Number of Heatwaves per Year by Metropolitan Region",
       x = "Year",
       y = "Metropolitan Region") +
  # Customize theme to resemble the example
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(angle = 90, vjust = 0.5),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )