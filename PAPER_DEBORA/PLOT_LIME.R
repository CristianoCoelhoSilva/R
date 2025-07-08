lime_importance <- explanation %>%
  group_by(feature) %>%
  summarise(
    mean_abs = mean(abs(feature_weight)),
    freq = n(),
    rms = sqrt(mean(feature_weight^2)),
    sd = sd(feature_weight)
  ) %>%
  arrange(desc(mean_abs))

ggplot(lime_importance, aes(x = reorder(feature, mean_abs), y = mean_abs)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "",
    y = "LIME Value",
    title = ""
  ) +
  theme_minimal(base_size = 12) + # Apply the base theme
  theme( # Then, customize specific elements
    axis.text = element_text(size = 20),
    panel.background = element_rect(fill = "white", color = NA), # No border for panel background
    panel.border = element_blank() # Explicitly remove panel border
  )

print(lime_importance$feature)
print(lime_importance$mean_abs)
