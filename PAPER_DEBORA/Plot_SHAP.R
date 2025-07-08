sv_importance(shp, kind = "beeswarm", max_display = 20, show_numbers = TRUE, number_size = 3.2) +
  theme_bw() + # Isso define um fundo branco e um tema minimalista
  theme(
    axis.text = element_text(size = 21),
    legend.text = element_text(size = 18),
    aspect.ratio = 0.75,
    panel.background = element_rect(fill = "white"), # Garante um fundo branco mesmo com outros temas
    panel.border = element_blank() # Remove a borda do painel
  ) +
  scale_x_continuous(expand = c(0.05, 0), breaks = scales::extended_breaks(n = 8))


shap_matrix_from_shp <- get_shap_values(shp)

# Now, apply the calculations to this extracted numeric matrix
# No need for extra NULL checks or conversions as get_shap_values should return numeric
mean_abs_shap <- apply(abs(shap_matrix_from_shp), 2, mean)
sorted_importance <- sort(mean_abs_shap, decreasing = TRUE)
top_20_features <- head(sorted_importance, 20)

top_20_df <- data.frame(
  Feature = names(top_20_features),
  Mean_Absolute_SHAP = top_20_features
)

print(top_20_df)