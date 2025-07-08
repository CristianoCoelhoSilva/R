library(lime)
library(ggplot2)
library(dplyr)

# 1. Criar o explicador (explainer)
explainer <- lime::lime(
  x = as.data.frame(X),        # converte a matriz para data.frame
  model = best_model$fit,
  bin_continuous = TRUE
)

# 2. Aplicar o LIME para um subconjunto de observações
# Sugiro escolher algumas observações para facilitar visualização local
set.seed(123)
obs_to_explain <- as.data.frame(X) # por exemplo, 10 observações aleatórias

explanation <- lime::explain(
  x = obs_to_explain,
  explainer = explainer,
  n_features = 20,        # Quantas variáveis mostrar por explicação
  n_permutations = 5000,  # Quanto maior, mais preciso
  feature_select = "auto" # Pode usar: "auto", "forward_selection", etc.
)

lime_importance <- explanation %>%
  group_by(feature) %>%
  summarise(mean_weight = mean(abs(feature_weight))) %>%
  arrange(desc(mean_weight)) %>%
  slice_max(mean_weight, n = 20)

ggplot(lime_importance, aes(x = reorder(feature, mean_weight), y = mean_weight)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Variable",
    y = "Average Importance (|weight|)",
    title = ""
  ) +
  theme_minimal(base_size = 16)
