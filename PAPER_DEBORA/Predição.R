library(data.table)
library(tidyverse)
library(tidymodels)
library(parallel)
library(doParallel)
library(finetune)
library(fastshap)
library(shapviz)
library(future)
library(tune)
library(lime)
library(ggplot2)
library(dplyr)
set.seed(123)

Sys.setlocale("LC_TIME", "English")

papers = fread("base_machine_learning.csv")

papers <- papers %>%
  mutate(across(where(is.logical), as.integer))

to_machine_learning =
  papers

training1 <- papers %>% drop_na()

current_target_variable = "FL"

recipe <- recipe(FL ~ ., data = training1) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_nzv(all_predictors(), freq_cut = 20) %>%
  step_corr(all_numeric_predictors(), threshold = 0.8) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_mutate(across(where(is.logical), ~ as.integer(.))) %>%
  step_zv(all_predictors())

race_results = read_rds("race_results_20240416.rds") #results do Horse Results

selected_model_names = c(
  "recipe_Random Forest"
)

for(best_model_name in selected_model_names) {
  
  dir.create(file.path(".", "results", best_model_name), showWarnings = FALSE)
  
  futile.logger::flog.info(sprintf("Shapley values for %s", best_model_name))
  
  best_model_results =
    race_results %>%
    extract_workflow_set_result(id = best_model_name) %>%
    select_best(metric = "rmse")
  
  best_model_results
  
  best_model_workflow =
    race_results %>%
    extract_workflow(best_model_name) %>%
    finalize_workflow(
      best_model_results
    ) %>%
    fit(training1)
  
  best_model =
    best_model_workflow %>%
    extract_fit_parsnip()
  
  
  # Shapley value -----------------------------------------------------------
  
  prepared_recipe =
    recipe %>%
    prep()
  
  X =
    prepared_recipe %>%
    bake(new_data = to_machine_learning) %>%
    select(-all_of(current_target_variable)) %>%
    as.matrix()
  
  y = to_machine_learning[[current_target_variable]]
  y_pred = best_model %>% predict(X)
  
  y_probs = best_model %>% predict(new_data = X) %>% select(.pred)
  
  
  library(future)
  library(fastshap)
  
  # Configura paralelismo seguro
  plan(multisession, workers = parallel::detectCores() - 2)
  
  # Função de predição robusta
  pfun <- function(object, newdata) {
    library(ranger)  # carrega dentro do worker
    
    if (inherits(object, "ranger")) {
      predict(object, data = newdata)$predictions
    } else {
      predict(object, newdata = newdata)
    }
  }
  
  # Calcula valores de Shapley
  shap <- fastshap::explain(
    object = best_model$fit,  # modelo ranger ajustado
    X = X,                    # data.frame com as features
    nsim = 100,               # número de simulações
    adjust = TRUE,
    pred_wrapper = pfun,
    parallel = TRUE
  )

  pfun(best_model$fit, head(X))
  
  #stopCluster(cl)
  
  shp <- shapviz::shapviz(shap,
                          X_pred = predict(best_model$fit, data = X)$predictions,
                          X = X,
                          interactions = TRUE)
  
  
  
  sv_importance(shp, kind = "beeswarm", max_display = 20, show_numbers = T, number_size = 3.2)  +
    theme(
      axis.text = element_text(size = 21),
      legend.text = element_text(size = 18),
      # aspect.ratio = 0.75
    ) +
    scale_x_continuous(expand = c(0.05, 0)) +
    scale_x_continuous(breaks = scales::extended_breaks(n = 8))
  
  ggsave(
    path = sprintf("./results/%s", best_model_name),
    filename = "shap_values_beeswarm_dependence_plot.jpg",
    width = 25,
    height = 15
  )
  
  explainer <- lime::lime(
    x = as.data.frame(X),        # converte a matriz para data.frame
    model = best_model$fit,
    bin_continuous = TRUE
  )
  
  # 2. Aplicar o LIME para um subconjunto de observações
  # Sugiro escolher algumas observações para facilitar visualização local

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
  
  ggsave(
    path = sprintf("./results/%s", best_model_name),
    filename = "lime_plot.jpg",
    width = 25,
    height = 15
  )
  
}


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

