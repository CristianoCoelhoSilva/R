#rm(list = ls())
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
#library(ggthemes)
set.seed(123)

Sys.setlocale("LC_TIME", "English")
#ggthemr::ggthemr('pale', layout = 'scientific', text_size = 24, spacing = 2)

papers =
  fread("base_machine_learning.csv")

papers <- papers %>%
  mutate(across(where(is.logical), as.integer))

to_machine_learning =
  papers

training1 <- papers %>% drop_na()

folds <- vfold_cv(training1, v = 5)

current_target_variable = "FL"

recipe <- recipe(FL ~ ., data = training1) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_nzv(all_predictors(), freq_cut = 20) %>%
  step_corr(all_numeric_predictors(), threshold = 0.8) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_mutate(across(where(is.logical), ~ as.integer(.))) %>%
  step_zv(all_predictors())

processed_recipe_data =
  recipe %>%
  prep() %>% juice()
dim(processed_recipe_data)

str_subset(colnames(processed_recipe_data), "clean")

lr_spec =
  linear_reg(penalty = 0, mixture = 0) %>%
  set_engine("glmnet")

lr_with_regularization_spec =
  linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

svm_r_spec <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

knn_spec <-
  nearest_neighbor(neighbors = tune(), dist_power = tune(), weight_func = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")

nnet_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine("nnet", MaxNWts = 2600) %>%
  set_mode("regression")

nnet_param <-
  nnet_spec %>%
  extract_parameter_set_dials() %>%
  update(hidden_units = hidden_units(c(1, 27)))

rf_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>%
  set_engine("ranger",  importance = "impurity") %>%
  set_mode("regression")

xgb_spec <-
  boost_tree(learn_rate = tune(), trees = tune(), tree_depth = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

lightgbm_spec <-
  boost_tree(engine = "lightgbm", learn_rate = tune(), trees = tune(),
             tree_depth = tune()) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

horserace_workflow <-
  workflow_set(
    preproc = list(
      recipe
    ),
    models = list(
      "Linear Regression" = lr_spec,
      "Elastic Net" = lr_with_regularization_spec,
      "SVM (RBF Kernel)" = svm_r_spec,
      "k-nearest neighbor" = knn_spec,
      "MLP" = nnet_spec,
      "Random Forest" = rf_spec,
      "XGBoost" = xgb_spec
    ),
    cross = T
  ) 

horserace_workflow

cores <- parallel::detectCores(logical = FALSE) - 3
cores

cl <- makePSOCKcluster(cores)

registerDoParallel(cl)

GRID_SIZE_HYPERPARAMETER_TUNING <- 3

race_ctrl <- control_race(
  save_pred = TRUE,
  allow_par = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE,
  verbose = TRUE,
  verbose_elim = TRUE
)

# Define o plano de paralelismo com múltiplos workers
plan(multisession, workers = parallel::detectCores(logical = FALSE) - 2)

# Agora rode normalmente:
race_results <- horserace_workflow %>%
  workflow_map(
    "tune_race_anova",
    seed = 3,
    resamples = folds,
    grid = GRID_SIZE_HYPERPARAMETER_TUNING,
    metrics = metric_set(rmse, rsq),
    control = race_ctrl
  )

stopCluster(cl = cl)

saveRDS(
  object = race_results,
  file = "race_results_20240416.rds"
)


race_results %>%
  mutate(
    wflow_id = ifelse(wflow_id == "recipe_Linear Regression (regularized)", "recipe_Elastic Net", wflow_id)
  ) %>%
  collect_metrics() %>%
  group_by(wflow_id, .metric) %>%
  filter(mean == min(mean)) %>%
  ungroup() %>%
  mutate(model = str_replace(wflow_id, "recipe_", "")) %>%
  filter(.metric == "rmse") %>%
  mutate(
    .metric = case_when(
      .metric == "accuracy" ~ "Accuracy",
      .metric == "roc_auc" ~ "ROC AUC",
      TRUE ~ .metric
    )
  ) %>%
  mutate(model = tidytext::reorder_within(model, mean, .metric)) %>%
  ggplot(aes(x = model, y = mean)) +
  geom_point(size = 5) +
  tidytext::scale_x_reordered() +
  scale_y_continuous(breaks = scales::extended_breaks(n = 8), expand = c(0.1, 0.1)) +
  coord_flip() +
  geom_errorbar(
    aes(ymin = mean - 1.96 * std_err,
        ymax = mean + 1.96 * std_err),
    size = 1.5
  ) +
  labs(
    y = "RMSE",
    x = "Model"
  ) +
  theme(
    aspect.ratio = 0.8,
    panel.background = element_blank(),        # fundo do painel (área do gráfico) branco/vazio
    plot.background = element_blank(),         # fundo do plot inteiro
    panel.grid.major = element_line(color = "grey80"),  # grade maior cinza clara
    panel.grid.minor = element_line(color = "grey90")   # grade menor ainda mais clara (opcional)
  )


ggsave(
  path = "./results",
  filename = sprintf("target(%s)_errorPlot_metrics(validation set).jpg", current_target_variable),
  height = 7,
  width = 12
)