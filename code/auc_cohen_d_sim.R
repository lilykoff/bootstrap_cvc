library(tidyverse)
library(future)
library(tidymodels)
library(furrr)
n_cores = parallelly::availableCores() - 1
# print(n_cores)
source(here::here("code/utils.R"))

if(!dir.exists(here::here("results", "cohen_d_sim"))){
  dir.create(here::here("results", "cohen_d_sim"))
}
df_list = readRDS(here::here("data", "auc_known_list.rds"))
ifold = get_fold()

df = df_list[[ifold]]
df =
  df %>%
  mutate(outcome = factor(outcome))

n_vec = c(1000, 5000, 10000, 100000)

samples = map(.x = n_vec,
              .f = function(n){
                df %>%
                  sample_n(n, replace = FALSE)
              })

auc_metric = metric_set(roc_auc)
log_spec = logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

wflow = workflow() %>%
  add_model(log_spec) %>%
  add_variables(outcomes = outcome, predictors = predictor)

get_auc = function(n_repeats = 100, n_folds = 10, sample_df){
  set.seed(4575)
  sample_auc = AUC::auc(roc(sample_df$predictor, sample_df$outcome))
  folds = vfold_cv(sample_df, v = n_folds, repeats = n_repeats)
  res = fit_resamples(
    wflow,
    resamples = folds,
    metrics = auc_metric,
    control = control_resamples(save_pred = TRUE)
  )
  preds = collect_predictions(res)
  auc_by_repeat = preds %>%
    group_by(id, id2) %>%  # 'id' includes both repeat and fold information
    roc_auc(truth = outcome, event_level = "second", .pred_1)  %>%
    group_by(id) %>%
    summarize(auc = mean(.estimate))

  # Return AUC values as tibble
  auc_by_repeat %>%
    mutate(
      sample_auc = sample_auc,
      sample_size = nrow(sample_df)
    )
}
plan(multisession, workers = n_cores)
all_res =
  furrr::future_map_dfr(
    .x = samples,
    .f = get_auc,
    n_repeats = 100,
    n_folds = 10,
    .options = furrr_options(seed = TRUE, globals = TRUE)
  )

readr::write_rds(all_res, here::here("results", "cohen_d_sim", paste0("fold_", ifold, ".rds")))

plan(sequential)
