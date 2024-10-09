library(tidyverse)
library(tidymodels)
library(furrr)
library(future)
library(censored)
n_cores = parallelly::availableCores() - 1
print(n_cores)
source(here::here("code/utils.R"))

fold = NULL
rm(list = c("fold"))

force = FALSE

df_list = readRDS(here::here("data", "auc_known_list.rds"))

ifold = get_fold()

df = df_list[[ifold]]

df %>%
  mutate(outcome = factor(outcome))

rm(df_list)
n_vec = c(1000, 5000, 10000, 100000)
set.seed(1233)
samples = map(.x = n_vec,
              .f = function(n){
                df %>%
                  sample_n(n, replace = FALSE)
              })

bs_obj =
  map(
    .x = samples,
    n_boot = 1000,
    .f = function(n_boot, df) {
      replicate(n_boot,
                df %>%
                  slice_sample(
                    prop = 1,
                    replace = TRUE),
                    simplify = FALSE
                  )
                }
    )


if(!dir.exists(here::here("results", "bootstrap_cohend_auc"))){
  dir.create(here::here("results", "bootstrap_cohend_auc"))
}


if (!file.exists(here::here(
  "results",
  "bootstrap_cohend_auc",
  paste0("fold_", ifold, ".rds")
)) | force) {

  auc_metric = metric_set(roc_auc)
  log_spec = logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification")

  wflow = workflow() %>%
    add_model(log_spec) %>%
    add_variables(outcomes = outcome, predictors = predictor)

  get_auc = function(n_repeats = 100, n_folds = 10, sample_df){
    set.seed(4575)
    sample_auc = try({AUC::auc(AUC::roc(sample_df$predictor, sample_df$outcome))})
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
        sample_auc = if_else(is.numeric(sample_auc), sample_auc, NA_real_),
        sample_size = nrow(sample_df)
      )
  }
  plan(multisession, workers = n_cores)
  all_res =
    future_map_dfr(
      .x = bs_obj[[1]],
      .f = get_auc,
      n_repeats = 100,
      n_folds = 10,
      .options = furrr_options(seed = TRUE, globals = TRUE),
      .id = "boot"
    )
  readr::write_rds(all_res, here::here("results", "bootstrap_cohend_auc_n1_", paste0("fold_", ifold, ".rds")))
  rm(all_res)

  all_res =
    future_map_dfr(
      .x = bs_obj[[2]],
      .f = get_auc,
      n_repeats = 100,
      n_folds = 10,
      .options = furrr_options(seed = TRUE, globals = TRUE),
      .id = "boot"
    )
  readr::write_rds(all_res, here::here("results", "bootstrap_cohend_auc_n2_", paste0("fold_", ifold, ".rds")))
  rm(all_res)

  all_res_3 =
    future_map_dfr(
      .x = bs_obj[[3]],
      .f = get_auc,
      n_repeats = 100,
      n_folds = 10,
      .options = furrr_options(seed = TRUE, globals = TRUE),
      .id = "boot"
    )
  readr::write_rds(all_res, here::here("results", "bootstrap_cohend_auc_n3_", paste0("fold_", ifold, ".rds")))
  rm(all_res)

  all_res_1 =
    future_map_dfr(
      .x = bs_obj[[4]],
      .f = get_auc,
      n_repeats = 100,
      n_folds = 10,
      .options = furrr_options(seed = TRUE, globals = TRUE),
      .id = "boot"
    )
  readr::write_rds(all_res, here::here("results", "bootstrap_cohend_auc_n4_", paste0("fold_", ifold, ".rds")))
  rm(all_res)

  plan(sequential)
}

