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

bs_obj = readRDS(here::here("data", "bootstrap_obj.rds"))

ifold = get_fold()
if (!is.na(ifold)) {
  df = bs_obj[[ifold]]
  df = df %>%
    mutate(mortstat = factor(mortstat))
}

rm(bs_obj)

if(!dir.exists(here::here("results", "bootstrap_50_auc"))){
  dir.create(here::here("results", "bootstrap_50_auc"))
}


if (!file.exists(here::here(
  "results",
  "bootstrap_50_auc",
  paste0("fold_", ifold, ".rds")
)) | force) {

  auc_metric = metric_set(roc_auc)
  log_spec = logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification")


  fit_model = function(var, folds, spec, metrics, mort_df) {
    require(tidyverse)
    require(tidymodels)

    wflow = workflow() %>%
      add_model(spec) %>%
      add_variables(outcomes = mortstat, predictors = all_of(var)) # Assuming mort_binary is the binary outcome variable

    # fit model on folds
    res = fit_resamples(
      wflow,
      resamples = folds,
      metrics = metrics, # Set AUC as part of the metrics
      control = control_resamples(save_pred = TRUE)
    )


    preds = collect_predictions(res)

    # Group by the 'id' column (which tracks repeat and fold information) and calculate AUC per repeat
    auc_by_repeat = preds %>%
      group_by(id, id2) %>%  # 'id' includes both repeat and fold information
      roc_auc(truth = mortstat, event_level = "second", .pred_1)  %>%
      group_by(id) %>%
      summarize(auc = mean(.estimate))

    # Return AUC values as tibble
    auc_by_repeat %>%
      mutate(variable = var)
  }


  demo_vars = c(
    "age_in_years_at_screening",
    "gender",
    "cat_education",
    "cancer",
    "cat_alcohol",
    "cat_smoke",
    "bin_mobilityproblem",
    "general_health_condition"
  )
  pa_vars =
    df %>%
    select(total_AC, total_PAXMTSM, total_adeptsteps, total_scrfsteps,
           total_scsslsteps, total_oaksteps, total_vsrevsteps) %>%
    colnames()

  vars = c(demo_vars, pa_vars)



  set.seed(4575)
  folds = vfold_cv(df, v = 10, repeats = 50)
  fname = paste0("fold_", ifold, ".rds")

  plan(future::multisession, workers = n_cores)

  results =
    furrr::future_map_dfr(
      .x = vars,
      .f = fit_model,
      spec = log_spec,
      metrics = auc_metric,
      folds = folds,
      mort_df = df,
      .options = furrr_options(seed = TRUE, globals = TRUE)
    )

  saveRDS(results, here::here("results", "bootstrap_50_auc", fname))

}

plan(sequential)
