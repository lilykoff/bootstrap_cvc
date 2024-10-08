# run simulation
library(tidyverse)
library(tidymodels)
library(simstudy)
source(here::here("code/utils.R"))

if(!dir.exists(here::here("results", "auc_sim"))){
  dir.create(here::here("results", "auc_sim"))
}

# for now do normal RV
data_gen_tibble =
  tidyr::expand_grid(
    n = c(500, 1000, 2500, 5000, 10000),
    prev = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
    auc = c(0.6, 0.7, 0.8, 0.9),
    coefs = c(0.15)
  )

# > nrow(data_gen_tibble)
# [1] 120

ifold = get_fold()
temp_settings = data_gen_tibble %>%
  slice(ifold)

# coefs1 = c(0.15)

# normal rv with meanj 0 variance 1
d1 = defData(varname = "x1", formula = 0, variance = 1)


# d1 <- defData(d1, varname = "x2", formula = 0, variance = 1)
# d1 <- defData(d1, varname = "b1", formula = 0.3, dist = "binary")
# d1 <- defData(d1, varname = "b2", formula = 0.7, dist = "binary")

C1 = logisticCoefs(d1, temp_settings$coefs, popPrev = temp_settings$prev, auc = temp_settings$auc,
                   sampleSize = 1000000)

d1a = defData(d1, varname = "y",
              formula = "t(..C1) %*% c(1, x1)",
              dist = "binary", link = "logit"
      )

# test
# dd = genData(n = temp_settings$n, d1a)
dd = genData(n = 1000000, d1a)


actual_prev = dd[, mean(y)]
fit = rms::lrm(y ~ x1, data = dd)
actual_auc = fit$stats["C"]


auc_metric = metric_set(roc_auc)
log_spec = logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")
dd_dat =
  dd %>%
  mutate(y = factor(y))

wflow = workflow() %>%
  add_model(log_spec) %>%
  add_variables(outcomes = y, predictors = x1)

set.seed(4575)
folds = vfold_cv(dd_dat, v = 10, repeats = 10)

# fit model on folds
res = fit_resamples(
  wflow,
  resamples = folds,
  metrics = auc_metric,
  control = control_resamples(save_pred = TRUE)
)


preds = collect_predictions(res)

# Group by the 'id' column (which tracks repeat and fold information) and calculate AUC per repeat
auc_by_repeat = preds %>%
  group_by(id, id2) %>%  # 'id' includes both repeat and fold information
  roc_auc(truth = y, event_level = "second", .pred_1)  %>%
  group_by(id) %>%
  summarize(auc = mean(.estimate))

# Return AUC values as tibble
auc_by_repeat %>%
  mutate(
    actual_prev = actual_prev,
    actual_auc = actual_auc,
    n = temp_settings$n,
    target_prev = temp_settings$prev,
    target_auc = temp_settings$auc,
    target_coefs = temp_settings$coefs
  ) %>%
  readr::write_rds(here::here("results", "auc_sim", paste0("fold_", ifold, ".rds")))
