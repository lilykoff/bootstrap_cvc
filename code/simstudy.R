# install.packages("simstudy")
library(simstudy)
# https://www.rdatagen.net/post/2023-06-20-finding-coefficients-for-logistic-models-that-generate-data-with-desired-characteristics/

coefs1 <- c(0.15, 0.25, 0.10, 0.30)

d1 = defData(varname = "x1", formula = 0, variance = 1)
# d1 <- defData(d1, varname = "x2", formula = 0, variance = 1)
# d1 <- defData(d1, varname = "b1", formula = 0.3, dist = "binary")
# d1 <- defData(d1, varname = "b2", formula = 0.7, dist = "binary")

C1 = logisticCoefs(d1, coefs1, popPrev = 0.40, auc = 0.85)
C1
d1a = defData(d1, varname = "y",
               formula = "t(..C1) %*% c(1, x1)",
               dist = "binary", link = "logit"
)

dd <- genData(3000, d1a)

dd[, mean(y)]
fit <- rms::lrm(y ~ x1, data = dd)
fit$stats["C"]


auc_metric = metric_set(roc_auc)
log_spec = logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")


require(tidyverse)
require(tidymodels)
dd_dat =
  dd %>%
  mutate(y = factor(y))
wflow = workflow() %>%
  add_model(log_spec) %>%
  add_variables(outcomes = y, predictors = x1) # Assuming mort_binary is the binary outcome variable

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
auc_by_repeat
