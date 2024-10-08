library(AUC)
# https://stats.stackexchange.com/questions/422926/generate-synthetic-data-given-auc
# https://stats.stackexchange.com/questions/562000/how-to-simulate-a-calibrated-prediction-model-given-prevalence-and-auc
auc <- 0.8

t <- sqrt(log(1/(1-auc)**2))
z <- t-((2.515517 + 0.802853*t + 0.0103328*t**2) /
          (1 + 1.432788*t + 0.189269*t**2 + 0.001308*t**3))
d <- z*sqrt(2)

n <- 10000
x <- c(rnorm(n/2, mean = 0), rnorm(n/2, mean = d))
y <- c(rep(0, n/2), rep(1, n/2))

# more rare event
n <- 10000
x <- c(rnorm(n * (9/10), mean = 0), rnorm(n / 10, mean = d))
y <- c(rep(0, n * (9/10)), rep(1, n/10))

auc(roc(x, as.factor(y)))


data = tibble(var = x,
              outcome = factor(y))

################

auc_metric = metric_set(roc_auc)
log_spec = logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")


  require(tidyverse)
  require(tidymodels)

wflow = workflow() %>%
  add_model(log_spec) %>%
  add_variables(outcomes = outcome, predictors = var) # Assuming mort_binary is the binary outcome variable

set.seed(4575)
folds = vfold_cv(data, v = 10, repeats = 10)

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
  roc_auc(truth = outcome, event_level = "second", .pred_1)  %>%
  group_by(id) %>%
  summarize(auc = mean(.estimate))

# Return AUC values as tibble
auc_by_repeat


library(tidyverse)

auc <- 0.95

t <- sqrt(log(1/(1-auc)**2))
z <- t-((2.515517 + 0.802853*t + 0.0103328*t**2) /
          (1 + 1.432788*t + 0.189269*t**2 + 0.001308*t**3))
d <- z*sqrt(2)

n <- 10000
x <- c(rnorm(n/2, mean = 0), rnorm(n/2, mean = d))
y <- c(rep(0, n/2), rep(1, n/2))

data <- tibble(outcome = y, score = x)

# Applying Platt scaling to predictions / scores
model <- glm(outcome ~ score, family = "binomial", data = data)

df_preds <- bind_cols(
  data,
  # inverse logit of predictions from logistic regression model
  tibble(score_scaled = plogis(predict(model, newdata = data)))
) %>%
  mutate(score_interval = cut(score_scaled, seq(0, 1, 0.1), lables = FALSE))

df_preds %>%
  group_by(score_interval) %>%
  summarize(mean_actual = mean(outcome),
            mean_score = mean(score_scaled)) %>%
  ggplot(aes(mean_score, mean_actual)) +
  geom_line(aes(mean_actual, mean_actual, colour = "calibrated"))+
  geom_line(alpha=0.3, group = 1) +
  geom_point()+
  theme_bw() +
  scale_x_continuous(limits=c(0, 1)) +
  scale_y_continuous(limits=c(0, 1))
