
# n x k  dimension of training set design matrix

# n = 500, 1000, 5000, 10000, 20000
# k = 10, 50, 100, 200

# number of covars correlated with outcome = 10

# generate 100000 pts from N(mu, sigma), let y = 0
# generate 100000 pts from N(v, sigma), let y = 1
# let mu_i = 0, vi = 0.3 for i = 1,...,10, sigma = identiy covariance matrix

library(MASS)
library(tidyverse)
library(glmnet)
library(tidymodels)
library(pROC)

gen_data = function(k, n_inf = 10, n = 100000){
  mu = rep(0, n_inf)
  sigma = diag(n_inf)
  nu = rep(0.3, n_inf)

  mu2 = rep(0, k - n_inf)
  sigma2 = diag(k - n_inf)

  if(k > n_inf){
    non_inf = mvrnorm(n * 2, mu2, sigma2) %>%
      as_tibble(.name_repair = "unique") %>%
      janitor::clean_names()

    data_0_inf = mvrnorm(n, mu, sigma) %>%
      as_tibble(.name_repair = "unique") %>%
      mutate(Y = 0) %>%
      janitor::clean_names()

    data_1_inf = mvrnorm(n, nu, sigma) %>%
      as_tibble(.name_repair = "unique") %>%
      mutate(Y = 1) %>%
      janitor::clean_names()

    data = bind_rows(data_0_inf, data_1_inf) %>%
      bind_cols(non_inf) %>%
      janitor::clean_names()
  } else {
    data_0_inf = mvrnorm(n, mu, sigma) %>%
      as_tibble(.name_repair = "unique") %>%
      mutate(Y = 0) %>%
      janitor::clean_names()

    data_1_inf = mvrnorm(n, nu, sigma) %>%
      as_tibble(.name_repair = "unique") %>%
      mutate(Y = 1) %>%
      janitor::clean_names()

    data = bind_rows(data_0_inf, data_1_inf)
    }

 data

}

data_list = map(.x = c(10, 50, 100, 200), .f = gen_data)



get_true_auc = function(pop_data){
  X = as.matrix(pop_data %>% select(-y))
  y = pop_data$y
  model = glmnet(X, y, family = "binomial", alpha = 1)
  cv_lasso = cv.glmnet(X, y, alpha = 1)

  # Get the best lambda
  best_lambda = cv_lasso$lambda.min

  # Get predictions for new data
  predictions = predict(cv_lasso, newx = X, s = best_lambda) %>% as.numeric()

  roc_obj = pROC::roc(y, predictions, levels = c(0, 1), direction = "<")
  pROC::auc(roc_obj) %>% as.numeric()
}

true_auc = map(.x = data_list, .f = get_true_auc)

get_crossval_auc = function(pop_data, n){
  inds = sample(1:nrow(pop_data), n, replace = FALSE)
  data = pop_data[inds,]
  folds = vfold_cv(data, v = 10, strata = y)
  get_predictions = function(split) {
    # Fit the model on the training data
    train_data = analysis(split)
    test_data = assessment(split)
    X = as.matrix(train_data %>% select(-y))
    y = train_data$y
    model = glmnet(X, y, family = "binomial", alpha = 1)
    cv_lasso = cv.glmnet(X, y, alpha = 1)
    # Get the best lambda
    best_lambda = cv_lasso$lambda.min
    X_new = as.matrix(test_data %>% select(-y))
    # Get predictions for new data
    predictions = predict(cv_lasso, newx = X_new, s = best_lambda) %>% as.numeric() %>%
      bind_cols(truth = test_data$y) %>%
      rename("pred" = `...1`)
  }

  # Apply the function across all folds using map
  predictions_df = folds %>%
    mutate(predictions = map(splits, get_predictions)) %>%
    select(id, predictions) %>%
    unnest(predictions)

  auc_results = predictions_df %>%
    group_by(id) %>%
    mutate(truth = factor(truth)) %>%
    roc_auc(truth, pred, event_level = "second") %>%
    rename("AUC" = .estimate) %>%
    select(-.metric, -.estimator) %>%
    ungroup() %>%
    summarize(mean_fold_auc = mean(AUC, na.rm = TRUE))

  res = cvAUC::ci.cvAUC(predictions_df$pred, predictions_df$truth, folds = predictions_df$id)
  # Return the predictions with the truth
  tibble(
    cvAUC = res$cvAUC,
    ci_lb = res$ci[1],
    ci_ub = res$ci[2],
    foldauc = auc_results$mean_fold_auc,
    n = n
  )
}

# need to repeat this process 5000 times to calculate coverage probabilites

sample_sizes = c(500, 1000, 5000, 10000, 20000)


xval_aucs = map(.x = data_list,
                 .f = \(x) map_dfr(.x = sample_sizes, .f = get_crossval_auc, pop_data = x)) %>%
            bind_rows(.id = "k")

xval_aucs_rep = replicate(100,
                      map(.x = data_list,
                          .f = \(x) map_dfr(.x = sample_sizes, .f = get_crossval_auc, pop_data = x)) %>%
                        bind_rows(.id = "k"),
                      simplify = FALSE)

xval_aucs_df =
  xval_aucs_rep %>%
  bind_rows(.id = "rep")
