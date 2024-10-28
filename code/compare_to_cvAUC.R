# try out influence function thing
library(tidyverse)
library(tidymodels)
library(furrr)
library(future)
library(censored)
# n_cores = parallelly::availableCores() - 1
# print(n_cores)

# need to repeat this process 1000 times
source(here::here("code/utils.R"))
B = 1
fold = NULL
rm(list = c("fold"))

force = FALSE
data_gen_key = readRDS(here::here("data", "data_gen_key.rds"))
sample_sizes = unique(data_gen_key$n)

get_cvauc = function(data, inds, n_folds = 10, n_repeats = 1){
  # df = data[inds,]
  df = data
  folds = vfold_cv(df, v = n_folds, repeats = n_repeats)
  get_predictions = function(split) {
    # Fit the model on the training data
      train_data = analysis(split)
      test_data = assessment(split)
      model = glm(outcome ~ predictor, data = train_data, family = "binomial")
      predictions = predict(model, newdata = test_data)
      predictions %>%
        bind_cols(truth = test_data$outcome) %>%
        rename("pred" = `...1`)
  }

# Apply the function across all folds using map
  predictions_df = folds %>%
    mutate(predictions = map(splits, get_predictions)) %>%
    select(id, predictions) %>%
    unnest(predictions)

    res = cvAUC::ci.cvAUC(predictions_df$pred, predictions_df$truth, folds = predictions_df$id)
    # Return the predictions with the truth
    tibble(
      cvAUC = res$cvAUC,
      ci_lb = res$ci[1],
      ci_ub = res$ci[2]
    )
}

calc_influence =
  function(filename, auc, prevalence, n){
    df = readRDS(filename) %>%
      mutate(outcome = factor(outcome))
    get_cvauc(df, n_folds = 10, n_repeats = 1) %>%
      mutate(auc = auc,
             prev = prevalence,
             n = n)
  }


res_all =
  pmap_dfr(.l = list(filename = data_gen_key$name,
                     auc = data_gen_key$auc,
                     prevalence = data_gen_key$prev,
                     n = data_gen_key$n),
           .f = calc_influence)


