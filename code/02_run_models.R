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
ifold = get_fold()
fname = file.path(here::here(
  "data",
  "samples",
  paste0("fold_", sprintf("%02d", ifold), ".rds")))

out = file.path(here::here("results", "boot_repeats", paste0("boot_fold_", sprintf("%02d", ifold), ".rds")))

if(!file.exists(out) || force){
  df  = readRDS(here::here(fname)) %>%
    mutate(outcome = factor(outcome))
  data_gen_key = readRDS(here::here("data", "data_gen_key.rds"))

  sample_size =
    data_gen_key %>%
    filter(id == sprintf("%02d", ifold)) %>%
    pull(n)


  boot_inds = readRDS(here::here("data", paste0("bootstrap_indices_", sample_size, ".rds")))

  calc_auc = function(train_data, test_data) {
    # Example: Fit a model on the training data and evaluate on the test data
    # You can replace this with your own function
    model = glm(outcome ~ predictor, data = train_data, family = "binomial")
    predictions = predict(model, newdata = test_data)
    x = try({
      roc_obj = pROC::roc(test_data$outcome, predictions, levels = c(0, 1), direction = "<")
      pROC::auc(roc_obj) %>% as.numeric()
    })
    if (inherits(x, "try-error")) {
      return(NA_real_)
    } else {
      return(x)
    }
  }

  # function to calculate AUC for set of indices and data
  get_auc = function(inds, df, n_folds, n_repeats){
    data = df[inds,]
    set.seed(456)
    folds = vfold_cv(data, v = n_folds, repeats = n_repeats)
    results = folds %>%
      mutate(auc = map_dbl(splits, function(split) {
        # Extract training and testing sets
        train_data = analysis(split)
        test_data = assessment(split)

        # Apply the custom function and return as a numeric value
        calc_auc(train_data, test_data)
      })) %>%
      select(rep = id, fold = id2, auc)
  }

  apply_onerow = function(row, data, ind_df){
    indices = ind_df$samples[[row]]
    map_dfr(.x = indices,
            .f = get_auc,
            df = data,
            n_folds = 10,
            n_repeats = 100,
            .id = "boot")
  }
  # apply function over all repeats of bootstrapping process
  all_rows =
    map_dfr(
      .x = 1:nrow(boot_inds),
      ind_df = boot_inds,
      .f = apply_onerow,
      data = df,
      .id = "bootrepeat"
    )

  write_rds(all_rows, out, compress = "xz")
}

