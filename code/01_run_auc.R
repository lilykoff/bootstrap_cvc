library(tidyverse)
library(tidymodels)
library(furrr)
library(future)
library(censored)
# n_cores = parallelly::availableCores() - 1
# print(n_cores)
source(here::here("code/utils.R"))
B = 1000
fold = NULL
rm(list = c("fold"))

force = FALSE
ifold = get_fold()
data_gen_key = readRDS(here::here("data", "data_gen_key.rds"))
sample_sizes = unique(data_gen_key$n)

get_rcvauc = function(data, inds, n_folds = 10, n_repeats = 100){
  df = data[inds,]
  folds = vfold_cv(df, v = n_folds, repeats = n_repeats)
  results = folds %>%
    mutate(auc = map_dbl(splits, function(split) {
      # Extract training and testing sets
      train_data = analysis(split)
      test_data = assessment(split)
      model = glm(outcome ~ predictor, data = train_data, family = "binomial")
      predictions = predict(model, newdata = test_data)
      res = try({
        roc_obj = pROC::roc(test_data$outcome, predictions, levels = c(0, 1), direction = "<")
        pROC::auc(roc_obj) %>% as.numeric()
      })
      if (inherits(res, "try-error")) {
        return(NA_real_)
      } else {
        return(res)
      }
    })) %>%
    select(rep = id, fold = id2, auc)
  rm(folds)
  rm(df)
  results
}


for(samp in sample_sizes){
  xdf = data_gen_key %>%
    filter(n == samp)
  set.seed(ifold)
  indices = matrix(sample(1:samp, B*samp, replace = TRUE), nrow = B)
  for(i in 1:nrow(xdf)){
    x = xdf[i,]
    print(x)
    out_name = file.path(here::here("results", "boot_repeats", paste0("res_fold_", x$id, "_repeat_", ifold, ".rds")))
    if(!file.exists(out_name) || force){
      temp = try({
        df = readRDS(x$name) %>%
          mutate(outcome = factor(outcome))
        results = apply(indices, MARGIN = 1, FUN = get_rcvauc, data = df) %>%
          bind_rows(.id = "boot")
        write_rds(results, out_name, compress = "xz")
      })
      rm(temp)
    }
  }
}
