library(AUC)
library(tidyverse)
# https://stats.stackexchange.com/questions/422926/generate-synthetic-data-given-auc
# https://stats.stackexchange.com/questions/562000/how-to-simulate-a-calibrated-prediction-model-given-prevalence-and-auc

sim_known_auc = function(auc_target, n, prevalence) {
  t <- sqrt(log(1 / (1 - auc_target) ** 2))
  z <- t - ((2.515517 + 0.802853 * t + 0.0103328 * t ** 2) /
              (1 + 1.432788 * t + 0.189269 * t ** 2 + 0.001308 * t ** 3)
  )
  d <- z * sqrt(2)
  set.seed(1230012)
  x <- c(rnorm(n * (1 - prevalence), mean = 0), rnorm(n * prevalence, mean = d))
  y <- c(rep(0, n * (1 - prevalence)), rep(1, n * prevalence))
  return(tibble(predictor = x, outcome = y))

}
# test
# df = sim_known_auc(0.8, 1*10^7, 0.1)
# AUC::auc(roc(df$predictor, as.factor(df$outcome)))

sample_sizes = c(1000, 5000, 10000, 100000)
n_pop = 1 * 10 ^ 7
data_gen_key =
  tidyr::expand_grid(
    auc = c(0.6, 0.7, 0.8, 0.9),
    prev = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
    n = sample_sizes
  ) %>%
  mutate(id = row_number(),
         id = sprintf("%02d", id)) %>%
  mutate(name = file.path(here::here("data", "samples"), paste0("fold_", id, ".rds")))

data_gen_tibble =
  tidyr::expand_grid(auc = c(0.6, 0.7, 0.8, 0.9),
                     prev = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>%
  mutate(id = row_number())


# get indices of sample
set.seed(4575)
indices_100000 = sample(1:n_pop, 100000, replace = FALSE)
set.seed(4575)
indices_10000 = sample(indices_100000, 10000, replace = FALSE)
set.seed(4575)
indices_5000 = sample(indices_10000, 5000, replace = FALSE)
set.seed(4575)
indices_1000 = sample(indices_5000, 1000, replace = FALSE)

df_list =
  map2(data_gen_tibble$auc,
       data_gen_tibble$prev,
       ~ sim_known_auc(.x, n_pop, .y))

sample_and_save = function(sample_size, df, num) {
  indices = switch(
    sample_size,
    "1000" = indices_1000,
    "5000" = indices_5000,
    "10000" = indices_10000,
    "100000" = indices_100000
  )
  auc_prev = data_gen_tibble %>% filter(id == num)
  fold_to_write =
    data_gen_key %>%
    filter(auc == auc_prev$auc,
           prev == auc_prev$prev,
           n == as.numeric(sample_size)) %>%
    pull(id)

  temp =
    df[indices, ]

  readr::write_rds(temp, here::here(
    "data",
    "samples",
    paste0("fold_", fold_to_write, ".rds")
  ), compress = "xz")
  rm(temp)
}

for (i in 1:length(df_list)) {
  item = df_list[[i]]
  map(
    .x = c("1000", "5000", "10000", "100000"),
    .f = sample_and_save,
    df = item,
    num = i
  )
}

saveRDS(data_gen_key, here::here("data", "data_gen_key.rds"))

