library(AUC)
library(tidyverse)
# https://stats.stackexchange.com/questions/422926/generate-synthetic-data-given-auc
# https://stats.stackexchange.com/questions/562000/how-to-simulate-a-calibrated-prediction-model-given-prevalence-and-auc
set.seed(1230012)

sim_known_auc = function(auc_target, n, prevalence){
  t <- sqrt(log(1/(1-auc_target)**2))
  z <- t-((2.515517 + 0.802853*t + 0.0103328*t**2) /
            (1 + 1.432788*t + 0.189269*t**2 + 0.001308*t**3))
  d <- z*sqrt(2)

  x <- c(rnorm(n * (1-prevalence), mean = 0), rnorm(n * prevalence, mean = d))
  y <- c(rep(0, n * (1-prevalence)), rep(1, n * prevalence))
  return(tibble(predictor = x,
                outcome = y))

}
df = sim_known_auc(0.8, 1*10^7, 0.1)

# AUC::auc(roc(df$predictor, as.factor(df$outcome)))

data_gen_tibble =
  tidyr::expand_grid(
    auc = c(0.6, 0.7, 0.8, 0.9),
    prev = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
  )

df_list =
  map2(data_gen_tibble$auc, data_gen_tibble$prev, ~sim_known_auc(.x, 1*10^7, .y))

readr::write_rds(df_list, here::here("data", "auc_known_list.rds"), compress = "xz")
# for(item in df_list){
#   print(AUC::auc(roc(item$predictor, as.factor(item$outcome))))
# }

