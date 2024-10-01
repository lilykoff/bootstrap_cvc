# bootstrap library(tidyverse)
library(tidymodels)
library(future)
library(censored)
library(furrr)

# rm(list = ls())
# 100x cross-validated single variable mortality prediction, by wave


# 100x cross-validated single variable mortality prediction
df_all = readRDS(here::here("data", "covariates_accel_mortality_df.rds"))

df_mortality =
  df_all %>%
  filter(valid_accel) %>% # valid accelerometry
  filter(age_in_years_at_screening >= 50 &
           age_in_years_at_screening < 80) %>%  # age criteria
  filter(if_all(.cols = c(age_in_years_at_screening, gender,
                          race_hispanic_origin, cat_education,
                          cat_bmi, chd, chf, heartattack, stroke, cancer,
                          bin_diabetes, cat_alcohol, cat_smoke, bin_mobilityproblem,
                          general_health_condition, mortstat, permth_exm, total_PAXMTSM),
                ~!is.na(.x))) %>% # no missing data
  mutate(event_time = permth_exm / 12) # event time in years = person months since exam / 12

df_mortality_win =
  df_mortality %>%
  ungroup() %>%
  mutate(across(c(contains("total"), contains("peak")), ~DescTools::Winsorize(.x, quantile(.x, probs = c(0, 0.99)))))

# create bootstrap object
set.seed(123)
bootsrap_obj = replicate(1000,
                         df_mortality_win %>%
                           slice_sample(prop = 1, replace = TRUE, by = data_release_cycle),
                         simplify = FALSE)
saveRDS(bootsrap_obj, here::here("data", "bootstrap_obj.rds"))
