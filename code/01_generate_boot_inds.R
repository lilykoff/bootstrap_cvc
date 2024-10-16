library(tidyverse)
library(tidymodels)
library(furrr)
library(future)
library(censored)
n_cores = parallelly::availableCores() - 1
print(n_cores)
source(here::here("code/utils.R"))

repeats = 1000
B = 1000
sample_sizes = c(1000, 5000, 10000, 100000)



# function that, for each sample size, will generate indices for B bootstrapped samples
gen_samples = function(seed, B, n){
  set.seed(seed)
  replicate(B, sample(1:n, n, replace = TRUE), simplify = FALSE)
}


# create df to store the samples
for(n in sample_sizes){
  df = tidyr::expand_grid(rep = 1:repeats) %>%
    mutate(samples = map(rep, ~ gen_samples(.x, B = B, n = n)))
  readr::write_rds(df, here::here("data", paste0("bootstrap_indices_", n, ".rds")), compress = "xz")
  rm(df)
}



