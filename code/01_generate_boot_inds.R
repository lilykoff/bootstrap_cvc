library(tidyverse)
library(tidymodels)
library(furrr)
library(future)
library(censored)
n_cores = parallelly::availableCores() - 1
print(n_cores)
source(here::here("code/utils.R"))
force = FALSE

ifold = get_fold()

# repeats = 1000
B = 1000
sample_sizes = c(1000, 5000, 10000, 100000)

for(n in sample_sizes){
  num = which(sample_sizes == n)
  file = file.path(here::here("data", "boot_inds", paste0("inds_n", num, "_repeat", ifold, ".rds")))
  if(!file.exists(file) || force){
    x = matrix(sample(1:n, B*n, replace = TRUE), nrow = B)
    write_rds(x, here::here(file), compress = "xz")
    rm(x)
  }
}


# matrix(sample(1:n, B*n, replace = TRUE), nrow = B)
# # reshape the thing to a list of B samples
# array(sample(1:n, B*n*repeats, replace = TRUE), dim = c(B, n, repeats))
#
# # break up the repeats
# # function that, for each sample size, will generate indices for B bootstrapped samples
# gen_samples = function(seed, B, n){
#   set.seed(seed)
#   replicate(B, sample(1:n, n, replace = TRUE), simplify = FALSE)
# }
#
#
# # create df to store the samples
# for(n in sample_sizes){
#   file = file.path(here::here("data", paste0("bootstrap_indices_", n, ".rds")))
#   if(!file.exists(file) || force){
#     df = tidyr::expand_grid(rep = 1:repeats) %>%
#       mutate(samples = map(rep, ~ gen_samples(.x, B = B, n = n)))
#     readr::write_rds(df, here::here(file), compress = "xz")
#     rm(df)
#   }
# }
#
#
#
