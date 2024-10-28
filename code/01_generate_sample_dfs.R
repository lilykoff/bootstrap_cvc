#### not needed

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
ifold = get_fold()
fname = file.path(here::here("data", "known_auc_populations", paste0("population_", ifold, ".rds")))
# read in population
population = readRDS(here::here(fname))

force = FALSE
n_pop = nrow(population)
sample_sizes = c(1000, 5000, 10000, 100000)


set.seed(4575)
indices_100000 = sample(1:n_pop, 100000, replace = FALSE)
set.seed(4575)
indices_10000 = sample(indices_100000, 10000, replace = FALSE)
set.seed(4575)
indices_5000 = sample(indices_10000, 5000, replace = FALSE)
set.seed(4575)
indices_1000 = sample(indices_5000, 1000, replace = FALSE)

sample_and_save = function(id, indices){

}


key = tidyr::expand_grid(
  auc = c(0.6, 0.7, 0.8, 0.9),
  prev = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
  n = sample_sizes
) %>%
  mutate(id = row_number())

saveRDS(key, here::here("data", "sample_indices.rds"))




