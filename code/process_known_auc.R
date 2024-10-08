library(tidyverse)
require(colorout)
results = list.files(here::here("results", "auc_sim"),
                     full.names = TRUE,
                     pattern = "*.rds")


auc_res = map_dfr(results, .f = function(x){readRDS(x) %>%
    mutate(fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", x))})


readr::write_rds(auc_res, here::here("results", "known_auc.rds"))
# calculate running mean of AUC along folds
