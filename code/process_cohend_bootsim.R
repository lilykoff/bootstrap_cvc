library(tidyverse)

data_gen_tibble =
  tidyr::expand_grid(
    pop_auc = c(0.6, 0.7, 0.8, 0.9),
    pop_prev = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
  ) %>%
  mutate(fold = as.character(row_number()))

files = list.files(here::here("results", "bootstrap_cohend_auc"), full.names = TRUE)

boot_results =
  map_dfr(.x = files,
      .f = function(x){
        n = sub(".*auc\\/(.+)\\_fold.*", "\\1", x)
        fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", x)
        readRDS(x) %>%
          mutate(fold = fold) %>%
          left_join(data_gen_tibble, by = "fold")
      })
saveRDS(boot_results, here::here("results", "cohen_d_bootsim_allfolds.rds"))

orig_files = list.files(here::here("results", "cohen_d_sim"), full.names = TRUE)
results =
  map_dfr(.x = orig_files,
          .f = function(x){
            fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", x)
            readRDS(x) %>%
              mutate(fold = fold) %>%
              left_join(data_gen_tibble, by = "fold")
          })

saveRDS(results, here::here("results", "cohen_d_sim_allfolds.rds"))
