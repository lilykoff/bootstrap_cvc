library(tidyverse)


files = list.files(here::here("results", "boot_repeats"))
# the fold identifies the sample size, prevalence, and AUC

folds = sub(".*fold\\_(.+)\\_repeat.*", "\\1", files) %>% unique()
data_gen_key = readRDS(here::here("data", "data_gen_key.rds"))


for(f in folds){
  files = list.files(here::here(
    "results", "boot_repeats"),
    pattern = paste0("res_fold_", f, "_repeat*"),
    full.names = TRUE)

  res_df =
    map_dfr(.x = files,
            .f = function(x){
              rep_num = sub(".*repeat\\_(.+)\\.rds", "\\1", x)
              readRDS(x) %>%
                group_by(boot, rep) %>%
                summarize(cv_auc = mean(auc, na.rm = TRUE)) %>%
                group_by(boot) %>%
                summarize(rcv_auc = mean(cv_auc, na.rm = TRUE)) %>%
                ungroup() %>%
                summarize(across(rcv_auc,
                                 list(ci_lb = ~quantile(.x, 0.025),
                                      ci_ub = ~quantile(.x, 0.975)))) %>%
                mutate(repeat_num = rep_num)
            })
  fname = file.path(here::here("results", "boot_coverage", paste0("rcv_auc_fold_", f, ".rds")))
  if(!file.exists(fname) || force){
    saveRDS(fname)
  }
}
