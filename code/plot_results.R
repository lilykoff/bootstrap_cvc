library(tidyverse)
library(paletteer)
paletteer_d("ggthemes::colorblind")
c1 = "#E69F00FF"; c2 = "#009E73FF"; c3 = "#0072B2FF"; c4 = "#CC79A7FF"
c5 = "#56B4E9FF"; c6 = "#D55E00FF"
boot_results = readRDS(here::here("results", "cohen_d_bootsim_allfolds.rds"))
results = readRDS(here::here("results", "cohen_d_sim_allfolds.rds"))


results %>%
  drop_na() %>%
  mutate(repeat_num = as.numeric(sub(".*Repeat", "", id))) %>%
  filter(repeat_num <= 100) %>%
  mutate(sample_size = factor(sample_size)) %>%
  group_by(sample_size, pop_auc, pop_prev) %>%
  mutate(cummean_auc = dplyr::cummean(auc),
         pop_prev = paste0("Prevalence = ", pop_prev),
         pop_auclab = paste0("AUC = ", pop_auc)) %>%
  ggplot(aes(x = repeat_num, y = cummean_auc, color = sample_size)) +
  facet_grid(pop_auclab ~ pop_prev, scales = "free")+
  geom_line() +
  geom_hline(aes(yintercept = pop_auc), linetype = "dashed")+
  theme_light() +
  scale_color_manual(values = c(c1, c2, c3, c4), name = "Sample Size") +
  labs(x = "Number of 10-fold Repeats to Average Over", y = "Cumulative Mean AUC",
       title = "Comparison of 100 times Repeated 10-fold Cross Validation",
       subtitle = "On Samples from Populations with Varying AUC and Prevalence")


# for now summarize results from 100x repeat 10-fold cvc
results_summ =
  results %>%
  mutate(repeat_num = as.numeric(sub(".*Repeat", "", id))) %>%
  filter(repeat_num <= 100) %>%
  group_by(sample_size, pop_auc, pop_prev) %>%
  summarize(auc_sample_mean = mean(auc, na.rm = TRUE)) %>%
  ungroup()

bs_res_summ =
  boot_results %>%
  mutate(repeat_num = as.numeric(sub(".*Repeat", "", id))) %>%
  filter(repeat_num <= 100) %>%
  group_by(sample_size, pop_auc, pop_prev, boot) %>%
  summarize(auc = mean(auc, na.rm = TRUE)) %>%
  ungroup()

bs_cis =
  bs_res_summ %>%
  group_by(sample_size, pop_auc, pop_prev) %>%
  summarize(boot_sd = sd(auc),
            boot_mean = mean(auc),
            pctile_lb = quantile(auc, 0.025),
            pctile_ub = quantile(auc, 0.975)) %>%
  ungroup() %>%
  left_join(results_summ, by = c("sample_size", "pop_auc", "pop_prev")) %>%
  mutate(normal_lb = auc_sample_mean - 1.96 * boot_sd,
         normal_ub = auc_sample_mean + 1.96 * boot_sd)

bs_cis %>%
  filter(pop_auc == 0.6 & pop_prev == 0.05) %>%
  pivot_longer(cols = c(contains("ub"), contains("lb")), names_to = "type", values_to = "value") %>%
  mutate(bound = sub(".*\\_", "", type),
         type = sub("\\_.*", "", type)) %>%
  pivot_wider(names_from = bound, values_from = value) %>%
  ggplot(aes(x = as.factor(sample_size), ymin = ub, ymax = lb, color = type)) +
  # geom_point(position = position_dodge(width = .1)) +
  geom_errorbar(width = .1, position = position_dodge(width = .1))


bs_cis %>%
  pivot_longer(cols = c(contains("ub"), contains("lb")), names_to = "type", values_to = "value") %>%
  mutate(bound = sub(".*\\_", "", type),
         type = sub("\\_.*", "", type)) %>%
  pivot_wider(names_from = bound, values_from = value) %>%
  mutate(pop_prev = paste0("Prevalence = ", pop_prev),
         pop_auclab = paste0("AUC = ", pop_auc)) %>%
  ggplot(aes(x = as.factor(sample_size), ymin = ub, ymax = lb, color = type)) +
  # geom_point(position = position_dodge(width = .1)) +
  geom_errorbar(width = .2, position = position_dodge(width = .2), linewidth = .9) +
  facet_grid(pop_auclab ~ pop_prev, scales = "free_y") +
  geom_hline(aes(yintercept = pop_auc), linetype = "dashed") +
  theme_light() +
  scale_color_manual(values = c(c5, c6), name = "Type", labels = c("Normal-Based", "Percentile Based")) +
  labs(x = "Sample Size", y = "AUC (95% Bootstrapped CI)")
