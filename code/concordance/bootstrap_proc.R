# process bootstrap results
library(tidyverse)
files = list.files(here::here("results", "bootstrap"), full.names = TRUE)


all_files =
  map(.x = files,
      .f = readRDS) %>%
  list_rbind(names_to = "fold")
all_files

saveRDS(all_files, here::here("results", "all_concordance_boot_uni.rds"))

##############

x  = readRDS(here::here("results", "all_concordance_boot_uni.rds"))
x %>%
  mutate(variable = sub(".*total\\_", "", variable)) %>%
  mutate(variable = fct_reorder(variable, concordance, .fun = mean)) %>%
  ggplot(aes(y = concordance, x = variable))+
  geom_violin(draw_quantiles = c(.025, 0.5, 0.975)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

label_df =
  x %>%
  mutate(variable = sub(".*total\\_", "", variable)) %>%
  group_by(variable) %>%
  summarize(q025 = min(concordance))
  # summarize(q025 = quantile(concordance, 0.025))
x %>%
  mutate(variable = sub(".*total\\_", "", variable)) %>%
  mutate(variable = fct_reorder(variable, concordance, .fun = mean)) %>%
  ggplot(aes(y = concordance, x = variable, color = variable))+
  geom_violin(draw_quantiles = c(.025, 0.5, 0.975)) +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  ggrepel::geom_text_repel(data = label_df, aes(x = variable, y = q025, label = variable),
                           angle = 90, hjust = 1, vjust = 1, segment.colour=NA, inherit.aes = FALSE)+
  theme(legend.position = "none")





vec_age = x$concordance[x$variable=="age_in_years_at_screening"]
vec_scrf = x$concordance[x$variable=="total_scrfsteps"]
vec_adept = x$concordance[x$variable=="total_adeptsteps"]
t.test(vec_age, vec_scrf, paired = FALSE)

t.test(vec_adept, vec_scrf, paired = FALSE)
