library(tidyverse)


cal <- read_csv("data/calibration_data/metadata_calibration_samples.csv") %>% 
  select(sample_id, TC_gkg) %>% 
  mutate(set = "calibration")
val <- read_csv("data/validation_samples/validation_samples.csv") %>% 
  select(sample_id, TC_gkg) %>% 
  mutate(set = "validation")

merged <- bind_rows(cal, val)

library(ggridges)

p_ref <- ggplot(merged, aes(x = TC_gkg, y = set, fill = set))+
  geom_density_ridges2() +
  scale_fill_manual(values = c("#ff9501", "#e91fa9"))+
  ylab("") +
  xlab(expression(paste("SOC (g ", kg^{-1}, ")"))) +
  theme_minimal() +
  theme(legend.position = "none")


ggsave(p_ref, "")



