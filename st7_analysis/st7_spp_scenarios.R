load(paste0(outputs_path, '/spp/L202.spp_logisticFun.RData'))
library(ggplot2)

data = do.call(rbind,L202.spp_fuelPrefElast) %>%
  mutate(scenario_type = stringr::str_extract(scenario, "^[^_]+_[^_]+")) %>%
  filter(region == 'India')
  # group_by(year, scenario, scenario_type, x0, k) %>%
  # summarise(value = median(value))

ggplot(data %>% filter(scenario_type %in% c("spp_all20"))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type))

