#### PREPROCESS ================================================================
# ==============================================================================

# setwd to file location === #####
setwd('C:\\GCAM\\GCAM_7.0_Claudia\\gcam-core-iamcompact\\st7_analysis')
.libPaths('C:\\Users\\claudia.rodes\\Documents\\R\\win-library')

# load libraries and paths' variables, and extra functions and styles
source('load_libs_paths.R')
source('utils_data.R')
source('utils_style.R')

# load basic data
load_mapping_data()

# load project
prj_ref = load_prj('st7_Reference.dat',list_scen_reference)

# load queries
year_s = 2000
year_e = 2050
selected_scen = list_scen_reference

load_queries()
rm(prj)
gc()

#### FIGURES ===================================================================
# ==============================================================================

# select year and scenario palette
selected_year = 2030
scen_palette = scen_palette_studyReference

# create figures' subdirectory
dir_name = 'st7_studyReference'
if (!dir.exists(paste0(figures_path,dir_name))) dir.create(paste0(figures_path,dir_name))

# share plant consumption world
share_plant_data = food_consumption_world %>%
  # subset protein
  dplyr::filter(nestingSector1 == 'Protein') %>%
  select(Units, scenario, nestingSector2, year, value) %>% unique() %>%
  # sum consumption by animal vs plant protein
  group_by(Units, scenario, nestingSector2, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  # compute plant_share
  group_by(Units, scenario, year) %>%
  summarise(plant_share = 100 * sum(value[nestingSector2 == "Plant"]) / sum(value)) %>%
  ungroup() %>%
  filter(scenario %in% selected_scen) %>% rename_scen()

pl_protein_share_world = ggplot(data = share_plant_data) +
  geom_line(aes(x = year, y = plant_share, color = scenario), alpha = 1, linewidth = 2) +
  # scale
  scale_color_manual(values = scen_palette, name = 'Scenario') +
  scale_fill_manual(values = scen_palette, name = 'Scenario') +
  # labs
  labs(y = 'share of plant protein (%)', x = '') +
  ggtitle('World plant share consumption') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_protein_share_world, file = paste0(figures_path,dir_name,"/",'pl_protein_share_world.pdf'),
       width = 500, height = 500, units = 'mm')

print(summary(share_plant_data %>%
                tidyr::pivot_wider(names_from = scenario, values_from = plant_share) %>%
                mutate(diff_between_refs = St7_Reference_original - `St7_Reference_R-M-F`) %>%
                pull(diff_between_refs)))
###     Min.    1st Qu.     Median       Mean    3rd Qu.       Max.
### -1.368e-01 -9.678e-02 -5.382e-02 -5.702e-02 -5.299e-03  6.000e-08
### the variation si of at most 0.13 percentual points between the scenarios =>
### we consider as the reference scenario the one with the distinction R-M-F



#### STUDY CURRENT TRENDS AND %SHARES TO CALIBRATE THE SCENARIOS ===============
# ==============================================================================

# share plant consumption world
share_plant_data_reg = food_consumption_regional %>%
  # subset protein
  dplyr::filter(nestingSector1 == 'Protein',
                scenario == 'St7_Reference_R-M-F') %>%
  select(Units, region, scenario, nestingSector2, year, value) %>% unique() %>%
  # sum consumption by animal vs plant protein
  group_by(Units, region, scenario, nestingSector2, year) %>%
  mutate(value = sum(value)) %>%
  ungroup() %>%
  # compute plant_share
  group_by(Units, region, scenario, year) %>%
  summarise(plant_share = 100 * sum(value[nestingSector2 == "Plant"]) / sum(value)) %>%
  ungroup() %>%
  filter(scenario %in% selected_scen) %>% rename_scen()

tmp_share_plant_data_reg = share_plant_data_reg %>% filter(year == 2015)
write.csv(tmp_share_plant_data_reg, file = paste0(outputs_path, 'st7_studyReference_reference_values_plant_share_2015.csv'))
rm(tmp_share_plant_data_reg)

pl_protein_share_regional = ggplot(data = share_plant_data_reg %>%
                                     filter(year == 2015)) +
  geom_bar(stat = 'identity', aes(x = reorder(region, -plant_share), y = plant_share, fill = scenario), alpha = 1) +
  # scale
  scale_color_manual(values = scen_palette, name = 'Scenario') +
  scale_fill_manual(values = scen_palette, name = 'Scenario') +
  # labs
  labs(y = 'share of plant protein (%)', x = '') +
  ggtitle('Regional plant share consumption in 2015') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        axis.text.x = element_text(size=30, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_protein_share_regional, file = paste0(figures_path,dir_name,"/",'pl_protein_share_bars_regional.pdf'),
       width = 500, height = 500, units = 'mm')


#### CALIBRATE SPP AND FUELPREFELAST PARAMETER =================================
# ==============================================================================

## calibrate relation between spp and fuelPrefElast
selected_scen = c('St7_Reference_R-M-F','St7_calibrate_FuelPrefElast_fvv0.45')
share_plant_data = read.csv(paste0(outputs_path,"spp/calibration_food_consumption.csv"), skip = 1, header = TRUE) %>%
  rename_with(~ gsub("^X", "", .), starts_with("X")) %>%
  rename(nestingSector1 = subsector,
         nestingSector2 = subsector.1,
         nestingSector3 = subsector.2) %>%
  separate(nestingSector1, into = c("nestingSector1", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
  separate(nestingSector2, into = c("nestingSector2", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
  separate(nestingSector3, into = c("nestingSector3", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
  select(1:17,21) %>% unique() %>%
  tidyr::pivot_longer(cols = 7:17, names_to = 'year', values_to = 'value') %>%
  # subset protein
  dplyr::filter(nestingSector1 == 'Protein') %>%
  select(Units, scenario, nestingSector2, year, value) %>% unique() %>%
  # sum consumption by animal vs plant protein
  group_by(Units, scenario, nestingSector2, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  # compute plant_share
  group_by(Units, scenario, year) %>%
  summarise(plant_share = 100 * sum(value[nestingSector2 == "Plant"]) / sum(value)) %>%
  ungroup() %>%
  rename_scen() %>% filter(scenario %in% selected_scen)
write.csv(share_plant_data, file = paste0(outputs_path, 'spp/st7_calibrate_spp_fuelPrefElast.csv'))
conversion_factor = 0.45/share_plant_data[share_plant_data$year == '2050' & share_plant_data$scenario == 'St7_calibrate_FuelPrefElast_fvv0.45',]$plant_share
print(conversion_factor)
### conversion factor: fuelPrefElast = spp_f * 0.007325504



#### STUDY 2015 MICRO & MACRO NUTRIENTS INTAKE =================================
# ==============================================================================


