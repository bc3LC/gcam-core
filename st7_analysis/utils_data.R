
# load mapping data
load_mapping_data = function() {
  db_scen_mapping <<- read.csv(paste0(inputs_path,"mappings/db_scen_mapping.csv"), header = TRUE) %>%
    as.data.table()
  iso_gcam_regions <<- read.csv(paste0(inputs_path,"mappings/iso_GCAM_regID.csv"), skip = 6)
  id_gcam_regions <<- read.csv(paste0(inputs_path,"mappings/gcam_id_to_region.csv"))
  colnames(id_gcam_regions) = c('GCAM_region_ID', 'region')
  country_gcam_regions <- read.csv(paste0(inputs_path,"mappings/country_to_gcam_id.csv"))
  regions_key <<- left_join(country_gcam_regions, id_gcam_regions, by = "GCAM_region_ID") %>%
    select(-1)

  list_scen_reference <<- db_scen_mapping$db_scen_name[db_scen_mapping$scen_type == 'Reference']
  list_scen_fvv <<- db_scen_mapping$db_scen_name[db_scen_mapping$scen_type == 'FVV']
  list_scen_noRumiants <<- db_scen_mapping$db_scen_name[db_scen_mapping$scen_type == 'noRumiants']

  ## food items
  food_sector <<- c('Beef','Corn','Dairy','FiberCrop','FodderHerb','Fruits','Legumes',
                  'MiscCrop','NutsSeeds','OilCrop','Pork','Poultry','Rice','RootTuber',
                  'SheepGoat','Soybean','SugarCrop','Vegetables','Wheat','OilPalm',
                  'FodderGrass')
  # staple commodities
  staples <<- c("Corn", "OtherGrain", "Rice", "RootTuber", "Wheat")
  # animal commodities
  animal_commodities <<- c("Beef", "Dairy", "OtherMeat_Fish", "Pork", "Poultry", "SheepGoat")

}



# load nutritional data
load_nutritional_data = function() {
  # compute socioeconomic data
  # Socio-econ data
  ssp_data <<- read.csv(paste0(inputs_path,"nutrition/SSP2_population_by_demographic.csv"), skip = 1)
  ssp_data_clean <- iso_gcam_regions %>%
    select(-region_GCAM3, -GCAM_region_ID) %>%
    left_join(ssp_data, by = "iso", multiple = 'all') %>%
    select(-MODEL, -REGION) %>%
    rename(scenario = SCENARIO,
           variable = VARIABLE,
           unit = UNIT)
  # Remove X from year columns
  colnames(ssp_data_clean) <- gsub("X", "", colnames(ssp_data_clean))
  # Pivot longer
  ssp_data_long <<- ssp_data_clean %>%
    tidyr::pivot_longer(cols = 6:24, names_to = "year", values_to = "value") %>%
    mutate(value = value * 1e6,
           unit = "total population")
  # Isolate reference (total) population
  reference_pop <<- ssp_data_long %>%
    filter(variable == "Population") %>%
    rename(total_pop = value) %>%
    select(iso, year, total_pop)
  # Join and calculate demographic shares of population
  ssp_data_final <<- ssp_data_long %>%
    filter(variable != "Population") %>%
    left_join(reference_pop, by = c("iso", "year")) %>%
    mutate(demo_share = value / total_pop) %>%
    rename(sub_pop = value)  %>%
    # Remove total male and total female pop, we want by age/sex
    filter(!(variable %in% c("Population|Male", "Population|Female"))) %>%
    rename(pop_units = unit)
  # Get population by sex and age
  # Population weighting
  total_regional_pop <<- ssp_data_final %>%
    filter(year >= year_s, year <= year_e) %>%
    select(-scenario, -iso) %>%
    # get GCAM regions instead of country names
    left_join(regions_key, by = "country_name") %>%
    # get total regional population
    group_by(year, GCAM_region_ID, country_name, region) %>%
    # isolate total population by country
    distinct(total_pop) %>%
    group_by(year, GCAM_region_ID, region) %>%
    # sum for total regional population
    mutate(total_regional_pop = sum(total_pop)) %>%
    ungroup()

  weighted_pop <<- ssp_data_final %>%
    filter(year >= year_s, year <= year_e) %>%
    select(-scenario, -iso) %>%
    # get GCAM regions instead of country names
    left_join(regions_key, by = "country_name") %>%
    # get total regional population
    left_join(total_regional_pop) %>%
    # weight each country by its population over total regional pop
    group_by(country_name, year) %>%
    mutate(weight = total_pop / total_regional_pop) %>%
    # get GCAM population
    left_join(mutate(pop_all_regions, year = as.character(year)), by = c("region", "year"),
              multiple = "all") %>%
    # compute GCAM population by sex and age for each country
    mutate(weighted_demographics = demo_share * weight * population)

  weighted_pop_sex_age <<- weighted_pop %>%
    select(-pop_units, -sub_pop, -total_pop, -demo_share, -weight) %>%
    group_by(variable, year, region) %>%
    # sum the weighted averages for each country into GCAM regions
    summarize(pop_sex_age = sum(weighted_demographics))
}



# load both mapping and nutritional basic data
load_basic_data = function() {
  load_mapping_data()
  load_nutritional_data()
}


# find the db name given the scenario name
find_db_name = function(scenario) {
  return(db_scen_mapping$db_name[db_scen_mapping$db_scen_name == scenario])
}



# load or create rgcam project
load_prj = function(prj_name, prj_desired_scen) {
  db_path = '../output'
  query_path = paste0(inputs_path, 'queries/')
  queries <<- 'queries_beh.xml'

  if (!file.exists(prj_name)) {
    print('create prj')
    for (sc in prj_desired_scen) {
      print(sc)
      db_name = find_db_name(sc)

      ## create prj
      conn <- rgcam::localDBConn(db_path, db_name)
      prj <<- rgcam::addScenario(conn, prj_name, sc,
                                 paste0(query_path, queries),
                                 clobber = FALSE)

      # add 'nonCO2' large query
      fill_queries(db_path, db_name, prj_name, sc)
    }

    saveProject(prj, file = prj_name)

  } else {
    ## load prj
    print('load prj')
    prj <<- loadProject(prj_name)
  }

  return(prj)
}


# aux function of fill_queries to load heavy queries
data_query = function(type, db_path, db_name, prj_name, scenarios) {
  dt = data.frame()
  xml <- xml2::read_xml(paste0(inputs_path, 'queries/queries_beh_nonCO2.xml'))
  qq <- xml2::xml_find_first(xml, paste0("//*[@title='", type, "']"))

  emiss_list = unique(my_pol$Pollutant)
  for (sc in scenarios) {
    while (length(emiss_list) > 0) {
      current_emis = emiss_list[1:min(21,length(emiss_list))]
      qq_sec = gsub("current_emis", paste0("(@name = '", paste(current_emis, collapse = "' or @name = '"), "')"), qq)

      prj_tmp = rgcam::addSingleQuery(
        conn = rgcam::localDBConn(db_path,
                                  db_name,migabble = FALSE),
        proj = prj_name,
        qn = type,
        query = qq_sec,
        scenario = sc,
        regions = NULL,
        clobber = TRUE,
        transformations = NULL,
        saveProj = FALSE,
        warn.empty = FALSE
      )

      tmp = data.frame(prj_tmp[[sc]][type])
      if (nrow(tmp) > 0) {
        dt = dplyr::bind_rows(dt,tmp)
      }
      rm(prj_tmp)

      if (length(emiss_list) > 21) {
        emiss_list <- emiss_list[(21 + 1):length(emiss_list)]
      } else {
        emiss_list = c()
      }
    }
  }
  # Rename columns
  new_colnames <- sub(".*\\.(.*)", "\\1", names(dt))
  names(dt) <- new_colnames

  return(dt)
}




# load heavy queries
fill_queries = function(db_path, db_name, prj_name, sc) {
  # add nonCO2 query manually (it is too big to use the usual method)
  print('nonCO2 emissions by sector')
  dt_sec = data_query('nonCO2 emissions by sector (excluding resource production)', db_path, db_name, prj_name, sc)
  prj_tmp <- rgcam::addQueryTable(project = prj_name, qdata = dt_sec,
                                  queryname = 'nonCO2 emissions by sector (excluding resource production)', clobber = FALSE)
  prj <<- rgcam::mergeProjects(prj_name, list(prj,prj_tmp), clobber = FALSE)
}



# load queries
load_queries = function() {

  ###### ==== food and agriculture ====
  food_demand_world <<- rgcam::getQuery(prj, "food demand") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, input, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  food_demand_regional <<- rgcam::getQuery(prj, "food demand") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(region, scenario, input, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  # staple and non-staple calories
  staples_nonstaples_world <<- getQuery(prj, "food demand") %>%
    filter(scenario %in% selected_scen) %>%
    filter(year >= year_s, year <= year_e) %>%
    mutate(input = gsub("FoodDemand_", "", input)) %>%
    group_by(scenario, Units, year, input) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = input, values_from = value) %>%
    left_join(getQuery(prj, "food demand") %>%
                filter(scenario %in% selected_scen) %>%
                filter(year >= year_s, year <= year_e) %>%
                select(-input) %>%
                group_by(scenario, Units, year) %>%
                summarize(Total = sum(value)))

  staples_nonstaples_regional <<- getQuery(prj, "food demand") %>%
    filter(scenario %in% selected_scen) %>%
    filter(year >= year_s, year <= year_e) %>%
    mutate(input = gsub("FoodDemand_", "", input)) %>%
    tidyr::pivot_wider(names_from = input, values_from = value) %>%
    left_join(getQuery(prj, "food demand") %>%
                filter(scenario %in% selected_scen) %>%
                filter(year >= year_s, year <= year_e) %>%
                select(-input) %>%
                group_by(scenario, region, Units, year) %>%
                summarize(Total = sum(value))) %>%
    select(-c('gcam-consumer', 'nodeinput'))


  food_consumption_world <<- rgcam::getQuery(prj, "food consumption by type (specific)") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, subsector...4, subsector...5, subsector...6, technology, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    rename(nestingSector1 = subsector...4) %>%
    separate(nestingSector1, into = c("nestingSector1", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
    rename(nestingSector2 = subsector...5) %>%
    separate(nestingSector2, into = c("nestingSector2", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
    rename(nestingSector3 = subsector...6) %>%
    separate(nestingSector3, into = c("nestingSector3", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
    filter(year >= year_s, year <= year_e)

  food_consumption_regional <<- rgcam::getQuery(prj, "food consumption by type (specific)") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, region, scenario, subsector...4, subsector...5, subsector...6, technology, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    rename(nestingSector1 = subsector...4) %>%
    separate(nestingSector1, into = c("nestingSector1", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
    rename(nestingSector2 = subsector...5) %>%
    separate(nestingSector2, into = c("nestingSector2", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
    rename(nestingSector3 = subsector...6) %>%
    separate(nestingSector3, into = c("nestingSector3", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
    filter(year >= year_s, year <= year_e)

  ag_production_world <<- rgcam::getQuery(prj, "ag production by crop type") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(scenario, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_production_regional <<- rgcam::getQuery(prj, "ag production by crop type") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(region, scenario, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  food_demand_prices_world <<- rgcam::getQuery(prj, "food demand prices") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, input, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  food_demand_prices_regional <<- rgcam::getQuery(prj, "food demand prices") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, region, scenario, input, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_prices <<- rgcam::getQuery(prj, "ag import vs. domestic prices") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, sector, subsector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_prices_world <<- rgcam::getQuery(prj, "ag regional prices (weighted average b/t domestic and imported prices)") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_prices_regional <<- rgcam::getQuery(prj, "ag regional prices (weighted average b/t domestic and imported prices)") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, region, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_import_vs_domestic_world <<- rgcam::getQuery(prj, "ag import vs. domestic supply (Regional Armington competition)") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, sector, subsector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(subsector = ifelse(substr(subsector, 1, 8) == "domestic", "domestic",
                              ifelse(substr(subsector, 1, 8) == "imported", "imported", NA)),
           sector = sub(".*\\s", "", sector)) %>%
    filter(scenario %in% selected_scen) %>%
    filter(!is.na(subsector)) %>%
    filter(year >= year_s, year <= year_e)

  ag_import_vs_domestic_regional <<- rgcam::getQuery(prj, "ag import vs. domestic supply (Regional Armington competition)") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(region, Units, scenario, sector, subsector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(subsector = ifelse(substr(subsector, 1, 8) == "domestic", "domestic",
                              ifelse(substr(subsector, 1, 8) == "imported", "imported", NA)),
           sector = sub(".*\\s", "", sector)) %>%
    filter(scenario %in% selected_scen) %>%
    filter(!is.na(subsector)) %>%
    filter(year >= year_s, year <= year_e)

  ag_meet_dairy_prices_world <<- rgcam::getQuery(prj, "meat and dairy prices") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_meet_dairy_prices_regional <<- rgcam::getQuery(prj, "meat and dairy prices") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, region, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_meet_dairy_production_world <<- rgcam::getQuery(prj, "meat and dairy production by tech") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, sector, subsector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_meet_dairy_production_regional <<- rgcam::getQuery(prj, "meat and dairy production by tech") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, sector, subsector, year, region) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  feed_consumption_world <<- rgcam::getQuery(prj, "feed consumption by region") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, Units, year, input) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  feed_consumption_regional <<- rgcam::getQuery(prj, "feed consumption by region") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(region, scenario, Units, year, input) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)


  ###### ==== water ====
  water_withdrawals_world <<- rgcam::getQuery(prj, "water withdrawals by region") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_withdrawals_regional <<- rgcam::getQuery(prj, "water withdrawals by region") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(region, scenario, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_consumption <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(Units, scenario, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_consumption_world <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(Units, scenario, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_consumption_regional <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(Units, scenario, year, region) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_consumption_regional_sectorial <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(scenario, region, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_irr_rfd_world <<- rgcam::getQuery(prj, "land allocation by crop and water source") %>%
    filter(!is.na(water)) %>%
    filter(scenario %in% selected_scen) %>%
    filter(!crop %in% c('biomassGrass','biomassTree')) %>%
    dplyr::mutate(crop = sub("C4$", "", crop)) %>%
    dplyr::mutate(crop = sub("Tree$", "", crop)) %>%
    group_by(Units, scenario, year, water, crop) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_irr_rfd_regional <<- rgcam::getQuery(prj, "land allocation by crop and water source") %>%
    filter(!is.na(water)) %>%
    filter(scenario %in% selected_scen) %>%
    filter(!crop %in% c('biomassGrass','biomassTree')) %>%
    dplyr::mutate(crop = sub("C4$", "", crop)) %>%
    dplyr::mutate(crop = sub("Tree$", "", crop)) %>%
    group_by(region, Units, scenario, year, water, crop) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)


  ###### ==== emissions ====
  GWP <<- readr::read_csv(paste0(inputs_path,"mappings/GWP_AR5.csv"))

  co2_emiss <<- getQuery(prj,"CO2 emissions by region") %>%
    group_by(scenario, region, year, Units) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(ghg = "CO2") %>%
    filter(year >= year_s, year <= year_e)

  luc <<- getQuery(prj,"LUC emissions by region") %>%
    group_by(Units, scenario, region, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(ghg = "LUC CO2",
           Units = "MTC") %>%
    filter(year >= year_s, year <= year_e)

  nonco2 <<- getQuery(prj,"nonCO2 emissions by region") %>%
    filter(year >= year_s, year <= year_e)

  nonco2_luc <<- getQuery(prj,"nonCO2 emissions by sector (excluding resource production)") %>%
    filter(year >= year_s, year <= year_e) %>%
    filter(sector %in% food_sector) %>%
    filter(ghg %in% c('CH4_AGR','N2O_AGR')) %>%
    mutate(ghg = ifelse(ghg == 'CH4_AGR','CH4','N2O')) %>%
    mutate(Units = 'Mt') #Tg is equivalent to Mt


  ###### ==== GHG emissions ====
  ghg_by_ghg_world <<- bind_rows(luc,co2_emiss,nonco2) %>%
    left_join(GWP, by = c("Units", "ghg")) %>%
    mutate(value = value * GWP,
           Units = "MtCO2e") %>%
    group_by(group, scenario, year, Units) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    filter(!is.na(group))

  ghg_by_ghg_regional <<- bind_rows(luc,co2_emiss,nonco2) %>%
    left_join(GWP, by = c("Units", "ghg")) %>%
    mutate(value = value * GWP,
           Units = "MtCO2e") %>%
    group_by(region, group, scenario, year, Units) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    filter(!is.na(group))

  ghg_regional <<- bind_rows(luc,co2_emiss,nonco2) %>%
    left_join(GWP, by = c("Units", "ghg")) %>%
    mutate(value = value * GWP,
           Units = "MtCO2e") %>%
    group_by(scenario, region, year, Units) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup()

  ghg_world <<- bind_rows(luc,co2_emiss,nonco2) %>%
    left_join(GWP, by = c("Units", "ghg")) %>%
    mutate(value = value * GWP,
           Units = "MtCO2e") %>%
    group_by(scenario, year, Units) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup()



  ###### ===== land use ======
  land_use_world <<- getQuery(prj,"aggregated land allocation") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, year, landleaf) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e) %>%
    mutate(land_use_type = ifelse(landleaf %in% c("forest (managed)", "forest (unmanaged)"), 'Forest',
                                  ifelse(landleaf %in% c('crops','biomass','otherarable'), 'Cropland',
                                         ifelse(landleaf %in% c("pasture (grazed)","pasture (other)"), 'Pasture',
                                                ifelse(landleaf %in% c("shrubs","grass"), 'Shrubs & Grass',
                                                       'Other Natural')))))

  land_use_regional <<- getQuery(prj,"aggregated land allocation") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, year, region, landleaf) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e) %>%
    mutate(land_use_type = ifelse(landleaf %in% c("forest (managed)", "forest (unmanaged)"), 'Forest',
                                  ifelse(landleaf %in% c('crops','biomass','otherarable'), 'Cropland',
                                         ifelse(landleaf %in% c("pasture (grazed)","pasture (other)"), 'Pasture',
                                                ifelse(landleaf %in% c("shrubs","grass"), 'Shrubs & Grass',
                                                       'Other Natural')))))

  land_crop_world <<- getQuery(prj,"land allocation by crop") %>%
    dplyr::mutate(landleaf = sub("C4$", "", landleaf)) %>%
    dplyr::mutate(landleaf = sub("Tree$", "", landleaf)) %>%
    group_by(Units, scenario, year, landleaf) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(scenario %in% selected_scen) %>%
    filter(year >= year_s, year <= year_e)

  land_crop_regional <<- getQuery(prj,"land allocation by crop") %>%
    dplyr::mutate(landleaf = sub("C4$", "", landleaf)) %>%
    dplyr::mutate(landleaf = sub("Tree$", "", landleaf)) %>%
    group_by(Units, scenario, region, year, landleaf) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(scenario %in% selected_scen) %>%
    filter(year >= year_s, year <= year_e)


  carbon_stock_world <<- getQuery(prj,"vegetative carbon stock by region") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, year, landleaf) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  carbon_stock_regional <<- getQuery(prj,"vegetative carbon stock by region") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, year, region, landleaf) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)


  fertilizer_consumption_world <<- getQuery(prj,"fertilizer consumption by crop type") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, year, sector) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  fertilizer_consumption_regional <<- getQuery(prj,"fertilizer consumption by crop type") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, year, region, sector) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)


  ###### ===== population ======
  # population by region
  pop_all_regions <<- getQuery(prj, "population by region") %>%
    filter(year >= year_s, year <= year_e) %>%
    mutate(value = value * 1000) %>% # Convert from thous ppl to total ppl
    select(-Units) %>%
    rename(population = value)


  ###### ===== nutritional basic data ======
  load_nutritional_data()

}


