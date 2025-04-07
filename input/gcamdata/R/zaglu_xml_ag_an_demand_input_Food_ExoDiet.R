# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_ag_an_demand_input_Food_ExoDiet_xml
#'
#' Construct XML data structure for \code{ag_an_demand_input.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_an_demand_input.xml}. The corresponding file in the
#' original data system was \code{batch_demand_input_xml.R} (aglu XML).
module_aglu_ag_an_demand_input_Food_ExoDiet_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "aglu/AgMIP/A_demand_supplysector_Food_ExoDiet",
      FILE = "aglu/AgMIP/A_demand_nesting_subsector_Food_ExoDiet",
      FILE = "aglu/AgMIP/A_demand_subsector_Food_ExoDiet",
      FILE = "aglu/AgMIP/A_demand_technology_Food_ExoDiet",
      "L101.CropMeat_Food_Pcal_R_C_Y",
      "L109.ag_ALL_Mt_R_C_Y",
      "L109.an_ALL_Mt_R_C_Y",
      "L100.AgMIP_FoodWaste_Share_Pathway_SSP",
      "L100.IncomeElasticity_Food_ExoDiet_2050EL2_SSP",
      "L100.IncomeElasticity_Food_ExoDiet_2100EL2_SSP",
      "L100.IncomeElasticity_Food_ExoDiet_Static_SSP"
    )

  MODULE_OUTPUTS <-
    c(XML = "ag_an_demand_input_Food_ExoDiet_SSP1_2050EL2.xml", # Diet 2050
      XML = "ag_an_demand_input_Food_ExoDiet_SSP1_2050EL2_HalfWaste2050.xml", # Diet Waste 2050,
      XML = "ag_an_demand_input_Food_ExoDiet_SSP1_2100EL2.xml", # Diet 2100
      XML = "ag_an_demand_input_Food_ExoDiet_SSP1_2100EL2_HalfWaste2100.xml", # Diet Waste 2100,
      XML = "ag_an_demand_input_Food_ExoDiet_SSP1_Static.xml", # Static,
      XML = "ag_an_demand_input_Food_ExoDiet_SSP1_RegHet_Diet.xml", # Diet - Regional Heterogeneity
      XML = "ag_an_demand_input_Food_ExoDiet_SSP1_RegHet_DietWaste.xml", # Diet Waste - Regional Heterogeneity,


      XML = "ag_an_demand_input_Food_ExoDiet_SSP2_2050EL2.xml",
      XML = "ag_an_demand_input_Food_ExoDiet_SSP2_2050EL2_HalfWaste.xml",
      "L203.StubTech_demand_Food_ExoDiet")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================

    #Region income groups
    REG_low_lowermiddle_income <- c("Africa_Eastern", "Pakistan", "Africa_Southern",
                                    "South Asia", "Africa_Western", "India",
                                    "Europe_Eastern", "Africa_Northern", "South America_Northern",
                                    "Southeast Asia", "Indonesia")

    REG_uppermiddle_income <- c("Central America and Caribbean", "Central Asia",
                                "South Africa", "South America_Southern", "Colombia",
                                "China", "Argentina", "Mexico", "Middle East", "Brazil",
                                "Russia")

    REG_high_income <- c("Europe_Non_EU", "EU-12", "Taiwan", "South Korea", "EU-15", "Japan",
                         "Canada", "USA", "Australia_NZ", "European Free Trade Association")

    # ===================================================

    # Get mass-calories conversion rates for food commodities----
    # Note that food consumption in Mt in L109 files should be finalized
    # So the conversion rates are finalized here (after any potential earlier food adjustments)
    L109.ag_ALL_Mt_R_C_Y %>%
      # Combine the balance tables of crop and meat in Mt
      bind_rows(L109.an_ALL_Mt_R_C_Y) %>%
      select(GCAM_region_ID, GCAM_commodity, year, Food_Mt) %>%
      # keep food commodities only
      inner_join(L101.CropMeat_Food_Pcal_R_C_Y%>% distinct(GCAM_commodity),
                 by = "GCAM_commodity") %>%
      left_join_error_no_match(L101.CropMeat_Food_Pcal_R_C_Y %>% rename(Pcal = value),
                               by = c("GCAM_region_ID", "GCAM_commodity", "year")) ->
      L101.CropMeat_Food_kcalg_R_C_Y_1

    L101.CropMeat_Food_kcalg_R_C_Y_1 %>%
      dplyr::group_by_at(vars(-GCAM_region_ID, -Food_Mt, -Pcal)) %>%
      summarise_at(.vars = vars(Food_Mt, Pcal), sum) %>%
      mutate(value_world = Pcal / Food_Mt) %>%
      select(-Food_Mt, -Pcal)->
      L101.CropMeat_Food_kcalg_R_C_Y_1_World

    L101.CropMeat_Food_kcalg_R_C_Y_1 %>%
      left_join_error_no_match(L101.CropMeat_Food_kcalg_R_C_Y_1_World,
                               by = c("GCAM_commodity", "year")) %>%
      mutate(value = if_else(Food_Mt == 0, value_world,
                             Pcal / Food_Mt)) %>%
      select(-Food_Mt, -Pcal, -value_world) %>%
      filter(year %in% MODEL_BASE_YEARS) ->
      L101.CropMeat_Food_kcalg_R_C_Y


    # Build L203.Supplysector_demand: generic info for demand sectors by region
    A_demand_supplysector_Food_ExoDiet %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) -> # Remove any regions for which agriculture and land use are not modeled
      L203.Supplysector_demand_Food_ExoDiet

    # Build L203.NestingSubsectorAll_demand_food: generic info for food demand nesting subsectors by region
    # Filter for food demand since we only add extra nest to food sectors
    A_demand_nesting_subsector_Food_ExoDiet %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorAll"]], LOGIT_TYPE_COLNAME), GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) -> # Remove any regions for which agriculture and land use are not modeled
      L203.NestingSubsectorAll_demand_Food_ExoDiet

    # Build L203.SubsectorAll_demand: generic info for demand subsectors by region
    A_demand_subsector_Food_ExoDiet %>%
      write_to_all_regions(c(c(LEVEL2_DATA_NAMES[["SubsectorAll"]], "subsector0"), LOGIT_TYPE_COLNAME), GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) -> # Remove any regions for which agriculture and land use are not modeled
      L203.SubsectorAll_demand_Food_ExoDiet

    # Build L203.StubTech_demand: identification of stub technologies for demands by region
    A_demand_technology_Food_ExoDiet %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]], "subsector0"), GCAM_region_names = GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) -> # Remove any regions for which agriculture and land use are not modeled
      L203.StubTech_demand_Food_ExoDiet

    # Build L203.GlobalTechCoef_demand: input names of demand technologies
    A_demand_technology_Food_ExoDiet %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechCoef"]])) ->
      L203.GlobalTechCoef_demand_Food_ExoDiet

    # Build L203.GlobalTechShrwt_demand: shareweights of demand technologies
    L203.GlobalTechCoef_demand_Food_ExoDiet %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]]) %>%
      mutate(share.weight = 1) ->
      L203.GlobalTechShrwt_demand_Food_ExoDiet

    # Build L203.GlobalTechInterp_demand: Interpolation rule to fix initial shareweights
    A_demand_technology_Food_ExoDiet %>%
      filter(subsector != technology) %>%
      mutate(from.year = MODEL_FINAL_BASE_YEAR,
             to.year = max(MODEL_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechInterp"]])-> L203.GlobalTechInterp_demand_Food_ExoDiet



    # Calibrated staple and non-staple demands of crops and meat ----
    # Create table of regions, technologies and all base years
    # NOTE: Easiest if the model base years are subsetted from a full table as a last step in the construction of each of these tables
    A_demand_technology_Food_ExoDiet %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]], "minicam.energy.input", "market.name", "subsector0"), GCAM_region_names = GCAM_region_names) %>%
      mutate(market.name = region, stub.technology = technology) ->
      A_demand_technology_R_Food_ExoDiet
    # Add all base years
    A_demand_technology_R_Food_ExoDiet %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) ->
      A_demand_technology_R_Yh_Food_ExoDiet
    # Add all model years
    A_demand_technology_R_Food_ExoDiet %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      A_demand_technology_R_Y_Food_ExoDiet


    # Build L203.StubTechProd_food: crop and meat food supply by technology and region
    L101.CropMeat_Food_Pcal_R_C_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.ag_an_Food_Pcal_R_C_Y

    A_demand_technology_R_Yh_Food_ExoDiet %>%
      filter(grepl("^FoodDemand", supplysector)) %>%
      # Select food demand
      #filter(supplysector %in% c("FoodDemand_Staples", "FoodDemand_NonStaples")) %>%
      # Map in food demand by region / commodity / year
      left_join_error_no_match(L203.ag_an_Food_Pcal_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(calOutputValue = round(value, aglu.DIGITS_CALOUTPUT),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs0.share.weight = if_else(calOutputValue > 0, 1, 0), # NESTING SUBSECTOR SWS??????
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(c(LEVEL2_DATA_NAMES[["StubTechProd"]]), subsector0, subs0.share.weight) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%           # Remove any regions for which agriculture and land use are not modeled
      filter(year %in% MODEL_BASE_YEARS) ->                         # Also subset the calibration tables to only the model base years
      L203.StubTechProd_food_Food_ExoDiet

    # Build L203.StubCalorieContent: ----
    # calorie content of food crops (incl secondary products) and meat commodities
    L101.CropMeat_Food_kcalg_R_C_Y %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.ag_an_kcalg_R_C_Y

    A_demand_technology_R_Y_Food_ExoDiet %>%
      filter(grepl("^FoodDemand", supplysector)) %>%
      #filter(supplysector %in% c("FoodDemand_Staples", "FoodDemand_NonStaples")) %>%
      # Create NAs for future years, use left_join instead
      left_join(L203.ag_an_kcalg_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(efficiency = round(value, aglu.DIGITS_CALOUTPUT)) %>%
      # For each region / commodity,
      group_by(region, subsector0, subsector, technology) %>%
      # Calorie content are held constant in the future, so set value for future years at the final base year value
      mutate(efficiency = replace(efficiency, year > max(MODEL_BASE_YEARS), efficiency[year == max(MODEL_BASE_YEARS)])) %>%
      ungroup() %>%
      select(c(LEVEL2_DATA_NAMES[["StubTechCalorieContent"]], "subsector0")) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->          # Remove any regions for which agriculture and land use are not modeled
      L203.StubCalorieContent_Food_ExoDiet

    # FINAL DEMANDS ----

    # Base service exo food ----

    L203.Demand_Food_ExoDiet <- L203.StubTechProd_food_Food_ExoDiet %>%
      group_by(region, supplysector, year) %>%
      summarise(base.service = sum(calOutputValue)) %>%
      ungroup()

    L203.Demand_Food_ExoDiet %>%
      rename(energy.final.demand = supplysector)%>%
      select(LEVEL2_DATA_NAMES[["BaseService"]]) ->
      L203.BaseService_Food_ExoDiet

    # Build L203.PerCapitaBased: per-capita final demand attributes that do not vary by time period
    A_demand_supplysector_Food_ExoDiet %>%
      filter(!is.na(energy.final.demand)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]], GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->          # Remove any regions for which agriculture and land use are not modeled
      L203.PerCapitaBased_Food_ExoDiet

    A_demand_supplysector_Food_ExoDiet %>%
      filter(!is.na(energy.final.demand)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PriceElasticity"]], GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->          # Remove any regions for which agriculture and land use are not modeled
      L203.PriceElasticity_Food_ExoDiet

    A_demand_supplysector_Food_ExoDiet %>%
      filter(!is.na(energy.final.demand)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["IncomeElasticity"]], GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->          # Remove any regions for which agriculture and land use are not modeled
      L203.IncomeElasticity_Food_ExoDiet
    # We assumed a default of 0.1 income.elasticity;
    # But dynamic path is generated to target 2050EL2

    # *****************---------
    # Income elasticity scenarios ----

    # Note that PerCapitaBased is turned off so that future changes in diet will be pure income elast. driven
    # also 2020 was based on BAU

    # SSP1 ----
    # 2050 EL2 ----
    L203.IncomeElasticity_Food_ExoDiet_2050EL2 <-
      L100.IncomeElasticity_Food_ExoDiet_2050EL2_SSP %>%
      filter(scenario == "gSSP1") %>% select(-scenario)

    # assure sector names are the identical
    assertthat::assert_that(
      dplyr::setdiff(L203.IncomeElasticity_Food_ExoDiet_2050EL2 %>%
                       distinct(region, energy.final.demand, year),
                     L203.IncomeElasticity_Food_ExoDiet %>%
                       distinct(region, energy.final.demand, year)) %>% nrow ==0
    )

    # SSP1 ----
    # 2100 EL2 ----
    L203.IncomeElasticity_Food_ExoDiet_2100EL2 <-
      L100.IncomeElasticity_Food_ExoDiet_2100EL2_SSP %>%
      filter(scenario == "gSSP1") %>% select(-scenario)

    # assure sector names are the identical
    assertthat::assert_that(
      dplyr::setdiff(L203.IncomeElasticity_Food_ExoDiet_2100EL2 %>%
                       distinct(region, energy.final.demand, year),
                     L203.IncomeElasticity_Food_ExoDiet %>%
                       distinct(region, energy.final.demand, year)) %>% nrow ==0
    )

    # SSP1 ----
    # Static ----
    L203.IncomeElasticity_Food_ExoDiet_Static <-
      L100.IncomeElasticity_Food_ExoDiet_Static_SSP %>%
      filter(scenario == "gSSP1") %>% select(-scenario)

    # assure sector names are the identical
    assertthat::assert_that(
      dplyr::setdiff(L203.IncomeElasticity_Food_ExoDiet_Static %>%
                       distinct(region, energy.final.demand, year),
                     L203.IncomeElasticity_Food_ExoDiet_Static %>%
                       distinct(region, energy.final.demand, year)) %>% nrow ==0
    )
    #Regional heterogeneity income elasticity
    # Low/lower-middle income regions: static
    # Upper-middle income regions: converge to EL2 by 2100
    # High income regions: converge to EL2 by 2050
    L203.IncomeElasticity_Food_ExoDiet_Static_low_income <- L203.IncomeElasticity_Food_ExoDiet_Static %>%
      filter(region %in% REG_low_lowermiddle_income)

    L203.IncomeElasticity_Food_ExoDiet_2100EL2_middle_income <- L203.IncomeElasticity_Food_ExoDiet_2100EL2 %>%
      filter(region %in% REG_uppermiddle_income)

    L203.IncomeElasticity_Food_ExoDiet_2050EL2_high_income <- L203.IncomeElasticity_Food_ExoDiet_2050EL2 %>%
      filter(region %in% REG_high_income)

    L203.IncomeElasticity_Food_ExoDiet_reg_het <- bind_rows(L203.IncomeElasticity_Food_ExoDiet_Static_low_income,
                                                            L203.IncomeElasticity_Food_ExoDiet_2100EL2_middle_income,
                                                            L203.IncomeElasticity_Food_ExoDiet_2050EL2_high_income)

    # adding waste pathways ----

    L203.StubCalorieContent_Food_ExoDiet %>%
      left_join(
        L100.AgMIP_FoodWaste_Share_Pathway_SSP %>% filter(scenario == "gSSP1") %>%
          left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
          transmute(region, subsector = GCAM_commodity, year, WasteShare) %>%
          mutate(NonWasteShare = (1 - WasteShare) ) %>%
          group_by(region, subsector) %>%
          # 2015 was the model base year when efficiency was defined
          mutate(WasteScaler = NonWasteShare / NonWasteShare[year == 2015]) %>%
          ungroup %>% select(-WasteShare, -NonWasteShare),
        by = c("region", "subsector", "year")
      ) %>%
      replace_na(list(WasteScaler = 1)) %>%
      mutate(efficiency = WasteScaler * efficiency) %>%
      select(names(L203.StubCalorieContent_Food_ExoDiet)) ->
      L203.StubCalorieContent_Food_ExoDiet_WasteTrend

    L203.StubCalorieContent_Food_ExoDiet %>%
      left_join(
        L100.AgMIP_FoodWaste_Share_Pathway_SSP %>% filter(scenario == "gSSP1") %>%
          left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
          transmute(region, subsector = GCAM_commodity, year, WasteShare = StaticWaste) %>%
          mutate(NonWasteShare = (1 - WasteShare) ) %>%
          group_by(region, subsector) %>%
          # 2015 was the model base year when efficiency was defined
          mutate(WasteScaler = NonWasteShare / NonWasteShare[year == 2015]) %>%
          ungroup %>% select(-WasteShare, -NonWasteShare),
        by = c("region", "subsector", "year")
      ) %>%
      replace_na(list(WasteScaler = 1)) %>%
      mutate(efficiency = WasteScaler * efficiency) %>%
      select(names(L203.StubCalorieContent_Food_ExoDiet)) ->
      L203.StubCalorieContent_Food_ExoDiet_StaticWaste


    L203.StubCalorieContent_Food_ExoDiet %>%
      left_join(
        L100.AgMIP_FoodWaste_Share_Pathway_SSP %>% filter(scenario == "gSSP1") %>%
          left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
          transmute(region, subsector = GCAM_commodity, year, WasteShare = HalfWaste2050) %>%
          mutate(NonWasteShare = (1 - WasteShare) ) %>%
          group_by(region, subsector) %>%
          # 2015 was the model base year when efficiency was defined
          mutate(WasteScaler = NonWasteShare / NonWasteShare[year == 2015]) %>%
          ungroup %>% select(-WasteShare, -NonWasteShare),
        by = c("region", "subsector", "year")
      ) %>%
      replace_na(list(WasteScaler = 1)) %>%
      mutate(efficiency = WasteScaler * efficiency) %>%
      select(names(L203.StubCalorieContent_Food_ExoDiet)) ->
      L203.StubCalorieContent_Food_ExoDiet_HalfWaste2050

    L203.StubCalorieContent_Food_ExoDiet %>%
      left_join(
        L100.AgMIP_FoodWaste_Share_Pathway_SSP %>% filter(scenario == "gSSP1") %>%
          left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
          transmute(region, subsector = GCAM_commodity, year, WasteShare = HalfWaste2100) %>%
          mutate(NonWasteShare = (1 - WasteShare) ) %>%
          group_by(region, subsector) %>%
          # 2015 was the model base year when efficiency was defined
          mutate(WasteScaler = NonWasteShare / NonWasteShare[year == 2015]) %>%
          ungroup %>% select(-WasteShare, -NonWasteShare),
        by = c("region", "subsector", "year")
      ) %>%
      replace_na(list(WasteScaler = 1)) %>%
      mutate(efficiency = WasteScaler * efficiency) %>%
      select(names(L203.StubCalorieContent_Food_ExoDiet)) ->
      L203.StubCalorieContent_Food_ExoDiet_HalfWaste2100


    #Regional heterogeneity
    # Low/lower-middle income regions: static waste reduction
    # Upper-middle income regions: halve waste by 2100
    # High income regions: halve waste by 2050
    L203.StubCalorieContent_Food_ExoDiet_StaticWaste_low_income <-  L203.StubCalorieContent_Food_ExoDiet_StaticWaste %>%
      filter(region %in% REG_low_lowermiddle_income)

    L203.StubCalorieContent_Food_ExoDiet_HalfWaste2100_middle_income <- L203.StubCalorieContent_Food_ExoDiet_HalfWaste2100 %>%
      filter(region %in% REG_uppermiddle_income)

    L203.StubCalorieContent_Food_ExoDiet_HalfWaste2100_high_income <- L203.StubCalorieContent_Food_ExoDiet_HalfWaste2050 %>%
      filter(region %in% REG_high_income)

    L203.StubCalorieContent_Food_ExoDiet_reg_het <- bind_rows(L203.StubCalorieContent_Food_ExoDiet_StaticWaste_low_income,
                                                              L203.StubCalorieContent_Food_ExoDiet_HalfWaste2100_middle_income,
                                                              L203.StubCalorieContent_Food_ExoDiet_HalfWaste2100_high_income)

    # Produce outputs ----

    #Diet 2050
    create_xml("ag_an_demand_input_Food_ExoDiet_SSP1_2050EL2.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand_Food_ExoDiet, "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_Food_ExoDiet,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_Food_ExoDiet, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food_Food_ExoDiet, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent_Food_ExoDiet_WasteTrend, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_Food_ExoDiet, "SubsectorAll", "SubsectorLogit") %>%

      add_xml_data(L203.GlobalTechCoef_demand_Food_ExoDiet, "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand_Food_ExoDiet, "GlobalTechShrwt") %>%
      #add_xml_data(L203.GlobalTechInterp_demand_Food_ExoDiet, "GlobalTechInterp") %>%

      add_xml_data(L203.IncomeElasticity_Food_ExoDiet_2050EL2, "IncomeElasticity") %>%
      add_xml_data(L203.PriceElasticity_Food_ExoDiet, "PriceElasticity") %>%
      add_xml_data(L203.PerCapitaBased_Food_ExoDiet, "PerCapitaBased") %>%
      add_xml_data(L203.BaseService_Food_ExoDiet, "BaseService") %>%
      remove_regions_xml(c('EU-12','EU-15'), inverse = T) ->
      ag_an_demand_input_Food_ExoDiet_SSP1_2050EL2.xml

    #Diet 2100
    create_xml("ag_an_demand_input_Food_ExoDiet_SSP1_2100EL2.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand_Food_ExoDiet, "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_Food_ExoDiet,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_Food_ExoDiet, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food_Food_ExoDiet, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent_Food_ExoDiet_WasteTrend, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_Food_ExoDiet, "SubsectorAll", "SubsectorLogit") %>%

      add_xml_data(L203.GlobalTechCoef_demand_Food_ExoDiet, "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand_Food_ExoDiet, "GlobalTechShrwt") %>%
      #add_xml_data(L203.GlobalTechInterp_demand_Food_ExoDiet, "GlobalTechInterp") %>%

      add_xml_data(L203.IncomeElasticity_Food_ExoDiet_2100EL2, "IncomeElasticity") %>%
      add_xml_data(L203.PriceElasticity_Food_ExoDiet, "PriceElasticity") %>%
      add_xml_data(L203.PerCapitaBased_Food_ExoDiet, "PerCapitaBased") %>%
      add_xml_data(L203.BaseService_Food_ExoDiet, "BaseService") %>%
      remove_regions_xml(c('EU-12','EU-15'), inverse = T) ->
      ag_an_demand_input_Food_ExoDiet_SSP1_2100EL2.xml

    #Static - updated with static waste
    create_xml("ag_an_demand_input_Food_ExoDiet_SSP1_Static.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand_Food_ExoDiet, "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_Food_ExoDiet,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_Food_ExoDiet, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food_Food_ExoDiet, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent_Food_ExoDiet_StaticWaste, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_Food_ExoDiet, "SubsectorAll", "SubsectorLogit") %>%

      add_xml_data(L203.GlobalTechCoef_demand_Food_ExoDiet, "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand_Food_ExoDiet, "GlobalTechShrwt") %>%
      #add_xml_data(L203.GlobalTechInterp_demand_Food_ExoDiet, "GlobalTechInterp") %>%

      add_xml_data(L203.IncomeElasticity_Food_ExoDiet_Static, "IncomeElasticity") %>%
      add_xml_data(L203.PriceElasticity_Food_ExoDiet, "PriceElasticity") %>%
      add_xml_data(L203.PerCapitaBased_Food_ExoDiet, "PerCapitaBased") %>%
      add_xml_data(L203.BaseService_Food_ExoDiet, "BaseService") %>%
      remove_regions_xml(c('EU-12','EU-15'), inverse = T) ->
      ag_an_demand_input_Food_ExoDiet_SSP1_Static.xml

    # Reg Het Diet
    create_xml("ag_an_demand_input_Food_ExoDiet_SSP1_RegHet_Diet.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand_Food_ExoDiet, "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_Food_ExoDiet,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_Food_ExoDiet, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food_Food_ExoDiet, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent_Food_ExoDiet_reg_het, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_Food_ExoDiet, "SubsectorAll", "SubsectorLogit") %>%

      add_xml_data(L203.GlobalTechCoef_demand_Food_ExoDiet, "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand_Food_ExoDiet, "GlobalTechShrwt") %>%
      #add_xml_data(L203.GlobalTechInterp_demand_Food_ExoDiet, "GlobalTechInterp") %>%

      add_xml_data(L203.IncomeElasticity_Food_ExoDiet_reg_het, "IncomeElasticity") %>%
      add_xml_data(L203.PriceElasticity_Food_ExoDiet, "PriceElasticity") %>%
      add_xml_data(L203.PerCapitaBased_Food_ExoDiet, "PerCapitaBased") %>%
      add_xml_data(L203.BaseService_Food_ExoDiet, "BaseService") %>%
      remove_regions_xml(c('EU-12','EU-15'), inverse = T) ->
      ag_an_demand_input_Food_ExoDiet_SSP1_RegHet_Diet.xml

    #Diet Waste 2050
    create_xml("ag_an_demand_input_Food_ExoDiet_SSP1_2050EL2_HalfWaste2050.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand_Food_ExoDiet, "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_Food_ExoDiet,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_Food_ExoDiet, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food_Food_ExoDiet, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent_Food_ExoDiet_HalfWaste2050, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_Food_ExoDiet, "SubsectorAll", "SubsectorLogit") %>%

      add_xml_data(L203.GlobalTechCoef_demand_Food_ExoDiet, "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand_Food_ExoDiet, "GlobalTechShrwt") %>%
      #add_xml_data(L203.GlobalTechInterp_demand_Food_ExoDiet, "GlobalTechInterp") %>%

      add_xml_data(L203.IncomeElasticity_Food_ExoDiet_2050EL2, "IncomeElasticity") %>%
      add_xml_data(L203.PriceElasticity_Food_ExoDiet, "PriceElasticity") %>%
      add_xml_data(L203.PerCapitaBased_Food_ExoDiet, "PerCapitaBased") %>%
      add_xml_data(L203.BaseService_Food_ExoDiet, "BaseService") %>%
      remove_regions_xml(c('EU-12','EU-15'), inverse = T) ->
      ag_an_demand_input_Food_ExoDiet_SSP1_2050EL2_HalfWaste2050.xml

    #Diet Waste 2100
    create_xml("ag_an_demand_input_Food_ExoDiet_SSP1_2100EL2_HalfWaste2100.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand_Food_ExoDiet, "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_Food_ExoDiet,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_Food_ExoDiet, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food_Food_ExoDiet, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent_Food_ExoDiet_HalfWaste2100, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_Food_ExoDiet, "SubsectorAll", "SubsectorLogit") %>%

      add_xml_data(L203.GlobalTechCoef_demand_Food_ExoDiet, "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand_Food_ExoDiet, "GlobalTechShrwt") %>%
      #add_xml_data(L203.GlobalTechInterp_demand_Food_ExoDiet, "GlobalTechInterp") %>%

      add_xml_data(L203.IncomeElasticity_Food_ExoDiet_2100EL2, "IncomeElasticity") %>%
      add_xml_data(L203.PriceElasticity_Food_ExoDiet, "PriceElasticity") %>%
      add_xml_data(L203.PerCapitaBased_Food_ExoDiet, "PerCapitaBased") %>%
      add_xml_data(L203.BaseService_Food_ExoDiet, "BaseService") %>%
      remove_regions_xml(c('EU-12','EU-15'), inverse = T) ->
      ag_an_demand_input_Food_ExoDiet_SSP1_2100EL2_HalfWaste2100.xml

    # Reg Het Diet Waste
    create_xml("ag_an_demand_input_Food_ExoDiet_SSP1_RegHet_DietWaste.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand_Food_ExoDiet, "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_Food_ExoDiet,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_Food_ExoDiet, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food_Food_ExoDiet, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent_Food_ExoDiet_reg_het, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_Food_ExoDiet, "SubsectorAll", "SubsectorLogit") %>%

      add_xml_data(L203.GlobalTechCoef_demand_Food_ExoDiet, "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand_Food_ExoDiet, "GlobalTechShrwt") %>%
      #add_xml_data(L203.GlobalTechInterp_demand_Food_ExoDiet, "GlobalTechInterp") %>%

      add_xml_data(L203.IncomeElasticity_Food_ExoDiet_reg_het, "IncomeElasticity") %>%
      add_xml_data(L203.PriceElasticity_Food_ExoDiet, "PriceElasticity") %>%
      add_xml_data(L203.PerCapitaBased_Food_ExoDiet, "PerCapitaBased") %>%
      add_xml_data(L203.BaseService_Food_ExoDiet, "BaseService") %>%
      remove_regions_xml(c('EU-12','EU-15'), inverse = T) ->
      ag_an_demand_input_Food_ExoDiet_SSP1_RegHet_DietWaste.xml

    L203.IncomeElasticity_Food_ExoDiet_2050EL2 <-
      L100.IncomeElasticity_Food_ExoDiet_2050EL2_SSP %>%
      filter(scenario == "gSSP2") %>% select(-scenario)

    # assure sector names are the identical
    assertthat::assert_that(
      dplyr::setdiff(L203.IncomeElasticity_Food_ExoDiet_2050EL2 %>%
                       distinct(region, energy.final.demand, year),
                     L203.IncomeElasticity_Food_ExoDiet %>%
                       distinct(region, energy.final.demand, year)) %>% nrow ==0
    )

    # adding waste pathways ----

    L203.StubCalorieContent_Food_ExoDiet %>%
      left_join(
        L100.AgMIP_FoodWaste_Share_Pathway_SSP %>% filter(scenario == "gSSP2") %>%
          left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
          transmute(region, subsector = GCAM_commodity, year, WasteShare) %>%
          mutate(WasteScaler = (1 - WasteShare) ) %>%
          group_by(region, subsector) %>%
          mutate(WasteScaler = WasteScaler / WasteScaler[year == 2020]) %>%
          ungroup %>% select(-WasteShare),
        by = c("region", "subsector", "year")
      ) %>%
      replace_na(list(WasteScaler = 1)) %>%
      mutate(efficiency = WasteScaler * efficiency) %>%
      select(names(L203.StubCalorieContent_Food_ExoDiet)) ->
      L203.StubCalorieContent_Food_ExoDiet_WasteTrend

    L203.StubCalorieContent_Food_ExoDiet %>%
      left_join(
        L100.AgMIP_FoodWaste_Share_Pathway_SSP %>% filter(scenario == "gSSP2") %>%
          left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
          transmute(region, subsector = GCAM_commodity, year, WasteShare = HalfWaste2050) %>%
          mutate(WasteScaler = (1 - WasteShare) ) %>%
          group_by(region, subsector) %>%
          mutate(WasteScaler = WasteScaler / WasteScaler[year == 2020]) %>%
          ungroup %>% select(-WasteShare),
        by = c("region", "subsector", "year")
      ) %>%
      replace_na(list(WasteScaler = 1)) %>%
      mutate(efficiency = WasteScaler * efficiency) %>%
      select(names(L203.StubCalorieContent_Food_ExoDiet)) ->
      L203.StubCalorieContent_Food_ExoDiet_HalfWaste2050

    # Produce outputs ----

    create_xml("ag_an_demand_input_Food_ExoDiet_SSP2_2050EL2.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand_Food_ExoDiet, "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_Food_ExoDiet,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_Food_ExoDiet, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food_Food_ExoDiet, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent_Food_ExoDiet_WasteTrend, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_Food_ExoDiet, "SubsectorAll", "SubsectorLogit") %>%

      add_xml_data(L203.GlobalTechCoef_demand_Food_ExoDiet, "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand_Food_ExoDiet, "GlobalTechShrwt") %>%
      #add_xml_data(L203.GlobalTechInterp_demand_Food_ExoDiet, "GlobalTechInterp") %>%

      add_xml_data(L203.IncomeElasticity_Food_ExoDiet_2050EL2, "IncomeElasticity") %>%
      add_xml_data(L203.PriceElasticity_Food_ExoDiet, "PriceElasticity") %>%
      add_xml_data(L203.PerCapitaBased_Food_ExoDiet, "PerCapitaBased") %>%
      add_xml_data(L203.BaseService_Food_ExoDiet, "BaseService") %>%
      remove_regions_xml(c('EU-12','EU-15'), inverse = T) ->
      ag_an_demand_input_Food_ExoDiet_SSP2_2050EL2.xml


    create_xml("ag_an_demand_input_Food_ExoDiet_SSP2_2050EL2_HalfWaste.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand_Food_ExoDiet, "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_Food_ExoDiet,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_Food_ExoDiet, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food_Food_ExoDiet, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent_Food_ExoDiet_HalfWaste2050, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_Food_ExoDiet, "SubsectorAll", "SubsectorLogit") %>%

      add_xml_data(L203.GlobalTechCoef_demand_Food_ExoDiet, "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand_Food_ExoDiet, "GlobalTechShrwt") %>%
      #add_xml_data(L203.GlobalTechInterp_demand_Food_ExoDiet, "GlobalTechInterp") %>%

      add_xml_data(L203.IncomeElasticity_Food_ExoDiet_2050EL2, "IncomeElasticity") %>%
      add_xml_data(L203.PriceElasticity_Food_ExoDiet, "PriceElasticity") %>%
      add_xml_data(L203.PerCapitaBased_Food_ExoDiet, "PerCapitaBased") %>%
      add_xml_data(L203.BaseService_Food_ExoDiet, "BaseService")  %>%
      remove_regions_xml(c('EU-12','EU-15'), inverse = T)->
      ag_an_demand_input_Food_ExoDiet_SSP2_2050EL2_HalfWaste.xml






    # export file for updating sectors in Food processing xmls ----
    L203.StubTech_demand_Food_ExoDiet %>%
      add_title("Food demand structure specified for exogenous diet") %>%
      add_comments("This data structure will be passed to food proc module to update xml") %>%
      add_units("NA") %>%
      add_legacy_name("L203.StubTech_demand_Food_ExoDiet") %>%
      add_precursors(MODULE_INPUTS) ->
      L203.StubTech_demand_Food_ExoDiet


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}

