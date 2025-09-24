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
    c(# Step 1 files for generating dietary change scenario (elasticity - driven)
      FILE = "aglu/AgMIP/GCAM_Near_Term_Intake_Reference",
      FILE = "aglu/AgMIP/AgMIP_EL2_intake_targets_foodgroup_r",
      FILE = "aglu/AgMIP/GCAM_AgMIP_food_group_mapping",
      FILE = "common/GCAM_region_names",
      "FAO_Food_Macronutrient_All",
      "L101.Pop_thous_R_Yh",
      "L101.Pop_thous_SSP_R_Yfut",
      "L102.gdp_mil90usd_Scen_R_Y",
      # Step 2 input files for base xml generation
      FILE = "aglu/AgMIP/A_demand_supplysector_Food_ExoDiet",
      FILE = "aglu/AgMIP/A_demand_nesting_subsector_Food_ExoDiet",
      FILE = "aglu/AgMIP/A_demand_subsector_Food_ExoDiet",
      FILE = "aglu/AgMIP/A_demand_technology_Food_ExoDiet",
      "L101.CropMeat_Food_Pcal_R_C_Y",
      "L109.ag_ALL_Mt_R_C_Y",
      "L109.an_ALL_Mt_R_C_Y",
      # food proc linkage moved from module_energy_food_processing_xml
      "L2328.StubCalorieContent",
      "L2328.StubCaloriePriceConv",
      # waste pathways
      "L100.AgMIP_FoodWaste_Share_Pathway_SSP")

  MODULE_OUTPUTS <-
    c(XML = "ag_an_demand_input_Food_ExoDiet_SSP1_VLLO.xml",
      XML = "ag_an_demand_input_Food_ExoDiet_SSP1_VLHO.xml",
      XML = "ag_an_demand_input_Food_ExoDiet_SSP2_VLLO.xml",
      XML = "ag_an_demand_input_Food_ExoDiet_SSP2_VLHO.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)
    cur_env <- environment()


    # Note that we will calculate total Pcal by food group, which will be exogenously driven
    # by population and "income elasticity" to meet EL2 targets.
    # Since we have population, we can first derive EL2 total Pcal by food group



    # Step 1. Derive GCAM dietary change pathways EL2 ----

    # A. Get GCAM base year (2021)/AgMIP calibration year (2020) intake ----

    # instead of using
    # EL2_Base_Year <- 2020
    # MODEL_FINAL_BASE_YEAR (2021) is close enough to 2020
    # We will use MODEL_FINAL_BASE_YEAR for GCAM

    ## Adding NEC to L101.CropMeat_Food_Pcal_R_C_Y (GCAM base year intake) ----
    # NEC is not endogenous to GCAM; we only want to check here
    # but NEC is supply not intake? Does this matter? Not here as it is added only for awareness

    ## Pull GCAM intake values including NEC in Pcal (and kcal/ca/d)
    FAO_Food_Macronutrient_All %>%
      filter(year %in% aglu.MODEL_MACRONUTRIENT_YEARS) %>%
      # Aggregate to region and GCAM commodity
      dplyr::group_by_at(vars(GCAM_region_ID, GCAM_commodity, year, macronutrient)) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      # Mean over aglu.MODEL_MACRONUTRIENT_YEARS
      dplyr::group_by_at(vars(GCAM_region_ID, GCAM_commodity, macronutrient)) %>%
      summarise(value = mean(value), .groups = "drop") %>%
      spread(macronutrient, value) ->
      DF_Macronutrient_FoodItem1

    ## Adding NEC to L101.CropMeat_Food_Pcal_R_C_Y
    L101.CropMeat_Food_Pcal_R_C_Y %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      bind_rows(
        DF_Macronutrient_FoodItem1 %>%
          filter(GCAM_commodity == "NEC") %>%
          transmute(GCAM_region_ID, GCAM_commodity,
                    year = MODEL_FINAL_BASE_YEAR,
                    value = MKcal/10^6) # convert to Pcal; # calorie was not Cal/kilocalorie; just small calorie
        ) ->
      GCAM_BaseData_Diet_Intake_NEC

    GCAM_AgMIP_food_group_mapping %>%
      distinct(GCAM_food_agg) %>% pull %>% c(.,"NEC") ->
      COMM_GCAM_food_agg

    ## Convert PCal to kcal/ca/d ----
    GCAM_BaseData_Diet_Intake_NEC %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(
        L101.Pop_thous_R_Yh %>%
          filter(year == MODEL_FINAL_BASE_YEAR) %>%
          rename(totalPop = value),
        by = c("year", "GCAM_region_ID")) %>%
      mutate(value = value / totalPop * 10^9 /365) %>%
      left_join(
        GCAM_AgMIP_food_group_mapping %>%
          distinct(GCAM_commodity = GCAM_food_commodities, GCAM_food_agg), by = "GCAM_commodity") %>%
      # NA to NEC
      mutate(GCAM_food_agg = if_else(is.na(GCAM_food_agg), GCAM_commodity, GCAM_food_agg)) %>%
      group_by_at(vars(-GCAM_commodity, -value)) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      mutate(GCAM_food_agg = factor(GCAM_food_agg,
                                    levels = rev(COMM_GCAM_food_agg))) %>%
      select(-totalPop) %>%
      mutate(measure = "GCAM_2021_Intake") %>%
      select(-year, -region) ->
      GCAM_BaseData_Diet_Intake_NEC1

    # Note that Fish in EL2 mapping is indeed Other meat and fish
    GCAM_BaseData_Diet_Intake_NEC1 %>%
      rename(sector = GCAM_food_agg) %>%
      mutate(sector = as.character(sector)) %>%
      mutate(sector = replace(sector, sector == "OtherMeat_Fish", "Fish")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")  ->
      GCAM_BaseData_Diet_Intake_NEC1

    # B Get near term targets (e.g., GCAM 2025/2030) ----
    GCAM_Near_Term_Intake_Reference %>%
      gather_years() %>%
      filter(year %in% c(2025)) %>%
      rename(GCAM_commodity = technology) %>%
      #distinct(GCAM_commodity) %>% pull
      left_join(
        GCAM_AgMIP_food_group_mapping %>%
          distinct(GCAM_commodity = GCAM_food_commodities, sector = GCAM_food_agg),
        by = "GCAM_commodity") %>%
      select(-scenario) %>%
      group_by_at(vars(-value, -GCAM_commodity)) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      left_join_error_no_match(GCAM_region_names, by = c("region")) %>%
      mutate(sector = replace(sector, sector == "OtherMeat_Fish", "Fish") ) ->
      GCAM_Near_Term_Intake_Reference_Agg_Pcal


    GCAM_Near_Term_Intake_Reference_Agg_Pcal %>%
      left_join(
        # Note that we used SSP in GCAM_Near_Term_Intake_Reference
        # 2025 should not be very different across scenarios
        L101.Pop_thous_SSP_R_Yfut %>%
          filter(scenario == "SSP2") %>%
          rename(totalPop = value) %>% select(-scenario) %>%
          left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID"),
        by = c("region", "year", "GCAM_region_ID")
      ) %>%
      mutate(value = value / totalPop * 10^9 /365) %>% # to kcal/ca/d
      select(-totalPop) %>%
      mutate(year = paste0("GCAM_intake_", year)) %>%
      spread(year, value) ->
      Dietary_Intake_Near_Term_Calibration


    # C. Get  EL2 dietary change shocks ----
    # We will implement dietary change as percentage "shocks"
    # We first derive a Scaler comparing EL2 target vs. intake (2020) in the AgMIP data (from Marco)

    AgMIP_EL2_intake_targets_foodgroup_r %>%
      select(-diet_scenario, -unit) %>%
      rename(sector = AgMIP_food_group_agg) %>%
      spread(measure, value) %>%
      mutate(EL2_Scaler = targetEL2/intake2020) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      EL2_Intake_Shock_GCAM_0

    ## Adjustments
    # add Taiwan EL2 targets per China
    # map fish to OtherMeat_Fish in EL2

    EL2_Intake_Shock_GCAM_0 %>%
      filter(region == "China") %>%
      mutate(region = "Taiwan", GCAM_region_ID = 30) %>%
      bind_rows(
        EL2_Intake_Shock_GCAM_0
      ) ->
      EL2_Intake_Shock_GCAM

    # ensure all regions are included
    assertthat::assert_that(
      EL2_Intake_Shock_GCAM %>% distinct(region) %>%
        anti_join(GCAM_region_names, by = "region") %>% nrow() == 0)


    # D. Connect GCAM EL2 base year intake to scalers to derive GCAM EL2 targets ----

    GCAM_BaseData_Diet_Intake_NEC1 %>%
      bind_rows(
        EL2_Intake_Shock_GCAM %>%
          select(GCAM_region_ID, sector, region, EL2_Scaler) %>%
          gather(measure, value, EL2_Scaler)) ->
      GCAM_AgMIP_Supply_Intake_base

    GCAM_AgMIP_Supply_Intake_base %>%
      spread(measure, value) %>%
      left_join(
        Dietary_Intake_Near_Term_Calibration,
        by = c("GCAM_region_ID", "sector", "region")) %>%
      #filter(region == "Pakistan")
      # assuming "others" in GCAM, mainly misc crops, e.g. stimulants, is fixed
      # mutate(EL2_Scaler = if_else(sector == "Others", 1, EL2_Scaler)) %>%
      # it won't affect anything since fish supply is unlimited
      # but let's keep the targets in
      # scale GCAM 2021 values to targets using EL2_Scaler
      mutate(GCAM_TargetEL2 = GCAM_2021_Intake * EL2_Scaler) ->
      GCAM_AgMIP_Supply_Intake_base3

    # Clean up measure names
    # change others to OtherCrop for GCAM uses

    c("Staples", "Oils", "PlantProtein",
      "Sugar", "FruitsVeg", "OtherCrop",
      "Ruminant", "NonRuminant", "Dairy", "Fish", "NEC") ->
      GCAM_supplysector

    GCAM_AgMIP_Supply_Intake_base3 %>%
      select(GCAM_region_ID, region, sector,
             GCAM_intake_2021 = GCAM_2021_Intake,
             GCAM_intake_2025,
             GCAM_TargetEL2) %>%
      gather(measure, value, GCAM_intake_2021:GCAM_TargetEL2) %>%
      mutate(sector = as.character(sector)) %>% #distinct(sector)
      # change others to OtherCrop for GCAM uses
      mutate(sector = if_else(sector == "Others", "OtherCrop", sector)) %>%
      rename(supplysector = sector) %>%
      mutate(supplysector = factor(supplysector, levels = GCAM_supplysector)) ->
      GCAM_AgMIP_Supply_Intake_base4

    # omitted code visualizing /checking data here

    # E. Develop scenarios by completing the trajectory  of intakes ----

    #"GCAM_TargetEL2" will be set to a future year with linear interpolations in-between
    # Note that target is defined based on kcal/ca/d so no SSP differentiation

    # * Scenario VLLO ----
    # For VLLO, we will use 2025 - 2070 linear path and constant after that

    GCAM_AgMIP_Supply_Intake_base4_EL2_2070 <-
      GCAM_AgMIP_Supply_Intake_base4 %>%
      mutate(measure = case_when(measure == "GCAM_intake_2021" ~ 2021,
                                 measure == "GCAM_intake_2025" ~ 2025,
                                 measure == "GCAM_TargetEL2" ~ 2070)) %>%
      arrange(GCAM_region_ID, region, supplysector, measure)


    # EL2 2070: Interpolate diet linearly to 2070
    GCAM_AgMIP_Supply_Intake_base4 %>%
      filter(supplysector != "NEC") %>%
      distinct(GCAM_region_ID, supplysector) %>%
      repeat_add_columns(tibble(year = c(2021,seq(2025, 2070,5)))) %>%
      left_join(select(GCAM_AgMIP_Supply_Intake_base4_EL2_2070, -region),
                by = c("GCAM_region_ID", "supplysector", "year" = "measure")) %>%
      group_by_at(vars(-year, -value)) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup() ->
      GCAM_AgMIP_Supply_Intake_base5_EL2_2070

    # Adding post-2070 as 2070
    GCAM_AgMIP_Supply_Intake_base5_EL2_2070 %>%
      bind_rows(
        GCAM_AgMIP_Supply_Intake_base5_EL2_2070 %>%
          filter(year == 2070) %>% select(-year) %>%
          repeat_add_columns(tibble(year = seq(2075, 2100,5)))
      ) ->
      GCAM_AgMIP_Supply_Intake_base6_EL2_2070

    ## Need to define SSP scenario here ----
    GCAM_AgMIP_Supply_Intake_base6_EL2_2070 %>%
      repeat_add_columns(tibble(scenario = c("SSP1", "SSP2"))) %>%
      arrange(scenario)->
      GCAM_Intake_kcal_Scenario_VLLO_2025_2070_SSP


    # * Scenario VLHO ----
    # For VLHO, we will use 2025 - 2100 linear path and constant after that

    GCAM_AgMIP_Supply_Intake_base4_EL2_2100 <-
      GCAM_AgMIP_Supply_Intake_base4 %>%
      mutate(measure = case_when(measure == "GCAM_intake_2021" ~ 2021,
                                 measure == "GCAM_intake_2025" ~ 2025,
                                 measure == "GCAM_TargetEL2" ~ 2100)) %>%
      arrange(GCAM_region_ID, region, supplysector, measure)


    # EL2 2050: Interpolate diet linearly to 2100
    GCAM_AgMIP_Supply_Intake_base4 %>%
      filter(supplysector != "NEC") %>%
      distinct(GCAM_region_ID, supplysector) %>%
      repeat_add_columns(tibble(year = c(2021,seq(2025, 2100,5)))) %>%
      left_join(select(GCAM_AgMIP_Supply_Intake_base4_EL2_2100, -region),
                by = c("GCAM_region_ID", "supplysector", "year" = "measure")) %>%
      group_by_at(vars(-year, -value)) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup() ->
      GCAM_AgMIP_Supply_Intake_base5_EL2_2100

    ## Need to define SSP scenario here ----
    GCAM_AgMIP_Supply_Intake_base5_EL2_2100 %>%
      repeat_add_columns(tibble(scenario = c("SSP1", "SSP2"))) %>%
      arrange(scenario) ->
      GCAM_Intake_kcal_Scenario_VLHO_2025_2100_SSP


    # F. Pcal from kcal per ca per day and derive income elasticity required ----


    # Get pc GDP and GDP in future periods
    L101.Pop_thous_R_Yh %>%
      repeat_add_columns(
        tibble(scenario = unique(L101.Pop_thous_SSP_R_Yfut$scenario))) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      rename(totalPop = value) ->
      L201.Pop_SSP_BaseYear

    L102.gdp_mil90usd_Scen_R_Y %>%
      filter(year >= MODEL_FINAL_BASE_YEAR) %>%
      rename(GDP = value) %>%
      left_join_error_no_match(
        L101.Pop_thous_SSP_R_Yfut %>%
          rename(totalPop = value) %>%
          # bind 2021 values
          bind_rows(L201.Pop_SSP_BaseYear),
        by = c("GCAM_region_ID", "scenario", "year")
      ) %>%
      mutate(pcGDP = GDP /totalPop * 1000) ->
      POPGDP_SSPs

    # derive income elasticity required
    ## Scenario VLLO
    GCAM_Intake_kcal_Scenario_VLLO_2025_2070_SSP %>%
      left_join(POPGDP_SSPs %>%
                  select(scenario, GCAM_region_ID, year, totalPop, GDP),
                by = c("GCAM_region_ID", "year", "scenario")) %>%
      mutate(Pcal = value * 365 * totalPop/10^9) %>%
      group_by(scenario, GCAM_region_ID, supplysector) %>%
      arrange(scenario, GCAM_region_ID, supplysector, year) %>%
      mutate(Lag_GDP = lag(GDP),
             LogDiffGDP = log(GDP/lag(GDP)),
             LogDiffPcal = log(Pcal/lag(Pcal)),
             income.elasticity = LogDiffPcal / LogDiffGDP) %>%
      ungroup() %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # update names to new final demand
      mutate(energy.final.demand = as.character(supplysector),
             energy.final.demand = if_else(energy.final.demand == "Staples",
                                           energy.final.demand, paste0("NonStaples_", energy.final.demand)),
             energy.final.demand = paste0("FoodDemand_", energy.final.demand)) ->
      L101.CropMeat_Food_Pcal_R_C_Y_IntakePathways_VLLO_2025_2070_SSP

    L101.CropMeat_Food_Pcal_R_C_Y_IntakePathways_VLLO_2025_2070_SSP %>%
      filter(year >= min(MODEL_FUTURE_YEARS)) %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      # Taiwan and South American North has constant pc GDP after 2050 per our assumptions
      # this led to inf in income elasticity
      mutate(income.elasticity = replace(income.elasticity, is.infinite(income.elasticity), 0)) ->
      L100.IncomeElasticity_Food_ExoDiet_VLLO_2025_2070_SSP

    assertthat::assert_that(
      L100.IncomeElasticity_Food_ExoDiet_VLLO_2025_2070_SSP %>%
        filter(is.na(income.elasticity)) %>%
        nrow() == 0 )

    # *income elasticity needed done for VLLO ----


    ## Scenario VLLO
    GCAM_Intake_kcal_Scenario_VLHO_2025_2100_SSP %>%
      left_join(POPGDP_SSPs %>%
                  select(scenario, GCAM_region_ID, year, totalPop, GDP),
                by = c("GCAM_region_ID", "year", "scenario")) %>%
      mutate(Pcal = value * 365 * totalPop/10^9) %>%
      group_by(scenario, GCAM_region_ID, supplysector) %>%
      arrange(scenario, GCAM_region_ID, supplysector, year) %>%
      mutate(Lag_GDP = lag(GDP),
             LogDiffGDP = log(GDP/lag(GDP)),
             LogDiffPcal = log(Pcal/lag(Pcal)),
             income.elasticity = LogDiffPcal / LogDiffGDP) %>%
      ungroup() %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # update names to new final demand
      mutate(energy.final.demand = as.character(supplysector),
             energy.final.demand = if_else(energy.final.demand == "Staples",
                                           energy.final.demand, paste0("NonStaples_", energy.final.demand)),
             energy.final.demand = paste0("FoodDemand_", energy.final.demand)) ->
      L101.CropMeat_Food_Pcal_R_C_Y_IntakePathways_VLHO_2025_2100_SSP

    L101.CropMeat_Food_Pcal_R_C_Y_IntakePathways_VLHO_2025_2100_SSP %>%
      filter(year >= min(MODEL_FUTURE_YEARS)) %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      # Taiwan and South American North has constant pc GDP after 2050 per our assumptions
      # this led to inf in income elasticity
      mutate(income.elasticity = replace(income.elasticity, is.infinite(income.elasticity), 0)) ->
      L100.IncomeElasticity_Food_ExoDiet_VLHO_2025_2100_SSP

    assertthat::assert_that(
      L100.IncomeElasticity_Food_ExoDiet_VLHO_2025_2100_SSP %>%
        filter(is.na(income.elasticity)) %>%
        nrow() == 0 )

    # *income elasticity needed done for VLHO ----

    # Step 2. repeat module_aglu_L203.ag_an_demand_input here ----

    ## Get mass-calories conversion rates for food commodities----
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

    #* [added here] processing food nesting structure for exo diet----
    #*  deal with food proc file changes
    L2328.StubCalorieContent %>%
      select(-supplysector, -subsector, -subsector0) %>%
      left_join_error_no_match(L203.StubTech_demand_Food_ExoDiet,
                               by = c("region", "stub.technology")) ->
      L2328.StubCalorieContent_Food_ExoDiet

    L2328.StubCaloriePriceConv %>%
      select(-supplysector, -subsector, -subsector0) %>%
      left_join_error_no_match(L203.StubTech_demand_Food_ExoDiet,
                               by = c("region", "stub.technology")) ->
      L2328.StubCaloriePriceConv_Food_ExoDiet


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


    ## Calibrated staple and non-staple demands of crops and meat ----
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
      left_join_error_no_match(L203.ag_an_Food_Pcal_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(calOutputValue = round(value, aglu.DIGITS_CALOUTPUT),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs0.share.weight = if_else(calOutputValue > 0, 1, 0),
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(c(LEVEL2_DATA_NAMES[["StubTechProd"]]), subsector0, subs0.share.weight) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%           # Remove any regions for which agriculture and land use are not modeled
      filter(year %in% MODEL_BASE_YEARS) ->                         # Also subset the calibration tables to only the model base years
      L203.StubTechProd_food_Food_ExoDiet

    ## Build L203.StubCalorieContent: ----
    # calorie content of food crops (incl secondary products) and meat commodities
    L101.CropMeat_Food_kcalg_R_C_Y %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.ag_an_kcalg_R_C_Y

    A_demand_technology_R_Y_Food_ExoDiet %>%
      filter(grepl("^FoodDemand", supplysector)) %>%
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
    # efficiency values here in 2021 should match those in L203.StubCalorieContent
    # only the supplysector group is different

    ## FINAL DEMANDS ----

    ### Base service exo food ----
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
    # We assumed a default of 0.1 (placeholder) income.elasticity;
    # But dynamic path is generated to target 2050EL2

    ## Done Step2. repeating module_aglu_L203.ag_an_demand_input ----

    # *****************---------

    # Step 3. update income elasticity and food waste pathways  ----
    ## update income elasticity with what derived in Step 1 and generate scenario based XML
    ## also update L203.StubCalorieContent_Food_ExoDiet to reflect waste changes


    # Note that PerCapitaBased is turned off so that future changes in diet will be pure income elast. driven
    # also 2025 was based on BAU

    ## SSP1 VLLO ----

    for (ssp in paste0("SSP", 1:2)) {

    ## update L203.IncomeElasticity_Food_ExoDiet
    L203.IncomeElasticity_Food_ExoDiet_updated <-
      L100.IncomeElasticity_Food_ExoDiet_VLLO_2025_2070_SSP %>%
      filter(scenario == ssp) %>% select(-scenario)

    # assure sector names are the identical
    assertthat::assert_that(
      dplyr::setdiff(L203.IncomeElasticity_Food_ExoDiet_updated %>%
                       distinct(region, energy.final.demand, year),
                     L203.IncomeElasticity_Food_ExoDiet %>%
                       distinct(region, energy.final.demand, year)) %>% nrow == 0
    )


    # Adjust the constant trend based on SSP scenarios
    L203.StubCalorieContent_Food_ExoDiet %>%
      left_join(
        L100.AgMIP_FoodWaste_Share_Pathway_SSP %>%
          filter(scenario == ssp) %>%
          transmute(region, subsector = GCAM_commodity, year, WasteShare) %>%
          mutate(NonWasteShare = (1 - WasteShare) ) %>%
          group_by(region, subsector) %>%
          # 2021 was the model base year when efficiency was defined
          # we didn't do any adjustment before the base year; assuming waste shares were the same
          filter(year >= MODEL_FINAL_BASE_YEAR) %>%
          mutate(WasteScaler = NonWasteShare / NonWasteShare[year == MODEL_FINAL_BASE_YEAR]) %>%
          ungroup %>% select(-WasteShare, -NonWasteShare) %>%
          rename(stub.technology = subsector),
        by = c("region", "stub.technology", "year")
      ) -> L203.StubCalorieContent_Food_ExoDiet1

    # assert that we have values for all future years
    assertthat::assert_that(
      L203.StubCalorieContent_Food_ExoDiet1 %>%
        filter(year >= MODEL_FINAL_BASE_YEAR, is.na(WasteScaler)) %>% nrow() == 0 )

    L203.StubCalorieContent_Food_ExoDiet1 %>%
      replace_na(list(WasteScaler = 1)) %>%
      mutate(efficiency = WasteScaler * efficiency) %>%
      select(names(L203.StubCalorieContent_Food_ExoDiet)) ->
      L203.StubCalorieContent_Food_ExoDiet_WasteTrend


    ### Produce outputs ----

    create_xml(paste0("ag_an_demand_input_Food_ExoDiet_",ssp,"_VLLO.xml")) %>%
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
      # commented here and above as it is an empty table
      #add_xml_data(L203.GlobalTechInterp_demand_Food_ExoDiet, "GlobalTechInterp") %>%
      add_xml_data(L203.IncomeElasticity_Food_ExoDiet_updated, "IncomeElasticity") %>%
      add_xml_data(L203.PriceElasticity_Food_ExoDiet, "PriceElasticity") %>%
      add_xml_data(L203.PerCapitaBased_Food_ExoDiet, "PerCapitaBased") %>%
      add_xml_data(L203.BaseService_Food_ExoDiet, "BaseService") %>%
      add_xml_data_generate_levels(L2328.StubCalorieContent_Food_ExoDiet, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2328.StubCaloriePriceConv_Food_ExoDiet, "StubCaloriePriceConv", "subsector","nesting-subsector",1,FALSE) %>%
      add_precursors(MODULE_INPUTS) ->
      ag_an_demand_input_Food_ExoDiet_SSPs

    assign(paste0("ag_an_demand_input_Food_ExoDiet_",ssp,"_VLLO.xml"),
           value = ag_an_demand_input_Food_ExoDiet_SSPs, envir = cur_env )


    }


    ## SSP1 VLHO ----

    for (ssp in paste0("SSP", 1:2)) {

      ## update L203.IncomeElasticity_Food_ExoDiet
      L203.IncomeElasticity_Food_ExoDiet_updated <-
        L100.IncomeElasticity_Food_ExoDiet_VLHO_2025_2100_SSP %>%
        filter(scenario == ssp) %>% select(-scenario)

      # assure sector names are the identical
      assertthat::assert_that(
        dplyr::setdiff(L203.IncomeElasticity_Food_ExoDiet_updated %>%
                         distinct(region, energy.final.demand, year),
                       L203.IncomeElasticity_Food_ExoDiet %>%
                         distinct(region, energy.final.demand, year)) %>% nrow ==0
      )

      # Adjust the constant trend based on SSP scenarios
      L203.StubCalorieContent_Food_ExoDiet %>%
        left_join(
          L100.AgMIP_FoodWaste_Share_Pathway_SSP %>%
            filter(scenario == ssp) %>%
            transmute(region, subsector = GCAM_commodity, year, WasteShare) %>%
            mutate(NonWasteShare = (1 - WasteShare) ) %>%
            group_by(region, subsector) %>%
            # 2021 was the model base year when efficiency was defined
            # we didn't do any adjustment before the base year; assuming waste shares were the same
            filter(year >= MODEL_FINAL_BASE_YEAR) %>%
            mutate(WasteScaler = NonWasteShare / NonWasteShare[year == MODEL_FINAL_BASE_YEAR]) %>%
            ungroup %>% select(-WasteShare, -NonWasteShare) %>%
            rename(stub.technology = subsector),
          by = c("region", "stub.technology", "year")
        ) -> L203.StubCalorieContent_Food_ExoDiet1

      # assert that we have values for all future years
      assertthat::assert_that(
        L203.StubCalorieContent_Food_ExoDiet1 %>%
          filter(year >= MODEL_FINAL_BASE_YEAR, is.na(WasteScaler)) %>% nrow() == 0 )

      L203.StubCalorieContent_Food_ExoDiet1 %>%
        replace_na(list(WasteScaler = 1)) %>%
        mutate(efficiency = WasteScaler * efficiency) %>%
        select(names(L203.StubCalorieContent_Food_ExoDiet)) ->
        L203.StubCalorieContent_Food_ExoDiet_WasteTrend

      ### Produce outputs ----

      create_xml(paste0("ag_an_demand_input_Food_ExoDiet_",ssp,"_VLHO.xml")) %>%
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
        # commented here and above as it is an empty table
        #add_xml_data(L203.GlobalTechInterp_demand_Food_ExoDiet, "GlobalTechInterp") %>%
        add_xml_data(L203.IncomeElasticity_Food_ExoDiet_updated, "IncomeElasticity") %>%
        add_xml_data(L203.PriceElasticity_Food_ExoDiet, "PriceElasticity") %>%
        add_xml_data(L203.PerCapitaBased_Food_ExoDiet, "PerCapitaBased") %>%
        add_xml_data(L203.BaseService_Food_ExoDiet, "BaseService") %>%
        add_xml_data_generate_levels(L2328.StubCalorieContent_Food_ExoDiet, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
        add_xml_data_generate_levels(L2328.StubCaloriePriceConv_Food_ExoDiet, "StubCaloriePriceConv", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors(MODULE_INPUTS) ->
        ag_an_demand_input_Food_ExoDiet_SSPs

      assign(paste0("ag_an_demand_input_Food_ExoDiet_",ssp,"_VLHO.xml"),
             value = ag_an_demand_input_Food_ExoDiet_SSPs, envir = cur_env )

    }



    # Done ----
    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}

