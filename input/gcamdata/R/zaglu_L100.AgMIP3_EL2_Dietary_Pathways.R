# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L100.AgMIP3_EL2_Dietary_Pathways
#'
#' Process AgMIP diet data to generate dietary change targets
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L100.IncomeElasticity_Food_ExoDiet_2050EL2_SSP}
#' @details Process AgMIP diet data to generat dietary change scenarios.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else inner_join left_join mutate rename select
#' @importFrom tidyr  complete drop_na gather nesting spread replace_na
#' @importFrom tibble tibble
#' @author XZ 2022
module_aglu_L100.AgMIP3_EL2_Dietary_Pathways <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "common/iso_GCAM_regID",
      FILE = "aglu/AgMIP/GCAM_AgMIP_food_group_mapping",
      FILE = "aglu/AgMIP/AgMIP_BAU_Diet2020",
      FILE = "aglu/AgMIP/Diet_3C_Ref",
      "L100.AgMIP_EL2_intake_targets_foodgroup_r",
      "FAO_Food_Macronutrient_All_2010_2019",
      "L101.CropMeat_Food_Pcal_R_C_Y",
      "L101.Pop_thous_R_Yh",
      "L101.Pop_thous_Scen_R_Yfut",
      "L102.gdp_mil90usd_Scen_R_Y",
      "L201.Pop_SSP2")

  MODULE_OUTPUTS <-
    c("L100.IncomeElasticity_Food_ExoDiet_2050EL2_SSP",
      "L100.IncomeElasticity_Food_ExoDiet_2100EL2_SSP",
      "L100.IncomeElasticity_Food_ExoDiet_Static_SSP")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    L100.AgMIP_EL2_intake_targets_foodgroup_r ->
      AgMIP_foodgrouptargets_GCAM

    # 4. dietary change pathways EL2 ----
    # Pull GCAM values and connect them to EL2 targets

    # Note that we will calculate total Pcal by food group, which will be exogenously driven
    # by population and "income elasticity" to meet EL2 targets.
    # Since we have population, we can first derive EL2 total Pcal by food group



    # Will only use NEC from here
    FAO_Food_Macronutrient_All_2010_2019 %>%
      filter(year %in% aglu.MODEL_MACRONUTRIENT_YEARS) %>%
      # Aggregate to region and GCAM commodity
      dplyr::group_by_at(vars(GCAM_region_ID, GCAM_commodity, year, macronutrient)) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      # Mean over aglu.MODEL_MACRONUTRIENT_YEARS
      dplyr::group_by_at(vars(GCAM_region_ID, GCAM_commodity, macronutrient)) %>%
      summarise(value = mean(value), .groups = "drop") %>%
      spread(macronutrient, value) ->
      DF_Macronutrient_FoodItem1

    # Adding NEC to L101.CropMeat_Food_Pcal_R_C_Y (GCAM base year intake)
    DF_Macronutrient_FoodItem1 %>%
      filter(GCAM_commodity == "NEC") %>%
      transmute(GCAM_region_ID, GCAM_commodity, year = 2015, value = MKcal/1000000) %>%
      bind_rows(L101.CropMeat_Food_Pcal_R_C_Y) ->
      GCAM_BaseData_Diet_Intake_NEC


    GCAM_AgMIP_food_group_mapping %>%
      distinct(GCAM_food_agg) %>% pull %>% c(.,"NEC") -> COMM_GCAM_food_agg

    # compare GCAM base year intake with AgMIP intake and EL2 target
    GCAM_BaseData_Diet_Intake_NEC %>%
      left_join_error_no_match(GCAM_region_names) %>%
      filter(year == 2015) %>%
      left_join_error_no_match(L201.Pop_SSP2) %>%
      mutate(value = value / totalPop * 1000000000 /365) %>%
      left_join(
        GCAM_AgMIP_food_group_mapping %>%
          distinct(GCAM_commodity = GCAM_food_commodities, GCAM_food_agg)) %>%
      mutate(GCAM_food_agg = if_else(is.na(GCAM_food_agg), GCAM_commodity, GCAM_food_agg)) %>%
      group_by_at(vars(-GCAM_commodity, -value)) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      mutate(GCAM_food_agg = factor(GCAM_food_agg,
                                    levels = rev(COMM_GCAM_food_agg))) %>%
      select(-totalPop) %>% mutate(measure = "GCAM2015_Intake") %>%
      select(-year, -region) ->
      GCAM_BaseData_Diet_Intake_NEC1

    GCAM_BaseData_Diet_Intake_NEC1 %>%
      rename(sector = GCAM_food_agg) %>%
      bind_rows(
        AgMIP_foodgrouptargets_GCAM %>% select(-diet_scenario, -unit) %>%
          rename(sector = AgMIP_food_group_agg) ) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      GCAM_AgMIP_Supply_Intake_base

    ## Adjustments
    # add Taiwan EL2 targets per China
    # map fish to OtherMeat_Fish in EL2
    GCAM_AgMIP_Supply_Intake_base %>%
      mutate(sector = replace(sector, sector == "OtherMeat_Fish", "Fish")) ->
      GCAM_AgMIP_Supply_Intake_base1

    GCAM_AgMIP_Supply_Intake_base1 %>%
      filter(region == "China", measure %in% c("intake2020", "targetEL2")) %>%
      mutate(region = "Taiwan", GCAM_region_ID = 30) %>%
      bind_rows(
        GCAM_AgMIP_Supply_Intake_base1
      ) ->
      GCAM_AgMIP_Supply_Intake_base2


    # GCAM_AgMIP_Supply_Intake_base2 %>% #filter(sector == "Grains") %>%
    #   #filter(GCAM_region_ID %in% c(1, 2, 7, 11, 12, 18)) %>%
    #   ggplot() + facet_wrap(~region) +
    #   geom_bar(aes(x = measure, y = value, fill = sector),
    #            stat = "identity", color = "black", size = 0.4) +
    #   labs(y = "kcal/ca/d", x = "Scenario")  +
    #   theme_bw() +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p;p

    #ggsave(plot = p, "AgMIP/CalorieSupplyToEL2Intake.png", width = 20, height = 20 )


    #AgMIP_BAU_Diet2020 may need an update later!!!

    AgMIP_BAU_Diet2020 %>%
      rename(region = Region, GCAM2020_Intake = `2020`) %>%
      left_join(
        GCAM_AgMIP_food_group_mapping %>%
          distinct(GCAM_commodity = GCAM_food_commodities, sector = GCAM_food_agg),
        by = "GCAM_commodity") %>%
      group_by_at(vars(-GCAM2020_Intake, -GCAM_commodity)) %>%
      summarize(GCAM2020_Intake = sum(GCAM2020_Intake), .groups = "drop") ->
      BAU_Diet2020_Agg_Pcal

    BAU_Diet2020_Agg_Pcal %>%
      left_join(
        L101.Pop_thous_Scen_R_Yfut %>%
          filter( scenario == "gSSP2", year == 2020) %>%
          rename(totalPop = value) %>% select(-scenario, -year) %>%
          left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID"),
        by = "region"
      ) %>%
      mutate(GCAM2020_Intake = GCAM2020_Intake / totalPop * 1000000000 /365) %>%
      select(-totalPop)->
      BAU_Diet2020_Agg

    # BY 3/2/2024: Use the same process as taking GCAM BAU Diet 2020 to get 2025 values
    # Now, we will do the same for 2025 in the 3C Ref diet
    Diet_3C_Ref %>%
      filter(year == 2025) %>%
      rename(GCAM_commodity = technology) %>%
      left_join(
        GCAM_AgMIP_food_group_mapping %>%
          distinct(GCAM_commodity = GCAM_food_commodities, sector = GCAM_food_agg),
        by = "GCAM_commodity") %>%
      select(-scenario) %>%
      group_by_at(vars(-value, -GCAM_commodity)) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      rename(measure = year,
             supplysector = sector) %>%
      left_join_error_no_match(GCAM_region_names, by = c("region"))-> Diet_3C_Ref_Agg_Pcal_2025

    Diet_3C_Ref_Agg_Pcal_2025 %>%
      left_join(L101.Pop_thous_Scen_R_Yfut %>%
                  filter( scenario == "gSSP1") %>%
                  rename(totalPop = value) %>% select(-scenario) %>%
                  left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID"),
                by = c("region", "GCAM_region_ID", "measure" = "year")) %>%
      mutate(value = value / totalPop * 1000000000 /365) %>%
      select(-totalPop) %>%
      mutate(supplysector = if_else(supplysector == "Others", "OtherCrop", supplysector),
             measure = as.character(measure)) ->
      Diet_3C_Ref_Agg_2025

    GCAM_AgMIP_Supply_Intake_base2 %>%
      left_join_error_no_match(GCAM_region_names, by = c("GCAM_region_ID", "region")) %>%
      spread(measure, value) %>%
      # [ToDo!] will need to update 2020 values later
      #mutate(GCAM2020_Intake = GCAM2015_Intake) %>%
      # [Done]
      left_join(BAU_Diet2020_Agg, by = c("GCAM_region_ID", "sector", "region")) %>%
      mutate(EL2_Scaler = targetEL2/intake2020,
             GCAMBase_IntakeScaler = intake2020/GCAM2020_Intake) %>%
      # assuming "others" in GCAM, mainly misc crops, e.g. stimulants is fixed
      mutate(EL2_Scaler = if_else(sector == "Others", 1, EL2_Scaler)) %>%
      # scale GCAM 2020 values to targets using EL2_Scaler
      mutate(GCAM_TargetEL2 = GCAM2020_Intake * EL2_Scaler) ->
      GCAM_AgMIP_Supply_Intake_base3

    c("Staples", "Oils", "PlantProtein",
      "Sugar", "FruitsVeg", "OtherCrop",
      "Ruminant", "NonRuminant", "Dairy", "OtherMeat_Fish", "NEC") ->
      GCAM_supplysector

    GCAM_AgMIP_Supply_Intake_base3 %>%
      select(GCAM_region_ID, sector, region, GCAM2015_Intake, GCAM2020_Intake, GCAM_TargetEL2) %>%
      gather(measure, value, GCAM2015_Intake, GCAM2020_Intake, GCAM_TargetEL2) %>%
      mutate(sector = as.character(sector)) %>%
      # change others to OtherCrop for GCAM uses
      mutate(sector = if_else(sector == "Others", "OtherCrop", sector)) %>%
      rename(supplysector = sector) %>%
      mutate(supplysector = factor(supplysector, levels = GCAM_supplysector)) %>%
      # bind in 2025 values
      bind_rows(Diet_3C_Ref_Agg_2025) ->
      GCAM_AgMIP_Supply_Intake_base4


    # GCAM_AgMIP_Supply_Intake_base4 %>%
    #   filter(supplysector != "NEC") %>%
    #   filter(measure!= "GCAM2015_Intake") ->df
    #
    # df %>% ggplot() + facet_wrap(~supplysector, scales = "free_x", nrow = 1) +
    #   geom_point(aes(x = region, y = value, fill = measure, shape = measure), size = 2 ) +
    #
    #   geom_linerange(data = df %>%
    #                    spread(measure, value) %>%
    #                    mutate(diff = GCAM2050_EL2 - GCAM2020_Intake,
    #                           Change = if_else(diff >=0, "Inc", "Dec")),
    #                  aes(x = region, ymin = GCAM2020_Intake,  ymax = GCAM2050_EL2, color = Change),
    #                  size = 1, alpha = 0.8 ) +
    #   coord_flip() +
    #   scale_shape_manual(values = c(21, 22)) +
    #   #geom_bar(aes(x = measure, y = value, fill = supplysector), stat = "identity", color = "black", size = 0.4) +
    #   labs(y = "kcal/ca/d", x = "Scenario", fill = "Diet", shape = "Diet")  +
    #   theme_bw() +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p;p
    #
    # ggsave(plot = p, "figures/GCAM_EL2IntakeShocks.png", width = 16, height = 14 )

    # df %>%
    #   filter(GCAM_region_ID %in% c(1, 5, 11)) ->df1
    #
    # df1 %>% ggplot() + facet_wrap(~region) +
    #   geom_point(aes(x = supplysector, y = value, fill = measure, shape = measure), size = 2 ) +
    #
    #   geom_linerange(data = df1 %>%
    #                    spread(measure, value) %>%
    #                    mutate(diff = GCAM2050_EL2 - GCAM2020_Intake,
    #                           Change = if_else(diff >=0, "Inc", "Dec")),
    #                  aes(x = supplysector, ymin = GCAM2020_Intake,  ymax = GCAM2050_EL2, color = Change),
    #                  size = 1, alpha = 0.8 ) +
    #   #coord_flip() +
    #   scale_shape_manual(values = c(21, 22)) +
    #   #geom_bar(aes(x = measure, y = value, fill = supplysector), stat = "identity", color = "black", size = 0.4) +
    #   labs(y = "kcal/ca/d", x = "Scenario", fill = "Diet", shape = "Diet")  +
    #   theme_bw() +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p;p
    #
    # ggsave(plot = p, "AgMIP/GCAM_EL2IntakeShocks_keyreg.png", width = 10, height = 7 )

    #"GCAM_TargetEL2" will be different depending on interpolation year
    GCAM_AgMIP_Supply_Intake_base4_EL2_2050 <- GCAM_AgMIP_Supply_Intake_base4 %>%
      mutate(measure = case_when(measure == "GCAM2015_Intake" ~ 2015,
                                 measure == "GCAM2020_Intake" ~ 2020,
                                 measure == "2025" ~ 2025,
                                 measure == "GCAM_TargetEL2" ~ 2050)) %>%
      arrange(GCAM_region_ID, region, supplysector, measure)

    GCAM_AgMIP_Supply_Intake_base4_EL2_2100 <- GCAM_AgMIP_Supply_Intake_base4 %>%
      mutate(measure = case_when(measure == "GCAM2015_Intake" ~ 2015,
                                 measure == "GCAM2020_Intake" ~ 2020,
                                 measure == "2025" ~ 2025,
                                 measure == "GCAM_TargetEL2" ~ 2100)) %>%
      arrange(GCAM_region_ID, region, supplysector, measure)

    GCAM_AgMIP_Supply_Intake_base4_Static <- GCAM_AgMIP_Supply_Intake_base4 %>%
      mutate(measure = case_when(measure == "GCAM2015_Intake" ~ 2015,
                                 measure == "GCAM2020_Intake" ~ 2020,
                                 measure == "2025" ~ 2025)) %>%
      na.omit() %>%
      arrange(GCAM_region_ID, region, supplysector, measure)

    # EL2 2050: Interpolate diet linearly to 2050
    GCAM_AgMIP_Supply_Intake_base4 %>%
      filter(supplysector != "NEC") %>%
      distinct(GCAM_region_ID, supplysector) %>%
      repeat_add_columns(tibble(year = seq(2015, 2050,5))) %>%
      left_join(select(GCAM_AgMIP_Supply_Intake_base4_EL2_2050, -region),  by = c("GCAM_region_ID", "supplysector", "year" = "measure")) %>%
      group_by_at(vars(-year, -value)) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup() ->
      GCAM_AgMIP_Supply_Intake_base5_EL2_2050

    # Adding post-2050 as 2050
    GCAM_AgMIP_Supply_Intake_base5_EL2_2050 %>%
      bind_rows(
        GCAM_AgMIP_Supply_Intake_base5_EL2_2050 %>%
          filter(year == 2050) %>% select(-year) %>%
          repeat_add_columns(tibble(year = seq(2055, 2100,5)))
      ) ->
      GCAM_AgMIP_Supply_Intake_base6_EL2_2050

    # EL2 2100: Interpolate diet linearly to 2100
    GCAM_AgMIP_Supply_Intake_base4 %>%
      filter(supplysector != "NEC") %>%
      distinct(GCAM_region_ID, supplysector) %>%
      repeat_add_columns(tibble(year = seq(2015, 2100,5))) %>%
      left_join(select(GCAM_AgMIP_Supply_Intake_base4_EL2_2100, -region),  by = c("GCAM_region_ID", "supplysector", "year" = "measure")) %>%
      group_by_at(vars(-year, -value)) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup() ->
      GCAM_AgMIP_Supply_Intake_base6_EL2_2100


    # Static: fill in 2025 values for all years
    GCAM_AgMIP_Supply_Intake_base4 %>%
      filter(supplysector != "NEC") %>%
      distinct(GCAM_region_ID, supplysector) %>%
      repeat_add_columns(tibble(year = seq(2015, 2100,5))) %>%
      left_join(select(GCAM_AgMIP_Supply_Intake_base4_Static, -region),  by = c("GCAM_region_ID", "supplysector", "year" = "measure")) %>%
      group_by_at(vars(-year, -value)) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() ->
      GCAM_AgMIP_Supply_Intake_base6_Static

    # Calculate Pcal from kcal per ca per day

    L101.Pop_thous_R_Yh %>%
      repeat_add_columns(tibble(scenario = unique(L101.Pop_thous_Scen_R_Yfut$scenario))) %>%
      filter(year == 2015) %>%
      rename(totalPop = value) -> L201.Pop_SSP_2015

    L102.gdp_mil90usd_Scen_R_Y %>%
      filter(year >= 2015) %>%
      rename(GDP = value) %>%
      left_join_error_no_match(
        L101.Pop_thous_Scen_R_Yfut %>%
          rename(totalPop = value) %>%
          # bind 2015 values
          bind_rows(L201.Pop_SSP_2015),
        by = c("GCAM_region_ID", "scenario", "year")
      ) %>%
      mutate(pcGDP = GDP /totalPop * 1000) ->
      POPGDP_SSPs

   # derive income elasticities required
    # EL2 2050
    GCAM_AgMIP_Supply_Intake_base6_EL2_2050 %>%
      left_join(POPGDP_SSPs %>%
                  select(scenario, GCAM_region_ID, year, totalPop, GDP),
                by = c("GCAM_region_ID", "year")) %>%
      mutate(Pcal = value * 365 * totalPop/1000000000) %>%
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
      L101.CropMeat_Food_Pcal_R_C_Y_IntakePathways_SSP_EL2_2050

    L101.CropMeat_Food_Pcal_R_C_Y_IntakePathways_SSP_EL2_2050 %>%
      filter(year >= 2025) %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      # Taiwan and South American North has constant GDPs after 2050 per our assumptions
      # this led to inf in income elasticity
      mutate(income.elasticity = replace(income.elasticity, is.infinite(income.elasticity), 0)) ->
      L100.IncomeElasticity_Food_ExoDiet_2050EL2_SSP


    # EL2 2100
    GCAM_AgMIP_Supply_Intake_base6_EL2_2100 %>%
      left_join(POPGDP_SSPs %>%
                  select(scenario, GCAM_region_ID, year, totalPop, GDP),
                by = c("GCAM_region_ID", "year")) %>%
      mutate(Pcal = value * 365 * totalPop/1000000000) %>%
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
      L101.CropMeat_Food_Pcal_R_C_Y_IntakePathways_SSP_EL2_2100

    L101.CropMeat_Food_Pcal_R_C_Y_IntakePathways_SSP_EL2_2100 %>%
      filter(year >= 2025) %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      # Taiwan and South American North has constant GDPs after 2050 per our assumptions
      # this led to inf in income elasticity
      mutate(income.elasticity = replace(income.elasticity, is.infinite(income.elasticity), 0)) ->
      L100.IncomeElasticity_Food_ExoDiet_2100EL2_SSP

    # Static
    GCAM_AgMIP_Supply_Intake_base6_Static %>%
      left_join(POPGDP_SSPs %>%
                  select(scenario, GCAM_region_ID, year, totalPop, GDP),
                by = c("GCAM_region_ID", "year")) %>%
      mutate(Pcal = value * 365 * totalPop/1000000000) %>%
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
      L101.CropMeat_Food_Pcal_R_C_Y_IntakePathways_SSP_Static

    L101.CropMeat_Food_Pcal_R_C_Y_IntakePathways_SSP_Static %>%
      filter(year >= 2025) %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      # Taiwan and South American North has constant GDPs after 2050 per our assumptions
      # this led to inf in income elasticity
      mutate(income.elasticity = replace(income.elasticity, is.infinite(income.elasticity), 0)) ->
      L100.IncomeElasticity_Food_ExoDiet_Static_SSP





    # Produce outputs ----
    #********************************* ----


    L100.IncomeElasticity_Food_ExoDiet_2050EL2_SSP %>%
      add_title("Dietary change scenarios driven by changing income elasticities by food groups") %>%
      add_units("NA") %>%
      add_comments("Generated dietary change scenarios per AgMIP EAT-Lancet targets") %>%
      add_precursors(MODULE_INPUTS) ->
      L100.IncomeElasticity_Food_ExoDiet_2050EL2_SSP

    L100.IncomeElasticity_Food_ExoDiet_2100EL2_SSP %>%
      add_title("Dietary change scenarios driven by changing income elasticities by food groups") %>%
      add_units("NA") %>%
      add_comments("Generated dietary change scenarios per AgMIP EAT-Lancet targets") %>%
      add_precursors(MODULE_INPUTS) ->
      L100.IncomeElasticity_Food_ExoDiet_2100EL2_SSP

    L100.IncomeElasticity_Food_ExoDiet_Static_SSP %>%
      add_title("Dietary change scenarios driven by changing income elasticities by food groups") %>%
      add_units("NA") %>%
      add_comments("Generated dietary scenarios - no change from 2025") %>%
      add_precursors(MODULE_INPUTS) ->
      L100.IncomeElasticity_Food_ExoDiet_Static_SSP


    # Done & return data----
    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
