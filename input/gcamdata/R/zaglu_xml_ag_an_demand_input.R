# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_ag_an_demand_input_xml
#'
#' Construct XML data structure for \code{ag_an_demand_input.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_an_demand_input.xml}. The corresponding file in the
#' original data system was \code{batch_demand_input_xml.R} (aglu XML).
module_aglu_ag_an_demand_input_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L203.Supplysector_demand",
      "L203.NestingSubsectorAll_demand_food",
      "L203.SubsectorAll_demand_food",
      "L203.SubsectorAll_demand_nonfood",
      "L203.StubTech_demand_food",
      "L203.StubTech_demand_nonfood",
      "L203.GlobalTechCoef_demand",
      "L203.GlobalTechShrwt_demand",
      "L203.StubTechProd_food",
      "L203.StubTechProd_nonfood_crop",
      "L203.StubTechProd_nonfood_meat",
      "L203.StubTechProd_For",
      "L203.StubCalorieContent",
      "L203.PerCapitaBased",
      "L203.BaseService",
      "L203.IncomeElasticity",
      "L203.PriceElasticity",
      "L203.SubregionalShares",
      "L203.DemandFunction_food",
      "L203.DemandStapleParams",
      "L203.DemandNonStapleParams",
      "L203.DemandStapleRegBias",
      "L203.DemandNonStapleRegBias",
      "L203.StapleBaseService",
      "L203.NonStapleBaseService",
      "L203.GlobalTechInterp_demand",
      FILE = "common/GCAM_region_names",
      "L100.AgMIP_FoodWaste_Share_Pathway_SSP")

  MODULE_OUTPUTS <-
    c(XML = "ag_an_demand_input.xml",
      XML = "ag_an_demand_input_NonFood.xml",
      XML = "ag_an_demand_input_Food.xml",
      XML = "ag_an_demand_input_Food_Waste_SSP1.xml",
      XML = "ag_an_demand_input_Food_Waste_SSP1_HalfWaste2050.xml",
      XML = "ag_an_demand_input_Food_Waste_SSP2.xml",
      XML = "ag_an_demand_input_Food_Waste_SSP2_HalfWaste2050.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================

    # Produce outputs ----
    create_xml("ag_an_demand_input.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand, "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_food,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_food, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_food, "SubsectorAll", "SubsectorLogit") %>%
      add_logit_tables_xml(L203.SubsectorAll_demand_nonfood, "SubsectorAll", "SubsectorLogit") %>%
      add_xml_data(L203.StubTech_demand_nonfood, "StubTech") %>%
      add_xml_data(L203.GlobalTechCoef_demand, "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand, "GlobalTechShrwt") %>%
      add_xml_data(L203.GlobalTechInterp_demand, "GlobalTechInterp") %>%
      add_xml_data(L203.StubTechProd_nonfood_crop, "StubTechProd") %>%
      add_xml_data(L203.StubTechProd_nonfood_meat, "StubTechProd") %>%
      add_xml_data(L203.StubTechProd_For, "StubTechProd") %>%
      add_xml_data(L203.PerCapitaBased, "PerCapitaBased") %>%
      add_xml_data(L203.BaseService, "BaseService") %>%
      add_xml_data(L203.IncomeElasticity, "IncomeElasticity") %>%
      add_xml_data(L203.PriceElasticity, "PriceElasticity") %>%
      add_xml_data(L203.SubregionalShares, "SubregionalShares") %>%
      add_xml_data(L203.DemandFunction_food, "DemandFunction_food") %>%
      add_xml_data(L203.DemandStapleParams, "DemandStapleParams") %>%
      add_xml_data(L203.DemandNonStapleParams, "DemandNonStapleParams") %>%
      add_xml_data(L203.DemandStapleRegBias, "DemandStapleRegBias") %>%
      add_xml_data(L203.DemandNonStapleRegBias, "DemandNonStapleRegBias") %>%
      add_xml_data(L203.StapleBaseService, "StapleBaseService") %>%
      add_xml_data(L203.NonStapleBaseService, "NonStapleBaseService") %>%
      add_precursors(MODULE_INPUTS) ->
      ag_an_demand_input.xml

    # Separate ag_an_demand_input.xml into 2 pieces

    create_xml("ag_an_demand_input_NonFood.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand %>% filter(grepl("NonFood", supplysector)), "Supplysector") %>%
      add_logit_tables_xml(L203.SubsectorAll_demand_nonfood, "SubsectorAll", "SubsectorLogit") %>%
      add_xml_data(L203.StubTech_demand_nonfood, "StubTech") %>%
      add_xml_data(L203.GlobalTechCoef_demand %>% filter(grepl("NonFood", sector.name)), "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand %>% filter(grepl("NonFood", sector.name)), "GlobalTechShrwt") %>%
      add_xml_data(L203.GlobalTechInterp_demand %>% filter(grepl("NonFood", sector.name)), "GlobalTechInterp") %>%
      add_xml_data(L203.StubTechProd_nonfood_crop, "StubTechProd") %>%
      add_xml_data(L203.StubTechProd_nonfood_meat, "StubTechProd") %>%
      add_xml_data(L203.StubTechProd_For, "StubTechProd") %>%
      add_xml_data(L203.PerCapitaBased, "PerCapitaBased") %>%
      add_xml_data(L203.BaseService, "BaseService") %>%
      add_xml_data(L203.IncomeElasticity, "IncomeElasticity") %>%
      add_xml_data(L203.PriceElasticity, "PriceElasticity") %>%
      add_precursors(MODULE_INPUTS) ->
      ag_an_demand_input_NonFood.xml

    create_xml("ag_an_demand_input_Food.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand %>% filter(!grepl("NonFood", supplysector)), "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_food,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_food, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_food, "SubsectorAll", "SubsectorLogit") %>%
      add_xml_data(L203.GlobalTechCoef_demand %>% filter(!grepl("NonFood", sector.name)), "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand %>% filter(!grepl("NonFood", sector.name)), "GlobalTechShrwt") %>%
      add_xml_data(L203.GlobalTechInterp_demand %>% filter(!grepl("NonFood", sector.name)), "GlobalTechInterp") %>%
      add_xml_data(L203.SubregionalShares, "SubregionalShares") %>%
      add_xml_data(L203.DemandFunction_food, "DemandFunction_food") %>%
      add_xml_data(L203.DemandStapleParams, "DemandStapleParams") %>%
      add_xml_data(L203.DemandNonStapleParams, "DemandNonStapleParams") %>%
      add_xml_data(L203.DemandStapleRegBias, "DemandStapleRegBias") %>%
      add_xml_data(L203.DemandNonStapleRegBias, "DemandNonStapleRegBias") %>%
      add_xml_data(L203.StapleBaseService, "StapleBaseService") %>%
      add_xml_data(L203.NonStapleBaseService, "NonStapleBaseService") %>%
      add_precursors(MODULE_INPUTS) ->
      ag_an_demand_input_Food.xml


    # adding waste pathways ----
    ## Adding SSP1 for CWF for now----
    # the pathways were developed based on per capital income vs. waste share
    # implying income elasticity of waste could increase

    L203.StubCalorieContent %>%
      left_join(
        L100.AgMIP_FoodWaste_Share_Pathway_SSP %>% filter(scenario == "gSSP1") %>%
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
      select(names(L203.StubCalorieContent)) ->
      L203.StubCalorieContent_WasteTrend

    create_xml("ag_an_demand_input_Food_Waste_SSP1.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand %>% filter(!grepl("NonFood", supplysector)), "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_food,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_food, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent_WasteTrend, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_food, "SubsectorAll", "SubsectorLogit") %>%
      add_xml_data(L203.GlobalTechCoef_demand %>% filter(!grepl("NonFood", sector.name)), "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand %>% filter(!grepl("NonFood", sector.name)), "GlobalTechShrwt") %>%
      add_xml_data(L203.GlobalTechInterp_demand %>% filter(!grepl("NonFood", sector.name)), "GlobalTechInterp") %>%
      add_xml_data(L203.SubregionalShares, "SubregionalShares") %>%
      add_xml_data(L203.DemandFunction_food, "DemandFunction_food") %>%
      add_xml_data(L203.DemandStapleParams, "DemandStapleParams") %>%
      add_xml_data(L203.DemandNonStapleParams, "DemandNonStapleParams") %>%
      add_xml_data(L203.DemandStapleRegBias, "DemandStapleRegBias") %>%
      add_xml_data(L203.DemandNonStapleRegBias, "DemandNonStapleRegBias") %>%
      add_xml_data(L203.StapleBaseService, "StapleBaseService") %>%
      add_xml_data(L203.NonStapleBaseService, "NonStapleBaseService") %>%
      add_precursors(MODULE_INPUTS) ->
      ag_an_demand_input_Food_Waste_SSP1.xml

    L203.StubCalorieContent %>%
      left_join(
        L100.AgMIP_FoodWaste_Share_Pathway_SSP %>% filter(scenario == "gSSP1") %>%
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
      select(names(L203.StubCalorieContent)) ->
      L203.StubCalorieContent_HalfWaste2050

    create_xml("ag_an_demand_input_Food_Waste_SSP1_HalfWaste2050.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand %>% filter(!grepl("NonFood", supplysector)), "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_food,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_food, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent_HalfWaste2050, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_food, "SubsectorAll", "SubsectorLogit") %>%
      add_xml_data(L203.GlobalTechCoef_demand %>% filter(!grepl("NonFood", sector.name)), "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand %>% filter(!grepl("NonFood", sector.name)), "GlobalTechShrwt") %>%
      add_xml_data(L203.GlobalTechInterp_demand %>% filter(!grepl("NonFood", sector.name)), "GlobalTechInterp") %>%
      add_xml_data(L203.SubregionalShares, "SubregionalShares") %>%
      add_xml_data(L203.DemandFunction_food, "DemandFunction_food") %>%
      add_xml_data(L203.DemandStapleParams, "DemandStapleParams") %>%
      add_xml_data(L203.DemandNonStapleParams, "DemandNonStapleParams") %>%
      add_xml_data(L203.DemandStapleRegBias, "DemandStapleRegBias") %>%
      add_xml_data(L203.DemandNonStapleRegBias, "DemandNonStapleRegBias") %>%
      add_xml_data(L203.StapleBaseService, "StapleBaseService") %>%
      add_xml_data(L203.NonStapleBaseService, "NonStapleBaseService") %>%
      add_precursors(MODULE_INPUTS) ->
      ag_an_demand_input_Food_Waste_SSP1_HalfWaste2050.xml


    ## Adding SSP2 for core for now----
    # the pathways were developed based on per capital income vs. waste share
    # implying income elasticity of waste could increase

    L203.StubCalorieContent %>%
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
      select(names(L203.StubCalorieContent)) ->
      L203.StubCalorieContent_WasteTrend

    create_xml("ag_an_demand_input_Food_Waste_SSP2.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand %>% filter(!grepl("NonFood", supplysector)), "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_food,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_food, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent_WasteTrend, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_food, "SubsectorAll", "SubsectorLogit") %>%
      add_xml_data(L203.GlobalTechCoef_demand %>% filter(!grepl("NonFood", sector.name)), "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand %>% filter(!grepl("NonFood", sector.name)), "GlobalTechShrwt") %>%
      add_xml_data(L203.GlobalTechInterp_demand %>% filter(!grepl("NonFood", sector.name)), "GlobalTechInterp") %>%
      add_xml_data(L203.SubregionalShares, "SubregionalShares") %>%
      add_xml_data(L203.DemandFunction_food, "DemandFunction_food") %>%
      add_xml_data(L203.DemandStapleParams, "DemandStapleParams") %>%
      add_xml_data(L203.DemandNonStapleParams, "DemandNonStapleParams") %>%
      add_xml_data(L203.DemandStapleRegBias, "DemandStapleRegBias") %>%
      add_xml_data(L203.DemandNonStapleRegBias, "DemandNonStapleRegBias") %>%
      add_xml_data(L203.StapleBaseService, "StapleBaseService") %>%
      add_xml_data(L203.NonStapleBaseService, "NonStapleBaseService") %>%
      add_precursors(MODULE_INPUTS) ->
      ag_an_demand_input_Food_Waste_SSP2.xml

    L203.StubCalorieContent %>%
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
      select(names(L203.StubCalorieContent)) ->
      L203.StubCalorieContent_HalfWaste2050

    create_xml("ag_an_demand_input_Food_Waste_SSP2_HalfWaste2050.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand %>% filter(!grepl("NonFood", supplysector)), "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_food,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_food, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent_HalfWaste2050, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_food, "SubsectorAll", "SubsectorLogit") %>%
      add_xml_data(L203.GlobalTechCoef_demand %>% filter(!grepl("NonFood", sector.name)), "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand %>% filter(!grepl("NonFood", sector.name)), "GlobalTechShrwt") %>%
      add_xml_data(L203.GlobalTechInterp_demand %>% filter(!grepl("NonFood", sector.name)), "GlobalTechInterp") %>%
      add_xml_data(L203.SubregionalShares, "SubregionalShares") %>%
      add_xml_data(L203.DemandFunction_food, "DemandFunction_food") %>%
      add_xml_data(L203.DemandStapleParams, "DemandStapleParams") %>%
      add_xml_data(L203.DemandNonStapleParams, "DemandNonStapleParams") %>%
      add_xml_data(L203.DemandStapleRegBias, "DemandStapleRegBias") %>%
      add_xml_data(L203.DemandNonStapleRegBias, "DemandNonStapleRegBias") %>%
      add_xml_data(L203.StapleBaseService, "StapleBaseService") %>%
      add_xml_data(L203.NonStapleBaseService, "NonStapleBaseService") %>%
      add_precursors(MODULE_INPUTS) ->
      ag_an_demand_input_Food_Waste_SSP2_HalfWaste2050.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}



