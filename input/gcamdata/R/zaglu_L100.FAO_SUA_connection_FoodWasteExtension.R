# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L100.FAO_SUA_connection_FoodWasteExtension
#'
#' Pull and further process SUA data needed
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{FAO_SUA_APE_balance}
#' @details Pull and further process SUA data needed. Calculate moving average if needed.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else inner_join left_join mutate rename select
#' @importFrom tidyr  complete drop_na gather nesting spread replace_na
#' @importFrom tibble tibble
#' @author XZ 2022
module_aglu_L100.FAO_SUA_connection_FoodWasteExtension <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/A_demand_food_staples",
      FILE = "aglu/A_demand_food_nonstaples",
      "DF_Macronutrient_FoodItem4",
      "L100.AgMIP_FoodWaste_Share_Pathway_SSP")

  MODULE_OUTPUTS <-
    c("L101.CropMeat_Food_Pcal_R_C_Y",
      "L100.demand_food_staples",
      "L100.demand_food_nonstaples")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    A_demand_food_staples -> L100.demand_food_staples
    A_demand_food_nonstaples -> L100.demand_food_nonstaples
    # If FoodWasteModel == TRUE, income elasticity will be updated (lower) and food calorie intake is represented
    # Note that food waste pathway will be needed in configuration & aglu_ag_an_demand_input_xml
    FoodWasteModel = TRUE
    # If False, the original food demand model representing calorie supply is used


    ##* L101.ag_Food_Pcal_R_C_Y ----
    L101.CropMeat_Food_Pcal_R_C_Y <-
      DF_Macronutrient_FoodItem4 %>%
      transmute(GCAM_region_ID, GCAM_commodity, year, value = MKcal/1000)

    ## 3.1 Food waste model  ----

    if (FoodWasteModel == TRUE) {

      L100.demand_food_staples %>% mutate(income.elesticity = 0.03) ->
        L100.demand_food_staples
      L100.demand_food_nonstaples%>% mutate(income.elesticity = 0.33) ->
        L100.demand_food_nonstaples

      ##* L101.ag_Food_Pcal_R_C_Y ----
      # Note that we don't have historical waste shares so applying 2015/2020 values
      # 2015 (MODEL_FINAL_BASE_YEAR) has the same base values so use gSSP2 here
      L101.CropMeat_Food_Pcal_R_C_Y <-
        DF_Macronutrient_FoodItem4 %>%
        left_join_error_no_match(
          # Base year food waste
          L100.AgMIP_FoodWaste_Share_Pathway_SSP %>%
            filter(year == MODEL_FINAL_BASE_YEAR, scenario == "gSSP2") %>% select(-year),
          by = c("GCAM_commodity", "GCAM_region_ID")
        ) %>%
        transmute(GCAM_region_ID, GCAM_commodity, year, value = MKcal/1000 * (1 - WasteShare))

      ##* L101.ag_Food_Pcal_R_C_Y_WithWaste ----
      L101.CropMeat_Food_Pcal_R_C_Y_WithWaste <-
        DF_Macronutrient_FoodItem4 %>%
        transmute(GCAM_region_ID, GCAM_commodity, year, value = MKcal/1000)

    }



    # Produce outputs ----
    #********************************* ----

    L101.CropMeat_Food_Pcal_R_C_Y %>%
      add_title("FAO food calories consumption by GCAM region, commodity, and year") %>%
      add_units("Pcal") %>%
      add_comments("Aggregates FAO data by GCAM region, commodity, and year") %>%
      add_comments("Data is also converted from tons to Pcal") %>%
      add_legacy_name("L101.CropMeat_Food_Pcal_R_C_Y") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "FAO_Food_Macronutrient_All_2010_2019",
                     "FAO_Food_MacronutrientRate_2010_2019_MaxValue",
                     "DF_Macronutrient_FoodItem4",
                     "L100.AgMIP_FoodWaste_Share_Pathway_SSP") ->
      L101.CropMeat_Food_Pcal_R_C_Y

    L100.demand_food_staples %>%
      add_title("Food demand parameters for staple food") %>%
      add_units("NA") %>%
      add_comments("Income elasticity is updated if food waste is modeled") %>%
      add_legacy_name("L100.demand_food_staples") %>%
      add_precursors("aglu/A_demand_food_staples") ->
      L100.demand_food_staples

    L100.demand_food_nonstaples %>%
      add_title("Food demand parameters for nonstaple food") %>%
      add_units("NA") %>%
      add_comments("Income elasticity is updated if food waste is modeled") %>%
      add_legacy_name("L100.demand_food_nonstaples") %>%
      add_precursors("aglu/A_demand_food_nonstaples") ->
      L100.demand_food_nonstaples


    # Done & return data----
    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
