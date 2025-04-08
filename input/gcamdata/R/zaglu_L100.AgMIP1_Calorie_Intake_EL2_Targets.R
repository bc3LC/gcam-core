# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L100.AgMIP1_Calorie_Intake_EL2_Targets
#'
#' Process AgMIP diet data to generate dietary change targets
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L100.AgMIP_EL2_intake_targets_foodgroup_r}
#' @details Process AgMIP diet data to generat dietary change scenarios.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else inner_join left_join mutate rename select
#' @importFrom tidyr  complete drop_na gather nesting spread replace_na
#' @importFrom tibble tibble
#' @author XZ 2022
module_aglu_L100.AgMIP1_Calorie_Intake_EL2_Targets <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "common/iso_GCAM_regID",
      FILE = "aglu/AgMIP/GCAM_AgMIP_food_group_mapping",
      FILE = "aglu/AgMIP/A_demand_supplysector_Food_ExoDiet",
      FILE = "aglu/AgMIP/EL2_foodgrouptargets_current_population_updated",
      "L100.Pop_thous_SSP_ctry_Yfut")

  MODULE_OUTPUTS <-
    c("L100.AgMIP_EL2_intake_targets_foodgroup_r")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # 1. AgMIP (Marco Springmann) food group targets: EAT Lancet 2.0 (EL2) ----
    ##  Target translation to GCAM region and sectors ----

    # Process EL2 target raw data
    # The base year was 2020, FLX is the EL2 main scenario
    # "fat_ani" was previously removed (seem target is zero)
    # but also due to complications in mapping
    # now we add it back for waste estimate. But fat is still under other meat!
    EL2_foodgrouptargets_current_population_updated %>%
      filter(Age == "all-a", Diet_scenario == "FLX",
             Unit == "kcal/d_w",
             Year == 2020,
             !Food_group %in% c("total")) %>%
      transmute(measure = Measure, diet_scenario = Diet_scenario, unit = Unit,
                food_group = Food_group, iso = tolower(Region),
                year = Year, value = Value) %>%
      spread(measure, value) %>%
      # DMA (& a few regions) didn't have 2020 value
      # the same is true for fat_ani, but pct = -100
      # so zero target
      replace_na(list(abs = 0)) %>%
      mutate(intake2020 = abs - chg) %>% #filter(is.na(intake2020))
      rename(targetEL2 = abs) %>%
      mutate(targetEL2_low = intake2020 + chg/2) %>%
      select(-chg, -pct, -year) %>%
      gather(measure, value, targetEL2, targetEL2_low, intake2020) ->
      AgMIP_foodgrouptargets1

    ## Join 2020 population to aggregate to GCAM region ----
    # 2020 population should be the same across SSPs
    L100.Pop_thous_SSP_ctry_Yfut %>%
      filter(year == 2020, scenario == "SSP2") %>%
      left_join_error_no_match(
        iso_GCAM_regID %>% distinct(iso, GCAM_region_ID), by = "iso") %>%
      select(-scenario, -year) %>% rename(weight = value) ->
      L100.Pop_2020

    AgMIP_foodgrouptargets1 %>%
      left_join(L100.Pop_2020, by = "iso") %>%
      # remove agg regions or small ones
      filter(!is.na(GCAM_region_ID)) %>%
      dplyr:::group_by_at(vars(-iso, -weight, -value)) %>%
      summarize(value = weighted.mean(w = weight, value), .groups = "drop") ->
      AgMIP_foodgrouptargets2

    ## Map to GCAM sector common sectors ----
    GCAM_AgMIP_food_group_mapping %>%
      distinct(AgMIP_food_group_agg) %>% pull -> COMM_AgMIP_food_group_agg

    # assert sector consistency
    assertthat::assert_that(
      A_demand_supplysector_Food_ExoDiet %>%
        distinct(supplysector) %>%
        transmute(AgMIP_food_group_agg = gsub("FoodDemand_NonStaples_|FoodDemand_", "", supplysector)) %>%
        left_join_error_no_match(
          GCAM_AgMIP_food_group_mapping %>%
            distinct(AgMIP_food_group_agg), by = "AgMIP_food_group_agg"
        ) %>% nrow() >0
    )


    AgMIP_foodgrouptargets2 %>%
      # spread to fill pork as 0 in Pakistan
      spread(GCAM_region_ID,value, fill = 0) %>%
      # will need to add TWN later
      gather(GCAM_region_ID, value, -diet_scenario:-measure) %>%
      left_join_error_no_match(
        GCAM_AgMIP_food_group_mapping %>%
          distinct(food_group = AgMIP_food_group, AgMIP_food_group_agg), by = "food_group") %>%
      dplyr::group_by_at(vars(-food_group, -value)) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      mutate(AgMIP_food_group_agg = factor(AgMIP_food_group_agg,
                                           levels = rev(COMM_AgMIP_food_group_agg))) %>%
      mutate(GCAM_region_ID = as.integer(GCAM_region_ID) ) ->
      L100.AgMIP_EL2_intake_targets_foodgroup_r


    ##  plot ----
    # library(ggplot2)
    # AgMIP_foodgrouptargets_GCAM  %>%
    #   filter(GCAM_region_ID %in% c(1, 2, 11)) %>%
    #   ggplot() + facet_wrap(~GCAM_region_ID) +
    #   geom_bar(aes(x = measure, y = value, fill = AgMIP_food_group_agg),
    #            stat = "identity", color = "black", size = 0.4) +
    #   labs(y = "kcal/ca/d") +
    #   theme_bw()

    # ***Done EL2 target translation to GCAM ----


    # Produce outputs ----
    #********************************* ----

    L100.AgMIP_EL2_intake_targets_foodgroup_r %>%
      add_title("AgMIP EAT-Lancet targets for dietary change scenarios") %>%
      add_units("NA") %>%
      add_comments("Generated dietary change targets per AgMIP EAT-Lancet targets") %>%
      add_precursors(MODULE_INPUTS) ->
      L100.AgMIP_EL2_intake_targets_foodgroup_r


    # Done & return data----
    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
