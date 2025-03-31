# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_food_processing_xml
#'
#' Construct XML data structure for \code{food_processing.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{food_processing.xml}. The corresponding file in the
#' original data system was \code{batch_food_processing_xml.R} (energy XML).
module_energy_food_processing_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L2328.Supplysector_food",
      "L2328.FinalEnergyKeyword_food",
      "L2328.SubsectorLogit_food",
      "L2328.SubsectorShrwtFllt_food",
      "L2328.SubsectorInterp_food",
      "L2328.StubTech_food",
      "L2328.GlobalTechShrwt_food",
      "L2328.GlobalTechCoef_food",
      "L2328.GlobalTechCost_food",
      "L2328.StubTechCost_food",
      "L2328.GlobalTechTrackCapital_food",
      "L2328.GlobalTechSCurve_food",
      "L2328.GlobalTechProfitShutdown_food",
      "L2328.StubTechProd_food",
      "L2328.StubTechCalInput_food_heat",
      "L2328.StubTechCoef_food",
      "L2328.GlobalTechSecOut_food"
      # Moved to food demand xml e.g., module_aglu_ag_an_demand_input_xml
      # "L2328.StubCalorieContent",
      # "L2328.StubCaloriePriceConv"
      )

  MODULE_OUTPUTS <-
    c(XML = "food_processing.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================

    # Produce outputs
    create_xml("food_processing.xml") %>%
      add_logit_tables_xml(L2328.Supplysector_food, "Supplysector") %>%
      add_xml_data(L2328.FinalEnergyKeyword_food, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2328.SubsectorLogit_food, "SubsectorLogit") %>%
      add_xml_data(L2328.SubsectorShrwtFllt_food, "SubsectorShrwtFllt") %>%
      add_xml_data(L2328.SubsectorInterp_food, "SubsectorInterp") %>%
      add_xml_data(L2328.StubTech_food, "StubTech") %>%
      add_xml_data(L2328.GlobalTechShrwt_food, "GlobalTechShrwt") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L2328.GlobalTechCoef_food, "GlobalTechCoef") %>%
      add_xml_data(L2328.GlobalTechSCurve_food, "GlobalTechSCurve") %>%
      add_xml_data(L2328.GlobalTechProfitShutdown_food, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2328.GlobalTechTrackCapital_food, "GlobalTechTrackCapital") %>%
      add_xml_data(L2328.GlobalTechCost_food, "GlobalTechCost") %>%
      add_xml_data(L2328.StubTechCost_food, "StubTechCost") %>%
      add_xml_data(L2328.StubTechProd_food, "StubTechProd") %>%
      add_xml_data(L2328.StubTechCalInput_food_heat, "StubTechCalInput") %>%
      add_xml_data(L2328.StubTechCoef_food, "StubTechCoef") %>%
      add_xml_data(L2328.GlobalTechSecOut_food, "GlobalTechSecOut") %>%
      add_precursors(MODULE_INPUTS) ->
      food_processing.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
