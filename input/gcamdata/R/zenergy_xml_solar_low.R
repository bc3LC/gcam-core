# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_solar_low_xml
#'
#' Construct XML data structure for \code{solar_low.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{solar_low.xml}. The corresponding file in the
#' original data system was \code{batch_solar_low_xml.R} (energy XML).
module_energy_solar_low_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.GlobalTechCapital_sol_low",
              "L223.GlobalIntTechCapital_sol_low",
             # for adjusting rooftop_pv for capital tracking purposes
             "L223.StubTechCapFactor_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "solar_low.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L223.GlobalTechCapital_sol_low <- get_data(all_data, "L223.GlobalTechCapital_sol_low")
    L223.GlobalIntTechCapital_sol_low <- get_data(all_data, "L223.GlobalIntTechCapital_sol_low")
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec")

    # This xml is empty if using EUREF data
    if (energy.ELEC_COST_SOURCE %in% c("EUREF", "WEO", "WEO-EUREF")){
      create_xml("solar_low.xml") ->
        solar_low.xml
    } else {
    # need to convert to standard non energy input for rooftop_pv for capital tracking purposes
    L223.StubTechCapFactor_elec %>%
      filter(stub.technology == "rooftop_pv") %>%
      left_join_error_no_match(L223.GlobalIntTechCapital_sol_low, by=c("supplysector" = "sector.name",
                                                                       "subsector" = "subsector.name",
                                                                       "stub.technology" = "intermittent.technology",
                                                                       "year")) %>%
      mutate(input.cost = capital.overnight * fixed.charge.rate / (capacity.factor * CONV_YEAR_HOURS * CONV_KWH_GJ)) %>%
      select(-capacity.factor, -capital.overnight, -fixed.charge.rate) %>%
      rename(minicam.non.energy.input = input.capital) ->
      rooftop_pv_low
    L223.GlobalIntTechCapital_sol_low %>%
      filter(subsector.name != "rooftop_pv") ->
      L223.GlobalIntTechCapital_sol_low

    # ===================================================

    # Produce outputs
    create_xml("solar_low.xml") %>%
      add_xml_data(L223.GlobalTechCapital_sol_low, "GlobalTechCapital") %>%
      add_xml_data(L223.GlobalIntTechCapital_sol_low, "GlobalIntTechCapital") %>%
      add_xml_data(rooftop_pv_low, "StubTechCost") %>%
      add_precursors("L223.GlobalTechCapital_sol_low", "L223.GlobalIntTechCapital_sol_low",
                     "L223.StubTechCapFactor_elec") ->
      solar_low.xml
    }
    return_data(solar_low.xml)
  } else {
    stop("Unknown command")
  }
}
