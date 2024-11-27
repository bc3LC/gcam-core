# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L300.elasticity
#'
#' Produce new final energy elasticities
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L344.bld_shell}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 20123
module_policy_L300.elasticity <- function(command, ...) {
  INDUSTRY_INCELAS <- c(inputs_of("module_energy_iron_steel_incelas_SSP_xml"),
              inputs_of("module_energy_aluminum_incelas_SSP_xml"),
              inputs_of("module_energy_cement_incelas_SSP_xml"),
              inputs_of("module_energy_chemical_incelas_SSP_xml"),
              inputs_of("module_energy_other_industry_incelas_SSP_xml")
              )
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_elasticity",
             "L254.IncomeElasticity_trn",
             "L254.PriceElasticity_trn",
             "L254.PerCapitaBased_trn",
             INDUSTRY_INCELAS))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L300.elasticity_income",
             "L300.elasticity_price",
             "L300.PerCapitaBased_trn"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_elasticity <- get_data(all_data, "policy/A_elasticity") %>%
      mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))

    L254.IncomeElasticity_trn <- get_data(all_data, "L254.IncomeElasticity_trn") %>%
      tidyr::pivot_longer(income.elasticity, names_to = "elasticity.type")
    L254.PriceElasticity_trn <- get_data(all_data, "L254.PriceElasticity_trn") %>%
      tidyr::pivot_longer(price.elasticity, names_to = "elasticity.type")
    L254.PerCapitaBased_trn <- get_data(all_data, "L254.PerCapitaBased_trn")

    L300.all_incelas <- bind_rows(L254.IncomeElasticity_trn, L254.PriceElasticity_trn)
    # Load all industry incelas
    for (nm in INDUSTRY_INCELAS){
      nm_sce <- tail(strsplit(nm, "_")[[1]], 1)
      tmp <- get_data(all_data, nm) %>%
        tidyr::pivot_longer(income.elasticity, names_to = "elasticity.type") %>%
        mutate(sce = toupper(nm_sce))
      L300.all_incelas <- bind_rows(L300.all_incelas, tmp)
    }

    # Convert to long format and interpolate any missing years
    if (any(is.na(A_elasticity$SSP))){
      L300.elasticity_noSSP <- A_elasticity %>%
        filter(is.na(SSP)) %>%
        select(-SSP) %>%
        gather_years() %>%
        group_by(xml, region, energy.final.demand, elasticity.type) %>%
        # Interpolates between min and max years in A_aeei
        complete(nesting(xml, region, energy.final.demand, elasticity.type), year = seq(min(year), max(year), 5)) %>%
        # If group only has one, approx_fun doesn't work, so we use this workaround
        mutate(value_NA = as.numeric(approx_fun(year, value))) %>%
        ungroup %>%
        mutate(shell.conductance = if_else(!is.na(value_NA), value_NA, value)) %>%
        select(-value_NA, -value)
    } else { L300.elasticity_noSSP <- tibble(xml = character(), region = character(), energy.final.demand = character(),
                                             elasticity.type = character(), year = numeric(), value = numeric())}

    if (any(!is.na(A_elasticity$SSP))){
      L300.elasticity_SSP <- A_elasticity %>%
        filter(!is.na(SSP)) %>%
        mutate(SSP = toupper(SSP)) %>%
        select(xml, region,  energy.final.demand, elasticity.type, SSP) %>%
        gcamdata::repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
        left_join_error_no_match(L300.all_incelas, by = c("region", "energy.final.demand", "elasticity.type", "year", "SSP" = "sce")) %>%
        select(-SSP)

    }else { L300.elasticity_SSP <- tibble(xml = character(), region = character(), energy.final.demand = character(),
                                            elasticity.type = character(), year = numeric(), value = numeric())}

    L300.elasticity <- bind_rows(L300.elasticity_noSSP, L300.elasticity_SSP)

    L300.PerCapitaBased_trn <- L254.PerCapitaBased_trn %>%
      semi_join(L300.elasticity, by = c("region", "energy.final.demand")) %>%
      # shouldn't matter which sce we choose, since they are all the same
      distinct(region, energy.final.demand, perCapitaBased) %>%
      # add in xml name
      left_join_error_no_match(distinct(L300.elasticity, region, xml, energy.final.demand), by = c("region", "energy.final.demand"))


    L300.elasticity %>%
      filter(elasticity.type == "income.elasticity") %>%
      tidyr::pivot_wider(names_from = elasticity.type, values_from = value) %>%
      add_title("income elasticity overwrite", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_elasticity",
                     "L254.IncomeElasticity_trn") ->
        L300.elasticity_income

    L300.elasticity %>%
      filter(elasticity.type == "price.elasticity") %>%
      tidyr::pivot_wider(names_from = elasticity.type, values_from = value) %>%
      add_title("price elasticity overwrite", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_elasticity",
                     "L254.PriceElasticity_trn") ->
      L300.elasticity_price

    L300.PerCapitaBased_trn  %>%
      add_title("elasticity per-capita based", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_elasticity",
                     "L254.PerCapitaBased_trn") ->
      L300.PerCapitaBased_trn


    return_data(L300.elasticity_income,
                L300.elasticity_price,
                L300.PerCapitaBased_trn)
  } else {
    stop("Unknown command")
  }
}
