# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L201.spp_parameters
#'
#' Builds SharePlantProtein diet parameters
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L201.spp_parameters}, \code{L201.spp_scenarios_names} (aglu level2).
#' @details This chunk computes the parameters to generate the cumulative number of plant protein intake by region by scenario
#' to modify GCAM's agriculture demand: generic information for supply sector, subsector and technology,
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by lag left_join mutate select summarise
#' @importFrom tidyr gather replace_na spread
#' @author CR July 2023
module_aglu_L201.spp_parameters <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/A_spp_parameters",
      FILE = "aglu/A_spp_scenarios")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.spp_parameters",
             "L201.spp_scenarios_names"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- region_GCAM3 <- GCAM_region_ID <- iso <- scenario <-
      region <- technology <- supplysector <- calOutputValue <- OtherUses_Mt <-
      subsector <- L201.spp_parameters <- NULL   # silence package check notes

    A_spp_parameters <- get_data(all_data, "aglu/A_spp_parameters")
    A_spp_scenarios <- get_data(all_data, "aglu/A_spp_scenarios")

    # sensitivity parameters (all possibilities)
    k_all = seq(from = 0.005, to = 1.6, by = 0.2)
    x0_all = seq(from = 2025, to = MODEL_HALF_CENTURY_YEAR, by = 2)
    combinations = expand.grid(x0 = x0_all, k = k_all)

    check_no_higher_100_and_no_diminishing = function(data) {
      data = data %>%
        # no higher than 100
        dplyr::mutate(spp_f = if_else(spp_f > 100, 100, spp_f)) %>%
        # spp_i <= spp_f
        dplyr::mutate(spp_f = if_else(spp_f < spp_i, spp_i, spp_f))
      return(invisible(data))
    }

    L201.spp_parameters_1 = list()
    for (i in 1:nrow(A_spp_scenarios)) {
      tmp_list = A_spp_parameters %>%
        dplyr::mutate(spp_f = eval(parse(text = A_spp_scenarios[i,]$spp_f_computation))) %>%
        dplyr::mutate(scenario = A_spp_scenarios[i,]$scen_name) %>%
        check_no_higher_100_and_no_diminishing(.)

      L201.spp_parameters_1[[A_spp_scenarios[i,]$scen_name]] = tmp_list
    }

    L201.spp_parameters_2 = list()
    for(i in 1:length(L201.spp_parameters_1)) {
      for(j in 1:nrow(combinations)) {
        sc = paste('spp',unique(L201.spp_parameters_1[[i]]$scenario),combinations[j,]$x0,combinations[j,]$k, sep = '_')
        tmp_list = cbind(L201.spp_parameters_1[[i]] %>%
                           dplyr::mutate(scenario = sc),
                         combinations[j,])
        L201.spp_parameters_2[[sc]] = tmp_list
      }
    }

    ## save results
    L201.spp_parameters_2 %>% tibble::as_tibble() %>%
      add_legacy_name("L201.spp_parameters") %>%
      add_precursors("aglu/A_spp_parameters") ->
      L201.spp_parameters

    paste0(names(L201.spp_parameters_2), ".xml") %>% tibble::as_tibble() %>%
      add_legacy_name("L201.spp_parameters") %>%
      add_precursors("aglu/A_spp_parameters") ->
      L201.spp_scenarios_names

    if(!dir.exists(paste0(outputs_path, '/spp'))) dir.create(paste0(outputs_path, '/spp'))
    save(L201.spp_parameters, file = paste0(outputs_path, '/spp/L201.spp_parameters.RData'))
    if(!dir.exists('output')) dir.create('output')
    save(L201.spp_scenarios_names, file = 'output/L201.spp_scenarios_names.RData')

    return_data(L201.spp_parameters,L201.spp_scenarios_names)
  } else {
    stop("Unknown command")
  }
}
