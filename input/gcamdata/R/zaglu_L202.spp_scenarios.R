# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L202.spp_scenarios
#'
#' Builds spp diet adoption
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs:
#'   \code{L202.spp_logisticFun} (aglu level2).
#' @details This chunk specifies the cumulative number of people by region that adopts the spp scenario tables for agriculture demand: generic information for supply sector, subsector and technology,
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by lag left_join mutate select summarise
#' @importFrom tidyr gather replace_na spread
#' @author CR July 2023
module_aglu_L202.spp_scenarios <- function(command, ...) {

  MODULE_INPUTS <-
    c("L201.spp_parameters")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return('L202.spp_logisticFun')
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- region_GCAM_IC <- GCAM_region_ID <- iso <- scenario <-
      region <- technology <- supplysector <- calOutputValue <- OtherUses_Mt <-
      subsector <- L202.spp_logisticFun <- NULL   # silence package check notes

    L201.spp_parameters <- get_data(all_data, "L201.spp_parameters")

    logit_function = function(data) {
      data = data %>%
        dplyr::mutate(A = spp_f - ((1+exp(-k*(MODEL_FINAL_BASE_YEAR-x0)))*(spp_f - spp_i)/(exp(-k*(MODEL_FINAL_BASE_YEAR-x0)) - exp(-k*(MODEL_HALF_CENTURY_YEAR-x0)))),
                      B = (1+exp(-k*(MODEL_FINAL_BASE_YEAR-x0)))*(1+exp(-k*(MODEL_HALF_CENTURY_YEAR-x0)))/(exp(-k*(MODEL_FINAL_BASE_YEAR-x0)) - exp(-k*(MODEL_HALF_CENTURY_YEAR-x0)))) %>%
        dplyr::mutate(value = A + B*((spp_f - spp_i)/(1 + exp(-k*(year-x0)))))
      return(invisible(data))
    }

    append_year = function(data, year) {
      res = cbind(data, year)
      return(invisible(res))
    }

    # Estimate the cumulative regional spp over time if not estimated yet
    if (!file.exists(paste0(outputs_path, '/spp/L202.spp_logisticFun.RData'))) {
      L202.spp_logisticFun = list()
      for (n in names(L201.spp_parameters)) {
        print(n)
        L202.spp_logisticFun_tmp = bind_rows(lapply(seq(MODEL_FINAL_BASE_YEAR,MODEL_HALF_CENTURY_YEAR,by=5), function(year) append_year(L201.spp_parameters[[n]], year))) %>%
          logit_function(.)
        L202.spp_logisticFun[[n]] = L202.spp_logisticFun_tmp
      }

      L202.spp_logisticFun %>% tibble::as_tibble() %>%
        add_legacy_name("L202.spp_logisticFun") %>%
        add_title("Plant Protein Share by region for all scenarios") %>%
        add_units("%") %>%
        add_comments("Estimation of the spp through different logit functions") %>%
        add_precursors("L201.spp_parameters") ->
        L202.spp_logisticFun

      if(!dir.exists(paste0(outputs_path, '/spp'))) dir.create(paste0(outputs_path, '/spp'))
      save(L202.spp_logisticFun, file = paste0(outputs_path, '/spp/L202.spp_logisticFun.RData'))
    } else {
      load(paste0(outputs_path, '/spp/L202.spp_logisticFun.RData'))
    }

    return_data(L202.spp_logisticFun)
  } else {
    stop("Unknown command")
  }
}
