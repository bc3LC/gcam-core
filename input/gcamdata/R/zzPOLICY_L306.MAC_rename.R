# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L306.MAC_rename
#'
#' Produce fuel standards coefficients
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L306.ResMAC_fos},\code{L252.MAC_higwp},\code{L306.AgMAC},\code{L306.MAC_an},\code{L306.ResMAC_fos_tc_average},\code{L306.ResMAC_fos_phaseInTime},\code{L306.MAC_higwp_tc_average},\code{L306.MAC_higwp_phaseInTime}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L306.MAC_rename <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "policy/A_MAC_rename",
                     FILE = "common/GCAM_region_names",
                     "L252.ResMAC_fos",
                     "L252.MAC_higwp",
                     "L252.AgMAC",
                     "L252.MAC_an",
                     "L252.ResMAC_fos_tc_average",
                     "L252.ResMAC_fos_phaseInTime",
                     "L252.MAC_higwp_tc_average",
                     "L252.MAC_higwp_phaseInTime")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L306.ResMAC_fos",
             "L306.MAC_higwp",
             "L306.AgMAC",
             "L306.MAC_an",
             "L306.ResMAC_fos_tc_average",
             "L306.ResMAC_fos_phaseInTime",
             "L306.MAC_higwp_tc_average",
             "L306.MAC_higwp_phaseInTime"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    A_MAC_rename <- A_MAC_rename %>% mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))

    if ("ALL" %in% A_MAC_rename$region){
      A_MAC_rename_global <- A_MAC_rename %>%
        filter(region == "ALL") %>%
        select(-region) %>%
        write_to_all_regions(c("xml", "region", "supplysector", "Non.CO2", "new.market.name"), GCAM_region_names)

      A_MAC_rename<- A_MAC_rename %>%
        filter(region != "ALL") %>%
        bind_rows(A_MAC_rename_global)
    }

    # function to filter and rename non-co2
    process_MAC_data <- function(data, rename_table = A_MAC_rename, sector_col) {
      sector_col_sym <- rlang::ensym(sector_col)  # Convert column name to symbol
      sector_col_str <- rlang::as_name(sector_col_sym)  # Convert symbol to string

      data %>%
        rename(supplysector = !!sector_col_sym) %>%  # Temporarily rename to "supplysector"
        semi_join(rename_table, by = c("region", "supplysector", "Non.CO2")) %>%
        left_join(rename_table, by = c("region", "supplysector", "Non.CO2")) %>%
        mutate(market.name = new.market.name) %>%
        select(-new.market.name) %>%
        rename(!!sector_col_sym := supplysector)  # Rename back to original column name
    }

    # For each MAC object, just need to filter it and rename Non-CO2
    L306.ResMAC_fos <- process_MAC_data(L252.ResMAC_fos, A_MAC_rename, resource)
    L306.MAC_higwp <- process_MAC_data(L252.MAC_higwp, A_MAC_rename, supplysector)
    L306.AgMAC <- process_MAC_data(L252.AgMAC, A_MAC_rename, AgSupplySector)
    L306.MAC_an <- process_MAC_data(L252.MAC_an, A_MAC_rename, supplysector)
    L306.ResMAC_fos_tc_average <- process_MAC_data(L252.ResMAC_fos_tc_average, A_MAC_rename, resource)
    L306.ResMAC_fos_phaseInTime <- process_MAC_data(L252.ResMAC_fos_phaseInTime, A_MAC_rename, resource)
    L306.MAC_higwp_tc_average <- process_MAC_data(L252.MAC_higwp_tc_average, A_MAC_rename, supplysector)
    L306.MAC_higwp_phaseInTime <- process_MAC_data(L252.MAC_higwp_phaseInTime, A_MAC_rename, supplysector)


    # Produce outputs
    return_data(L306.ResMAC_fos,
                L306.MAC_higwp,
                L306.AgMAC,
                L306.MAC_an,
                L306.ResMAC_fos_tc_average,
                L306.ResMAC_fos_phaseInTime,
                L306.MAC_higwp_tc_average,
                L306.MAC_higwp_phaseInTime)
  } else {
    stop("Unknown command")
  }
}

