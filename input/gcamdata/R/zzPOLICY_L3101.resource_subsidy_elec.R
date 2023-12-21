# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L3101.resource_subsidy_elec
#'
#' Produce new elec techs that take in a subsidy as a resource
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L344.bld_shell}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH December 2023
module_policy_L3101.resource_subsidy_elec <- function(command, ...) {
  AUTOMATED_OUTPUTS <-  gsub("L223.|L2233.", "L3101.", inputs_of("module_water_electricity_water_xml"))

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_resource_subsidy_elec",
             inputs_of("module_water_electricity_water_xml")))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L3101.RenewRsrc",
             "L3101.RenewRsrcPrice",
             "L3101.SmthRenewRsrcCurves",
             "L3101.ResTechShrwt",
             "L3101.StubTechCoef_elec_cool",
             AUTOMATED_OUTPUTS))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_resource_subsidy_elec <- get_data(all_data, "policy/A_resource_subsidy_elec") %>%
      mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))

    # 1. Set resource basic info ------------------
    L3101.RenewRsrc <- A_resource_subsidy_elec %>%
      mutate(market = region,
             price.unit = "1975$/GJ") %>%
      select(xml, region, renewresource = subsidy.name, output.unit, price.unit, market)

    L3101.RenewRsrcPrice <- L3101.RenewRsrc %>%
      select(xml, region, renewresource) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      mutate(price = 0)

    L3101.SmthRenewRsrcCurves <- A_resource_subsidy_elec %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS), maxSubResource, mid.price = 0.0000001, curve.exponent = 4.0) %>%
      select(xml, region, renewresource = subsidy.name, smooth.renewable.subresource = subsidy.name,
             year.fillout, maxSubResource, mid.price, curve.exponent)

    L3101.ResTechShrwt <- A_resource_subsidy_elec %>%
      mutate(smooth.renewable.subresource = subsidy.name, technology = subsidy.name) %>%
      select(xml, region, renewresource = subsidy.name, smooth.renewable.subresource, technology, shareweight.year = year) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = if_else(year >= shareweight.year, 1, 0)) %>%
      select(-shareweight.year)

    # 2a. Function for automatic replacement

    replace_tech <- function(nm, df_subsidy = A_resource_subsidy_elec){
      df <- get(nm)
      tech_col <-  names(df)[which(grepl("technology", names(df)))]
      # this needs to be length one
      stopifnot(length(tech_col) == 1)

      if ("region" %in% names(df)){
        df_replace_tech <- df %>%
          rename("tech.to.copy" = .data[[tech_col]]) %>%
          semi_join(df_subsidy ,
                    by = c("region", "tech.to.copy")) %>%
          left_join(df_subsidy%>%  select(xml, region, supplysector, subsector, new.tech.name, tech.to.copy),
                    by = c("region", "supplysector", "subsector", "tech.to.copy")) %>%
          rename("{tech_col}" := new.tech.name) %>%
          select(-tech.to.copy)
      } else {
        df_replace_tech <- df %>%
          rename("tech.to.copy" = .data[[tech_col]]) %>%
          semi_join(df_subsidy ,
                    by = c("tech.to.copy")) %>%
          left_join(df_subsidy %>%  distinct(xml, supplysector, subsector, new.tech.name, tech.to.copy),
                    by = c("sector.name" = "supplysector","subsector.name" = "subsector", "tech.to.copy")) %>%
          rename("{tech_col}" := new.tech.name) %>%
          select(-tech.to.copy)
      }

      return(df_replace_tech)
    }

    # replace_tech_water <- function(nm, df_subsidy = A_resource_subsidy_elec){
    #   df <- get(nm)
    #
    #   if ("region" %in% names(df)){
    #     df_replace_tech <- df %>%
    #       semi_join(df_subsidy ,
    #                 by = c("region", "subsector" = "tech.to.copy")) %>%
    #       left_join(df_subsidy%>%  select(xml, region, supplysector, subsector, new.tech.name, tech.to.copy),
    #                 by = c("region", "supplysector", "subsector", "tech.to.copy")) %>%
    #       rename("{tech_col}" := new.tech.name) %>%
    #       select(-tech.to.copy)
    #   } else {
    #     df_replace_tech <- df %>%
    #       semi_join(df_subsidy ,
    #                 by = c( "subsector.name" ="tech.to.copy")) %>%
    #       left_join(df_subsidy %>%  distinct(xml, new.tech.name, tech.to.copy),
    #                 by = c("subsector.name" = "tech.to.copy"), keep = TRUE) %>%
    #       mutate(tech.to.copy = gsub("\\(","\\\\(", tech.to.copy),
    #              tech.to.copy = gsub("\\)","\\\\)", tech.to.copy),
    #              sector.name = stringr::str_replace_all(sector.name, tech.to.copy, new.tech.name),
    #              subsector.name = stringr::str_replace_all(subsector.name, tech.to.copy, new.tech.name),
    #              technology = stringr::str_replace_all(technology, tech.to.copy, new.tech.name) ) %>%
    #       select(-new.tech.name, -tech.to.copy)
    #   }
    #
    #   return(df_replace_tech)
    # }

    # 2b.Automatic tech fill in ---------------
    get_data_list(all_data, inputs_of("module_water_electricity_water_xml"))
    for (df_nm in inputs_of("module_water_electricity_water_xml")){
      if (any(grepl("technology", names(get(df_nm))))){
        assign(gsub("L223.|L2233.", "L3101.", df_nm), replace_tech(df_nm))
      } else {
        assign(gsub("L223.|L2233.", "L3101.", df_nm), get(df_nm)[0,] )
      }
    }

    # 2c. Manual tech adjustments --------------
    # Need to manually adjust shareweights, coefficient of subsidy, costs, and calibrated value
    # First shareweights, if in future we adjust water techs, may need to change these as well:
    #  L3101.GlobalTechShrwt_elec_cool, L3101.GlobalTechShrwt_elecPassthru, L3101.SubsectorShrwtFllt_elec_cool

    # Set global tech db shareweights to zero
    L3101.GlobalIntTechShrwt_elec_cool <- L3101.GlobalIntTechShrwt_elec_cool %>%
      mutate(share.weight = 0)

    # Set stubtech shrwt to 1 only in subsidy year
    L3101.StubTechShrwt_elec_cool <- A_resource_subsidy_elec %>%
      select(xml, region, supplysector, subsector, stub.technology = new.tech.name,
             year, share.weight = shareweight) %>%
      complete(nesting(xml, region, supplysector, subsector, stub.technology),
               year = MODEL_YEARS) %>%
      tidyr::replace_na(list(share.weight = 0))

    # Subsidy coefficient THIS IS A NEW OUTPUT AND NEEDS TO BE ADDED TO OUTPUT LIST SEPARATELY
    L3101.StubTechCoef_elec_cool <- A_resource_subsidy_elec %>%
      select(xml, region, supplysector, subsector, stub.technology = new.tech.name,
             minicam.energy.input = subsidy.name, coefficient) %>%
      mutate(market.name = region) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS))

    # Costs - just adjusting overnight capital
    L3101.StubTechCapital_elecPassthru <- L3101.StubTechCapital_elecPassthru %>%
      left_join_error_no_match(A_resource_subsidy_elec %>% select(-year),
                               by = c("xml", "region", "supplysector", "subsector",
                                      "stub.technology" = "new.tech.name")) %>%
      select(xml, region, supplysector, subsector, stub.technology, year,
             capital.overnight = `capital-overnight`, input.capital, fixed.charge.rate)


    # Set calibration to zero
    L3101.StubTechProd_elecPassthru <- L3101.StubTechProd_elecPassthru %>%
      mutate(calOutputValue = 0,
             tech.share.weight = 0)

    L3101.StubTechProd_elec_cool <- L3101.StubTechProd_elec_cool %>%
      mutate(calOutputValue = 0,
             tech.share.weight = 0)


    # Produce outputs ------------------
    L3101.RenewRsrc %>%
      add_title("Subsidy resource info", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy_elec") ->
      L3101.RenewRsrc

    L3101.RenewRsrcPrice %>%
      add_title("Subsidy resource price", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy_elec") ->
      L3101.RenewRsrcPrice

    L3101.SmthRenewRsrcCurves %>%
      add_title("Subsidy resource curves", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy_elec") ->
      L3101.SmthRenewRsrcCurves

    L3101.ResTechShrwt %>%
      add_title("Subsidy resource shareweight", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy_elec") ->
      L3101.ResTechShrwt

    L3101.StubTechCoef_elec_cool %>%
      add_title("Coefficient of subsidy in elec tech", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy_elec") ->
      L3101.StubTechCoef_elec_cool

    # Bad practice but just going to repeat same metadata for all other outputs
    for (nm in AUTOMATED_OUTPUTS){
      assign(nm, get(nm) %>%
               add_title(nm, overwrite = T) %>%
               add_units("Unitless") %>%
               add_precursors("policy/A_resource_subsidy_elec", gsub("L3101.", "L2233.", nm))
             )
    }

    return_data(c("L3101.RenewRsrc", "L3101.RenewRsrcPrice",
                "L3101.SmthRenewRsrcCurves", "L3101.ResTechShrwt", "L3101.StubTechCoef_elec_cool",
                AUTOMATED_OUTPUTS))
  } else {
    stop("Unknown command")
  }
}
