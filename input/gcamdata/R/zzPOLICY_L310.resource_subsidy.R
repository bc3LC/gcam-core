# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L310.resource_subsidy
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
#' @author RLH December 2023
module_policy_L310.resource_subsidy <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_resource_subsidy",
             outputs_of("module_energy_L254.transportation_UCD"),
             outputs_of("module_energy_L244.building_det")))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L310.RenewRsrc",
             "L310.RenewRsrcPrice",
             "L310.SmthRenewRsrcCurves",
             "L310.ResTechShrwt",
             # TRANSPORT
             "L310.GlobalTranTechShrwt",
             "L310.GlobalTranTechSCurve",
             "L310.StubTranTechLoadFactor",
             "L310.StubTranTechCost",
             "L310.StubTechTrackCapital",
             "L310.StubTranTechCalInput",
             "L310.StubTranTechShwtFuture",
             "L310.StubTranTechCoef",
             # BUILDINGS
             "L310.GlobalTechShrwt_bld",
             "L310.GlobalTechCost_bld",
             "L310.GlobalTechTrackCapital_bld",
             "L310.SubsectorShrwt",
             "L310.SubsectorLogit_bld",
             "L310.FuelPrefElast_bld",
             "L310.StubTech_bld",
             "L310.StubTechEff_bld",
             "L310.StubTechCoef_bld",
             "L310.StubTechCalInputNoShrwt_bld",
             "L310.StubTechShrwt_bld",
             "L310.StubTechIntGainOutputRatio"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_resource_subsidy <- get_data(all_data, "policy/A_resource_subsidy") %>%
      mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))

    # 1. Set resource basic info ------------------
    L310.RenewRsrc <- A_resource_subsidy %>%
      mutate(market = region,
             price.unit = "1975$/GJ") %>%
      select(xml, region, renewresource = subsidy.name, output.unit, price.unit, market)

    L310.RenewRsrcPrice <- L310.RenewRsrc %>%
      select(xml, region, renewresource) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      mutate(price = 0)

    L310.SmthRenewRsrcCurves <- A_resource_subsidy %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS), maxSubResource, mid.price = 0.0000001, curve.exponent = 4.0) %>%
      select(xml, region, renewresource = subsidy.name, smooth.renewable.subresource = subsidy.name,
             year.fillout, maxSubResource, mid.price, curve.exponent)

    L310.ResTechShrwt <- A_resource_subsidy %>%
      mutate(smooth.renewable.subresource = subsidy.name, technology = subsidy.name) %>%
      select(xml, region, renewresource = subsidy.name, smooth.renewable.subresource, technology, shareweight.year = year) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = if_else(year >= shareweight.year, 1, 0)) %>%
      select(-shareweight.year)

    # 2. Transportation tech processing - simple copying ---------------
    # Global tech - just replace technology name
    L310.GlobalTranTechShrwt <- get_data(all_data, "L254.GlobalTranTechShrwt") %>%
      semi_join(A_resource_subsidy, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "tranTechnology" = "tech.to.copy")) %>%
      left_join(distinct(A_resource_subsidy, xml, supplysector, subsector, tech.to.copy, new.tech.name),
                by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "tranTechnology" = "tech.to.copy")) %>%
      mutate(tranTechnology = new.tech.name,
             # ALWAYS WANT SHAREWEIGHT ZERO FOR THIS
             share.weight = 0) %>%
      select(-new.tech.name)

    L310.GlobalTranTechSCurve <- get_data(all_data, "L254.GlobalTranTechSCurve") %>%
      semi_join(A_resource_subsidy, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "tranTechnology" = "tech.to.copy")) %>%
      left_join(distinct(A_resource_subsidy, xml, supplysector, subsector, tech.to.copy, new.tech.name),
                by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "tranTechnology" = "tech.to.copy")) %>%
      mutate(tranTechnology = new.tech.name) %>%
      select(-new.tech.name)

    # Stub tech - just replace technology name, plus add fixedOutput
    L310.StubTranTechLoadFactor<- get_data(all_data, "L254.StubTranTechLoadFactor") %>%
      filter(sce == "CORE") %>%
      semi_join(A_resource_subsidy, by = c("region", "supplysector", "tranSubsector"  = "subsector", "stub.technology" = "tech.to.copy"))  %>%
      left_join(distinct(A_resource_subsidy, xml, region, supplysector, tranSubsector = subsector, tech.to.copy, new.tech.name),
                by = c("region", "supplysector", "tranSubsector", "stub.technology" = "tech.to.copy")) %>%
      mutate(stub.technology = new.tech.name) %>%
      select(-new.tech.name)

    L310.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost")  %>%
      filter(sce == "CORE") %>%
      semi_join(A_resource_subsidy, by = c("region", "supplysector",  "tranSubsector"  = "subsector", "stub.technology" = "tech.to.copy"))  %>%
      left_join(distinct(A_resource_subsidy, xml, region, supplysector, tranSubsector = subsector,
                         tech.to.copy, new.tech.name, non.energy.input.cost),
                by = c("region", "supplysector", "tranSubsector", "stub.technology" = "tech.to.copy")) %>%
      mutate(stub.technology = new.tech.name,
             input.cost = non.energy.input.cost) %>%
      select(-new.tech.name, -non.energy.input.cost)

    L310.StubTechTrackCapital <- get_data(all_data, "L254.StubTechTrackCapital")  %>%
      filter(sce == "CORE") %>%
      semi_join(A_resource_subsidy, by = c("region", "supplysector", "subsector", "stub.technology" = "tech.to.copy"))  %>%
      left_join(distinct(A_resource_subsidy, xml, region, supplysector, subsector, tech.to.copy, new.tech.name),
                by = c("region", "supplysector",  "subsector", "stub.technology" = "tech.to.copy")) %>%
      mutate(stub.technology = new.tech.name) %>%
      select(-new.tech.name)

    L310.StubTranTechCalInput <- get_data(all_data, "L254.StubTranTechCalInput")  %>%
      filter(sce == "CORE") %>%
      semi_join(A_resource_subsidy, by = c("region", "supplysector", "tranSubsector" = "subsector", "stub.technology" = "tech.to.copy"))  %>%
      left_join(distinct(A_resource_subsidy, xml, region, supplysector, tranSubsector = subsector, tech.to.copy, new.tech.name),
                by = c("region", "supplysector", "tranSubsector", "stub.technology" = "tech.to.copy")) %>%
      mutate(stub.technology = new.tech.name,
             calibrated.value = 0,
             tech.share.weight = 0) %>%
      select(-new.tech.name)

    L310.StubTranTechShwtFuture <- L310.StubTranTechCalInput %>%
      select(-calibrated.value, -share.weight.year, -subs.share.weight, -tech.share.weight, -year) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      left_join_error_no_match(A_resource_subsidy, by = c("region", "supplysector", "tranSubsector" = "subsector",
                                                          "stub.technology" = "new.tech.name", "xml")) %>%
      mutate(share.weight = if_else(year.x == year.y, shareweight, 0)) %>%
      select(xml, region, supplysector, tranSubsector, stub.technology, year = year.x, share.weight)

    # 3. Transportation tech processing - calculate coefficient ---------------
    L310.StubTranTechCoef_energy <- get_data(all_data, "L254.StubTranTechCoef") %>%
      filter(sce == "CORE") %>%
      semi_join(A_resource_subsidy, by = c("region", "supplysector", "tranSubsector" = "subsector", "stub.technology" = "tech.to.copy"))  %>%
      left_join(distinct(A_resource_subsidy, xml, region, supplysector, tranSubsector = subsector, tech.to.copy, new.tech.name),
                by = c("region", "supplysector", "tranSubsector", "stub.technology" = "tech.to.copy")) %>%
      mutate(stub.technology = new.tech.name) %>%
      select(-new.tech.name)

    L310.StubTranTechCoef_subsidy <- L310.StubTranTechCoef_energy %>%
      # Switch minicam energy input name to the subsidy
      distinct(xml, region, supplysector, tranSubsector, stub.technology, year, market.name, sce) %>%
      left_join_error_no_match(distinct(A_resource_subsidy, xml, region, subsidy.name, supplysector,
                                        tranSubsector = subsector, stub.technology = new.tech.name, coefficient),
                               by = c("region", "supplysector", "tranSubsector", "stub.technology", "xml")) %>%
      rename(minicam.energy.input = subsidy.name)

    L310.StubTranTechCoef <- bind_rows(L310.StubTranTechCoef_energy,
                                       L310.StubTranTechCoef_subsidy)

    # 4. Building heating/cooling processing -------
    # may not have subsector name to copy if creating new one
    subsector.to.copy <- A_resource_subsidy %>%
      filter(grepl("resid|comm", supplysector)) %>%
      left_join_error_no_match(get_data(all_data, "L244.StubTech_bld") %>%
                                 distinct(supplysector, subs.to.copy = subsector, stub.technology),
                               by = c("supplysector", "tech.to.copy" = "stub.technology"))
    # Global tech info
    L310.GlobalTechShrwt_bld <- get_data(all_data, "L244.GlobalTechShrwt_bld") %>%
      semi_join(subsector.to.copy, by = c("sector.name" = "supplysector", "subsector.name" = "subs.to.copy",
                                          "technology" = "tech.to.copy")) %>%
      left_join(distinct(subsector.to.copy, xml, supplysector, subsector, subs.to.copy, tech.to.copy, new.tech.name),
                by = c("sector.name" = "supplysector", "subsector.name" = "subs.to.copy", "technology" = "tech.to.copy")) %>%
      mutate(subsector.name = subsector,
             technology = new.tech.name,
             # ALWAYS WANT SHAREWEIGHT ZERO GLOBALLY
             share.weight = 0) %>%
      select(-subsector, -new.tech.name)

    L310.GlobalTechCost_bld <- get_data(all_data, "L244.GlobalTechCost_bld") %>%
      semi_join(subsector.to.copy, by = c("sector.name" = "supplysector", "subsector.name" = "subs.to.copy",
                                          "technology" = "tech.to.copy")) %>%
      left_join(distinct(subsector.to.copy, xml, supplysector, subsector, subs.to.copy, tech.to.copy, new.tech.name, non.energy.input.cost),
                by = c("sector.name" = "supplysector", "subsector.name" = "subs.to.copy", "technology" = "tech.to.copy")) %>%
      mutate(subsector.name = subsector,
             technology = new.tech.name,
             # ALWAYS WANT SHAREWEIGHT ZERO GLOBALLY
             input.cost = non.energy.input.cost) %>%
      select(-subsector, -new.tech.name, -non.energy.input.cost)

    L310.GlobalTechTrackCapital_bld <- get_data(all_data, "L244.GlobalTechTrackCapital_bld") %>%
      semi_join(subsector.to.copy, by = c("sector.name" = "supplysector", "subsector.name" = "subs.to.copy",
                                          "technology" = "tech.to.copy")) %>%
      left_join(distinct(subsector.to.copy, xml, supplysector, subsector, subs.to.copy, tech.to.copy, new.tech.name),
                by = c("sector.name" = "supplysector", "subsector.name" = "subs.to.copy", "technology" = "tech.to.copy")) %>%
      mutate(subsector.name = subsector,
             technology = new.tech.name) %>%
      select(-subsector, -new.tech.name) %>%
      # filter out EE techs
      filter(!grepl("EE", technology))

    # Stubtech info
    L310.SubsectorShrwt <- subsector.to.copy %>%
      select(xml, region, supplysector, subsector, year) %>%
      mutate(share.weight = 1) %>%
      complete(nesting(xml, region, supplysector, subsector), year = MODEL_YEARS) %>%
      replace_na(list(share.weight = 0)) %>%
      # don't want to change subsector shareweight if subsector already exists
      anti_join(get_data(all_data, "L244.SubsectorShrwtFllt_bld"), by = c("region", "supplysector", "subsector"))

    L310.SubsectorLogit_bld <- get_data(all_data, "L244.SubsectorLogit_bld") %>%
      semi_join(subsector.to.copy, by = c("region", "supplysector", "subsector" = "subs.to.copy")) %>%
      left_join(subsector.to.copy, by = c("region", "supplysector", "subsector" = "subs.to.copy")) %>%
      select(xml, region, supplysector, subsector = subsector.y, logit.year.fillout, logit.exponent, logit.type)

    L310.FuelPrefElast_bld <- get_data(all_data, "L244.FuelPrefElast_bld") %>%
      semi_join(subsector.to.copy, by = c("region", "supplysector", "subsector" = "subs.to.copy")) %>%
      left_join(subsector.to.copy, by = c("region", "supplysector", "subsector" = "subs.to.copy")) %>%
      select(xml, region, supplysector, subsector = subsector.y, year.fillout, fuelprefElasticity)

    L310.StubTech_bld <- subsector.to.copy %>%
      select(xml, region, supplysector, subsector, stub.technology = new.tech.name)

    L310.StubTechEff_bld <- get_data(all_data, "L244.StubTechEff_bld") %>%
      semi_join(subsector.to.copy, by = c("region", "supplysector", "subsector" = "subs.to.copy")) %>%
      left_join(subsector.to.copy, by = c("region", "supplysector", "subsector" = "subs.to.copy")) %>%
      select(xml, region, supplysector, subsector = subsector.y, stub.technology = new.tech.name, year = year.x,
             minicam.energy.input, efficiency, market.name) %>%
      filter(!grepl("EE", stub.technology))

    # add in the subsidy cost
    L310.StubTechCoef_bld <- subsector.to.copy %>%
      select(xml, region, supplysector, subsector, stub.technology = new.tech.name,
             minicam.energy.input = subsidy.name, coefficient) %>%
      mutate(market.name = region) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS))

    L310.StubTechCalInputNoShrwt_bld <- L310.StubTechEff_bld %>%
      mutate(calibrated.value = 0) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(-market.name, -efficiency)

    L310.StubTechShrwt_bld <- subsector.to.copy %>%
      select(xml, region, supplysector, subsector, stub.technology = new.tech.name, share.weight = shareweight, subsidy.year = year) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = if_else(year == subsidy.year, 1, 0)) %>%
      select(-subsidy.year)

    # Only relevant for other techs
    L310.StubTechIntGainOutputRatio <- get_data(all_data, "L244.StubTechIntGainOutputRatio") %>%
      semi_join(subsector.to.copy, by = c("region", "supplysector", "subsector" = "subs.to.copy")) %>%
      left_join(subsector.to.copy, by = c("region", "supplysector", "subsector" = "subs.to.copy")) %>%
      select(xml, region, supplysector, subsector = subsector.y, technology = new.tech.name, year = year.x,
             internal.gains.output.ratio, internal.gains.market.name  )


    # Produce outputs ------------------
    L310.RenewRsrc %>%
      add_title("Subsidy resource info", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.RenewRsrc

    L310.RenewRsrcPrice %>%
      add_title("Subsidy resource price", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.RenewRsrcPrice

    L310.SmthRenewRsrcCurves %>%
      add_title("Subsidy resource curves", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.SmthRenewRsrcCurves

    L310.ResTechShrwt %>%
      add_title("Subsidy resource shareweight", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.ResTechShrwt

    ######### TRANSPORTATIONS

    L310.GlobalTranTechShrwt %>%
      add_title("Subsidy resource tran tech shareweight", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy", "L254.GlobalTranTechShrwt") ->
      L310.GlobalTranTechShrwt

    L310.GlobalTranTechSCurve %>%
      add_title("Subsidy resource tran tech scurve", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy", "L254.GlobalTranTechSCurve") ->
      L310.GlobalTranTechSCurve

    L310.StubTranTechLoadFactor %>%
      add_title("Subsidy resource tran tech load factor", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy", "L254.StubTranTechLoadFactor") ->
      L310.StubTranTechLoadFactor

    L310.StubTranTechCost %>%
      add_title("Subsidy resource tran tech cost", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy", "L254.StubTranTechCost") ->
      L310.StubTranTechCost

    L310.StubTechTrackCapital %>%
      add_title("Subsidy resource tran tech capital", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy", "L254.StubTechTrackCapital") ->
      L310.StubTechTrackCapital

    L310.StubTranTechCalInput %>%
      add_title("Subsidy resource tran tech cal input", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy", "L254.StubTranTechCalInput") ->
      L310.StubTranTechCalInput

    L310.StubTranTechShwtFuture %>%
      add_title("Subsidy resource tran tech shareweights", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy", "L254.StubTranTechCalInput") ->
      L310.StubTranTechShwtFuture

    L310.StubTranTechCoef %>%
      add_title("Subsidy resource tran tech coef", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy", "L254.StubTranTechCoef") ->
      L310.StubTranTechCoef

    ######### BUILDINGS

    L310.GlobalTechShrwt_bld %>%
      add_title("L310.GlobalTechShrwt_bld", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.GlobalTechShrwt_bld

    L310.GlobalTechCost_bld %>%
      add_title("L310.GlobalTechCost_bld", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.GlobalTechCost_bld

    L310.GlobalTechTrackCapital_bld %>%
      add_title("L310.GlobalTechTrackCapital_bld", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.GlobalTechTrackCapital_bld

    L310.SubsectorShrwt %>%
      add_title("L310.SubsectorShrwt", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.SubsectorShrwt

    L310.SubsectorLogit_bld %>%
      add_title("L310.SubsectorLogit_bld", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.SubsectorLogit_bld

    L310.FuelPrefElast_bld %>%
      add_title("L310.FuelPrefElast_bld", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.FuelPrefElast_bld

    L310.StubTech_bld %>%
      add_title("L310.StubTech_bld", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.StubTech_bld

    L310.StubTechEff_bld %>%
      add_title("L310.StubTechEff_bld", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.StubTechEff_bld

    L310.StubTechCoef_bld %>%
      add_title("L310.StubTechCoef_bld", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.StubTechCoef_bld

    L310.StubTechCalInputNoShrwt_bld %>%
      add_title("L310.StubTechCalInputNoShrwt_bld", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.StubTechCalInputNoShrwt_bld

    L310.StubTechShrwt_bld %>%
      add_title("L310.StubTechShrwt_bld", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.StubTechShrwt_bld


    L310.StubTechIntGainOutputRatio %>%
      add_title("L310.StubTechIntGainOutputRatio", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_resource_subsidy") ->
      L310.StubTechIntGainOutputRatio


    return_data(L310.RenewRsrc, L310.RenewRsrcPrice,
                L310.SmthRenewRsrcCurves, L310.ResTechShrwt,
                # TRANSPORT
                L310.GlobalTranTechShrwt,
                L310.GlobalTranTechSCurve,
                L310.StubTranTechLoadFactor,
                L310.StubTranTechCost,
                L310.StubTechTrackCapital,
                L310.StubTranTechCalInput,
                L310.StubTranTechShwtFuture,
                L310.StubTranTechCoef,
                # BUILDINGS
                L310.GlobalTechShrwt_bld,
                L310.GlobalTechCost_bld,
                L310.GlobalTechTrackCapital_bld,
                L310.SubsectorShrwt,
                L310.SubsectorLogit_bld,
                L310.FuelPrefElast_bld,
                L310.StubTech_bld,
                L310.StubTechEff_bld,
                L310.StubTechCoef_bld,
                L310.StubTechCalInputNoShrwt_bld,
                L310.StubTechShrwt_bld,
                L310.StubTechIntGainOutputRatio)
  } else {
    stop("Unknown command")
  }
}
