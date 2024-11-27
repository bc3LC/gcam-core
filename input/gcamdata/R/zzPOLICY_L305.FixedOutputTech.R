# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L305.FixedOutputTech
#'
#' Produce fuel standards coefficients
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L305.StubTechFixedOutput},\code{L305.StubTechLifetime}, \code{L305.GlbTechFixedOutput}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L305.FixedOutputTech <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_FixedOutputTech",
             FILE = "policy/A_FixedOutputTranTech",
             inputs_of("module_energy_transportation_UCD_CORE_xml")))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L305.StubTechFixedOutput",
             "L305.GlbTechFixedOutput",
             "L305.StubTechLifetime",
             "L305.StubTranTechFixedOutput",
             "L305.GlobalTranTechInterp",
             "L305.GlobalTranTechShrwt",
             "L305.GlobalTranTechSCurve",
             "L305.StubTranTechLoadFactor",
             "L305.StubTranTechCost",
             "L305.StubTechTrackCapital",
             "L305.StubTranTechCalInput",
             "L305.StubTranTechCoef"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_FixedOutputTech <- get_data(all_data, "policy/A_FixedOutputTech") %>%
      mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))
    A_FixedOutputTranTech <- get_data(all_data, "policy/A_FixedOutputTranTech") %>%
      mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))

    # 1. Regular tech processing ---------------
    L305.StubTechFixedOutput <- A_FixedOutputTech %>%
      select(-non.energy.input.cost, -lifetime.remove.start, -lifetime.remove.end) %>%
      gather_years(value_col = "fixedOutput") %>%
      filter(!is.na(fixedOutput)) %>%
      # Fill in missing years
      group_by(xml, region, supplysector, subsector, stub.technology) %>%
      # Interpolates between min and max years for each region/policy combo
      complete(nesting(xml, region, supplysector, subsector, stub.technology),
               year = seq(min(year), max(year), 5)) %>%
      mutate(fixedOutput = approx_fun(year, fixedOutput)) %>%
      ungroup

    L305.StubTechLifetime <- A_FixedOutputTech %>%
      select(xml, region, supplysector, subsector, stub.technology, lifetime.remove.start, lifetime.remove.end) %>%
      filter(!is.na(lifetime.remove.start),
             !is.na(lifetime.remove.end)) %>%
      tidyr::pivot_longer(cols = c(lifetime.remove.start, lifetime.remove.end), values_to = "year") %>%
      select(-name) %>%
      distinct() %>%
      # Fill in missing years
      group_by(xml, region, supplysector, subsector, stub.technology) %>%
      # Interpolates between min and max years for each region/policy combo
      complete(nesting(xml, region, supplysector, subsector, stub.technology),
               year = seq(min(year), max(year), 5)) %>%
      ungroup %>%
      mutate(lifetime = -1)


    L305.GlbTechFixedOutput <- A_FixedOutputTech %>%
      filter(create.tech == 1) %>%
      mutate(share.weight = 0,
             minicam.non.energy.input = "non-energy") %>%
      select(xml,
             sector.name = supplysector,
             subsector.name = subsector,
             technology = stub.technology,
             share.weight,
             minicam.non.energy.input,
             input.cost = non.energy.input.cost) %>%
      # Write to all model years
      repeat_add_columns(tibble(year = MODEL_YEARS))

    # 2. Transportation tech processing ---------------
    # Global tech - just replace technology name
    L305.GlobalTranTechInterp <- get_data(all_data, "L254.GlobalTranTechInterp") %>%
      semi_join(A_FixedOutputTranTech, by = c("sector.name" = "supplysector", "subsector.name" = "tranSubsector", "tranTechnology" = "tech.copy")) %>%
      left_join(distinct(A_FixedOutputTranTech, xml, supplysector, tranSubsector, tech.copy, stub.technology),
                by = c("sector.name" = "supplysector", "subsector.name" = "tranSubsector", "tranTechnology" = "tech.copy")) %>%
      mutate(tranTechnology = stub.technology) %>%
      select(-stub.technology)

    L305.GlobalTranTechShrwt <- get_data(all_data, "L254.GlobalTranTechShrwt") %>%
      semi_join(A_FixedOutputTranTech, by = c("sector.name" = "supplysector", "subsector.name" = "tranSubsector", "tranTechnology" = "tech.copy")) %>%
      left_join(distinct(A_FixedOutputTranTech, xml, supplysector, tranSubsector, tech.copy, stub.technology),
                               by = c("sector.name" = "supplysector", "subsector.name" = "tranSubsector", "tranTechnology" = "tech.copy")) %>%
      mutate(tranTechnology = stub.technology,
             # ALWAYS WANT SHAREWEIGHT ZERO FOR THIS
             share.weight = 0) %>%
      select(-stub.technology)

    L305.GlobalTranTechSCurve <- get_data(all_data, "L254.GlobalTranTechSCurve") %>%
      semi_join(A_FixedOutputTranTech, by = c("sector.name" = "supplysector", "subsector.name" = "tranSubsector", "tranTechnology" = "tech.copy")) %>%
      left_join(distinct(A_FixedOutputTranTech, xml, supplysector, tranSubsector, tech.copy, stub.technology),
                               by = c("sector.name" = "supplysector", "subsector.name" = "tranSubsector", "tranTechnology" = "tech.copy")) %>%
      mutate(tranTechnology = stub.technology) %>%
      select(-stub.technology)

    # Stub tech - just replace technology name, plus add fixedOutput
    L305.StubTranTechLoadFactor<- get_data(all_data, "L254.StubTranTechLoadFactor") %>%
      filter(sce == "CORE") %>%
      semi_join(A_FixedOutputTranTech, by = c("region", "supplysector", "tranSubsector", "stub.technology" = "tech.copy"))  %>%
      left_join(distinct(A_FixedOutputTranTech, xml, region, supplysector, tranSubsector, tech.copy, stub.technology),
                               by = c("region", "supplysector", "tranSubsector", "stub.technology" = "tech.copy")) %>%
      mutate(stub.technology = stub.technology.y) %>%
      select(-stub.technology.y)

    L305.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost")  %>%
      filter(sce == "CORE") %>%
      semi_join(A_FixedOutputTranTech, by = c("region", "supplysector", "tranSubsector", "stub.technology" = "tech.copy"))  %>%
      left_join(distinct(A_FixedOutputTranTech, xml, region, supplysector, tranSubsector, tech.copy, stub.technology),
                               by = c("region", "supplysector", "tranSubsector", "stub.technology" = "tech.copy")) %>%
      mutate(stub.technology = stub.technology.y) %>%
      select(-stub.technology.y)

    L305.StubTechTrackCapital <- get_data(all_data, "L254.StubTechTrackCapital")  %>%
      filter(sce == "CORE") %>%
      semi_join(A_FixedOutputTranTech, by = c("region", "supplysector", "subsector" = "tranSubsector", "stub.technology" = "tech.copy"))  %>%
      left_join(distinct(A_FixedOutputTranTech, xml, region, supplysector, tranSubsector, tech.copy, stub.technology),
                               by = c("region", "supplysector",  "subsector" ="tranSubsector", "stub.technology" = "tech.copy")) %>%
      mutate(stub.technology = stub.technology.y) %>%
      select(-stub.technology.y)

    L305.StubTranTechCalInput <- get_data(all_data, "L254.StubTranTechCalInput")  %>%
      filter(sce == "CORE") %>%
      semi_join(A_FixedOutputTranTech, by = c("region", "supplysector", "tranSubsector", "stub.technology" = "tech.copy"))  %>%
      left_join(distinct(A_FixedOutputTranTech, xml, region, supplysector, tranSubsector, tech.copy, stub.technology),
                               by = c("region", "supplysector", "tranSubsector", "stub.technology" = "tech.copy")) %>%
      mutate(stub.technology = stub.technology.y,
             calibrated.value = 0) %>%
      select(-stub.technology.y)

    L305.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef") %>%
      filter(sce == "CORE") %>%
      semi_join(A_FixedOutputTranTech, by = c("region", "supplysector", "tranSubsector", "stub.technology" = "tech.copy"))  %>%
      left_join(distinct(A_FixedOutputTranTech, xml, region, supplysector, tranSubsector, tech.copy, stub.technology),
                               by = c("region", "supplysector", "tranSubsector", "stub.technology" = "tech.copy")) %>%
      mutate(stub.technology = stub.technology.y) %>%
      select(-stub.technology.y)

    L305.StubTranTechFixedOutput <- A_FixedOutputTranTech %>%
      select(-create.tech, -tech.copy) %>%
      gather_years() %>%
      filter(!is.na(value)) %>%
      # Fill in missing years
      group_by(xml, region, supplysector, tranSubsector, stub.technology) %>%
      # Interpolates between min and max years for each region/policy combo
      complete(nesting(xml, region, supplysector, tranSubsector, stub.technology),
               year = seq(min(year), max(year), 5)) %>%
      # If group only has one, approx_fun doesn't work, so we use this workaround
      mutate(value_NA = as.numeric(approx_fun(year, value))) %>%
      ungroup %>%
      mutate(fixedOutput = if_else(!is.na(value_NA), value_NA, value)) %>%
      select(-value_NA, -value)


    # Produce outputs ---------------------
    L305.StubTechFixedOutput %>%
      add_title("Fixed Outputs by region and stub tech", overwrite = T) %>%
      add_units("EJ") %>%
      add_precursors("policy/A_FixedOutputTech") ->
      L305.StubTechFixedOutput

    L305.StubTechLifetime %>%
      add_title("Lifetime for techs", overwrite = T) %>%
      add_units("years") %>%
      add_precursors("policy/A_FixedOutputTech") ->
      L305.StubTechLifetime

    L305.GlbTechFixedOutput %>%
      add_title("Global tech db info for fixed output techs", overwrite = T) %>%
      add_units("1975$/GJ") %>%
      add_precursors("policy/A_FixedOutputTech") ->
      L305.GlbTechFixedOutput

    L305.StubTranTechFixedOutput  %>%
      add_title("Fixed transport Outputs by region and stub tech", overwrite = T) %>%
      add_units("EJ") %>%
      add_precursors("policy/A_FixedOutputTranTech") ->
      L305.StubTranTechFixedOutput

    return_data(L305.StubTechFixedOutput,
                L305.StubTechLifetime,
                L305.GlbTechFixedOutput,
                L305.StubTranTechFixedOutput,
                L305.GlobalTranTechInterp,
                L305.GlobalTranTechShrwt,
                L305.GlobalTranTechSCurve,
                L305.StubTranTechLoadFactor,
                L305.StubTranTechCost,
                L305.StubTechTrackCapital,
                L305.StubTranTechCalInput,
                L305.StubTranTechCoef)
  } else {
    stop("Unknown command")
  }
}

