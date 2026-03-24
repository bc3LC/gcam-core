# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L261.Cstorage
#'
#' Calculate carbon storage resource supply curves, shareweights, technology coefficients and costs, and other carbon storage information.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L261.Rsrc}, \code{L261.UnlimitRsrc}, \code{L261.RsrcCurves_C}, \code{L261.SectorLogitTables[[ curr_table ]]$data}, \code{L261.Supplysector_C}, \code{L261.SubsectorLogitTables[[ curr_table ]]$data}, \code{L261.SubsectorLogit_C}, \code{L261.SubsectorShrwtFllt_C}, \code{L261.StubTech_C}, \code{L261.GlobalTechCoef_C}, \code{L261.GlobalTechCost_C}, \code{L261.GlobalTechShrwt_C}, \code{L261.GlobalTechCost_C_High}, \code{L261.GlobalTechShrwt_C_nooffshore}, \code{L261.RsrcCurves_C_high}, \code{L261.RsrcCurves_C_low}, \code{L261.RsrcCurves_C_lowest}. The corresponding file in the
#' original data system was \code{L261.Cstorage.R} (energy level2).
#' @details The following tables pertaining to carbon storage properties are generated:
#' \itemize{
#'  \item{Carbon storage information}
#'  \item{Unlimited carbon storage information}
#'  \item{Supply curve of carbon storage resources}
#'  \item{High supply curve of onshore carbon storage resources}
#'  \item{Low supply curve of onshore carbon storage resources}
#'  \item{Lowest supply curve of onshore carbon storage resources}
#'  \item{Carbon storage sector information}
#'  \item{Subsector logit exponents of carbon storage sector}
#'  \item{Subsector shareweights of carbon storage sectors}
#'  \item{Identification of stub technologies of carbon storage}
#'  \item{Carbon storage global technology coefficients across base model years}
#'  \item{Carbon storage global technology costs across base model years}
#'  \item{Carbon storage global technology costs across base model years, high price scenario}
#'  \item{Shareweights of carbon storage technologies across base model years}
#'  \item{Shareweights of offshore carbon storage technologies}
#' }
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter mutate select
#' @importFrom tidyr complete nesting
#' @author AJS August 2017
module_energy_L261.Cstorage <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID",
             FILE = "energy/A61.rsrc_info",
             FILE = "energy/A61.sector",
             FILE = "energy/A61.subsector_logit",
             FILE = "energy/A61.subsector_shrwt",
             FILE = "energy/A61.globaltech_coef",
             FILE = "energy/A61.globaltech_cost",
             FILE = "energy/A61.globaltech_shrwt",
             FILE = "energy/A61.ResSubresourceProdLifetime",
             FILE = "energy/A61.ResReserveTechLifetime",
             FILE = "energy/A61.ResReserveTechDeclinePhase",
             FILE = "energy/A61.ResReserveTechProfitShutdown",
             FILE = "energy/A61.Cstorage_curves_dynamic",
             FILE = "energy/A61.globaltech_losses",
             FILE = "energy/IEA_CCUS_Projects_Database_2025",
             "L111.Prod_EJ_R_F_Yh",
             "L161.RsrcCurves_MtC_R",
             "L254.StubTranTechCalInput"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L261.Rsrc",
             "L261.UnlimitRsrc",
             "L261.RsrcCurves_C",
             "L261.ResTechShrwt_C",
             "L261.Supplysector_C",
             "L261.SubsectorLogit_C",
             "L261.SubsectorShrwtFllt_C",
             "L261.StubTech_C",
             "L261.GlobalTechCoef_C",
             "L261.GlobalTechCost_C",
             "L261.GlobalTechShrwt_C",
             "L261.GlobalTechCost_C_High",
             "L261.GlobalTechShrwt_C_nooffshore",
             "L261.RsrcCurves_C_high",
             "L261.RsrcCurves_C_low",
             "L261.RsrcCurves_C_lowest",
             "L261.ResSubresourceProdLifetime",
             "L261.ResReserveTechLifetime",
             "L261.ResReserveTechDeclinePhase",
             "L261.ResReserveTechProfitShutdown",
             "L261.ResReserveTechInvestmentInput",
             "L261.CStorageCurvesDynamic",
             "L261.DynamicCstorageRsrcMax",
             "L261.DynamicRsrc",
             "L261.DynamicResTechShrwt_C",
             "L261.RsrcPrice",
             "L261.StubTechEff",
             "L261.TechPmult",
             "L261.OutputEmissCoeff_C",
             "L261.DeleteNonCO2",
             "L261.StubTechShrwt",
             "L261.DeleteStubTech"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A61.rsrc_info <- get_data(all_data, "energy/A61.rsrc_info", strip_attributes = TRUE)
    A61.sector <- get_data(all_data, "energy/A61.sector", strip_attributes = TRUE)
    A61.subsector_logit <- get_data(all_data, "energy/A61.subsector_logit", strip_attributes = TRUE)
    A61.subsector_shrwt <- get_data(all_data, "energy/A61.subsector_shrwt", strip_attributes = TRUE)
    A61.globaltech_coef <- get_data(all_data, "energy/A61.globaltech_coef")
    A61.globaltech_losses <- get_data(all_data,"energy/A61.globaltech_losses")
    A61.globaltech_cost <- get_data(all_data, "energy/A61.globaltech_cost")
    A61.globaltech_shrwt <- get_data(all_data, "energy/A61.globaltech_shrwt", strip_attributes = TRUE)
    L161.RsrcCurves_MtC_R <- get_data(all_data, "L161.RsrcCurves_MtC_R", strip_attributes = TRUE)

    A61.ResSubresourceProdLifetime <- get_data(all_data, "energy/A61.ResSubresourceProdLifetime", strip_attributes = TRUE)
    A61.ResReserveTechLifetime <- get_data(all_data, "energy/A61.ResReserveTechLifetime", strip_attributes = TRUE)
    A61.ResReserveTechDeclinePhase <- get_data(all_data, "energy/A61.ResReserveTechDeclinePhase", strip_attributes = TRUE)
    A61.ResReserveTechProfitShutdown <- get_data(all_data, "energy/A61.ResReserveTechProfitShutdown", strip_attributes = TRUE)
    L111.Prod_EJ_R_F_Yh <- get_data(all_data, "L111.Prod_EJ_R_F_Yh", strip_attributes = TRUE)
    A61.Cstorage_curves_dynamic <- get_data(all_data, "energy/A61.Cstorage_curves_dynamic", strip_attributes = TRUE)

    L254.StubTranTechCalInput <- get_data(all_data, "L254.StubTranTechCalInput")

    IEA_CCUS_Projects_Database <- get_data(all_data, "energy/IEA_CCUS_Projects_Database_2025")

    IEA_data <- IEA_CCUS_Projects_Database %>%
      filter(`Project type` %in% c('Full chain','T&S','Storage'),
             !(`Project Status` %in% c('Suspended','Decommissioned','Cancelled')),
             !is.na(`Estimated capacity by IEA (Mt CO2/yr)`)) %>%
      group_by(Partners) %>%
      fill(Operation,.direction = 'downup') %>%
      ungroup() %>%
      filter(!is.na(`Operation`)) %>%
      group_by(Country,Operation,`Project Status`,`Fate of carbon`) %>%
      summarize(value = sum(`Estimated capacity by IEA (Mt CO2/yr)`)) %>%
      ungroup() %>%
      complete(Operation = c(1972:2035),nesting(Country,`Project Status`,`Fate of carbon`)) %>%
      mutate(value = if_else(is.na(value),0,value)) %>%
      group_by(Country,`Project Status`,`Fate of carbon`) %>%
      arrange(Operation) %>%
      mutate(value = cumsum(value)) %>%
      ungroup()

    IEA_data <- IEA_data %>%
      rename(year = Operation) %>%
      group_by(year,Country) %>%
      summarize(value = sum(value)) %>%
      ungroup()


    IEA_data <- IEA_data %>%
      rename(country_name = Country) %>%
      mutate(country_name = if_else(str_detect(country_name,'Korea'),'Korea, Republic of',
                                    if_else(str_detect(country_name,'United States'),'United States of America',
                                            if_else(country_name == "People's Republic of China",'China',
                                                    if_else(country_name == "Norway (storage), Belgium, Denmark, France, Germany, Latvia, the Netherlands, Poland, Sweden / Switzerland",'Norway',
                                                            if_else(str_detect(country_name,'Australia'),'Australia',
                                                                    if_else(str_detect(country_name,'Malaysia'),'Malaysia',
                                                                            if_else(country_name == "Chinese Taipei",'Taiwan',
                                                                                    if_else(country_name == "Lybia","Libyan Arab Jamahiriya",
                                                                                            if_else(country_name == "Island","Iceland",country_name))))))))),
             country_name = if_else(grepl("-",country_name),
                                    sub("-.*$", "", country_name),
                                    country_name)) %>%

      full_join(iso_GCAM_regID,by = c('country_name')) %>%
      filter(!is.na(value),
             year >= 2020) %>%
      group_by(year,GCAM_region_ID) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      complete(GCAM_region_ID = c(1:32),nesting(year)) %>%
      filter(!is.na(GCAM_region_ID)) %>%
      left_join_error_no_match(GCAM_region_names, by = c('GCAM_region_ID'))

    # ===================================================

    # Silence package notes
    . <- available <- capacity.factor <- curr_table <- extractioncost <-
      grade <- logit.type <- minicam.energy.input <- minicam.non.energy.input <-
      `output-unit` <- `price-unit` <- resource <- resource_type <- share.weight <-
      subresource <- subsector <- subsector.name <- supplysector <- technology <-
      value <- year <- region <- resource <- output.unit <- price.unit <-
      market <- logit.exponent <- coefficient <- input.cost <- NULL

    # Resource-reserve assumptions which just need to get copied to all regions and years
    A61.ResSubresourceProdLifetime %>%
      repeat_add_columns(GCAM_region_names) %>%
      select(LEVEL2_DATA_NAMES[["ResSubresourceProdLifetime"]]) ->
      L261.ResSubresourceProdLifetime

    A61.ResReserveTechLifetime %>%
      repeat_add_columns(GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["ResReserveTechLifetime"]]) ->
      L261.ResReserveTechLifetime

    A61.ResReserveTechDeclinePhase %>%
      repeat_add_columns(GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["ResReserveTechDeclinePhase"]]) ->
      L261.ResReserveTechDeclinePhase

    A61.ResReserveTechProfitShutdown %>%
      repeat_add_columns(GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["ResReserveTechProfitShutdown"]]) ->
      L261.ResReserveTechProfitShutdown

    #placeholder
    L261.ResReserveTechInvestmentInput <- L261.ResReserveTechDeclinePhase %>%
      mutate(minicam.non.energy.input = 'investment-cost',
             capital.coef = 1,
             tracking.market = 'capital') %>%
      select(LEVEL2_DATA_NAMES[["ResReserveTechInvestmentInput"]])

    OG_fluid_extraction_volume <- L111.Prod_EJ_R_F_Yh %>%
      filter(fuel %in% c('crude oil','natural gas'),
             technology != 'unconventional oil') %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(en_density = if_else(fuel == "natural gas",1/CONV_MT_EJ_GAS,
                                  if_else(fuel == "crude oil",1/CONV_MT_EJ_CRUDE_OIL,NA_real_)),
             mass_density = if_else(fuel == "natural gas",1/CONV_KG_M3_GAS,
                                    if_else(fuel == "crude oil",1/CONV_KG_M3_CRUDE_OIL,NA_real_)),
             OG_volumetric_flow_rate = value * en_density * mass_density,
             CO2_injection_MtCO2 = OG_volumetric_flow_rate * CONV_KG_M3_CO2) %>%
      group_by(year,region) %>%
      summarize(CO2_injection_MtC = sum(CO2_injection_MtCO2) / emissions.CONV_C_CO2) %>%
      ungroup() %>%
      group_by(region) %>%
      summarize(max_CO2_injection = max(CO2_injection_MtC)) %>%
      ungroup()
      #calculate each region's peak oil and gas production volumes in terms of an equivalent mass of CO2 at subsurface conditions

    USA_OG_volume_MTC <- OG_fluid_extraction_volume %>%
      filter(region == 'USA')

    USA_OG_volume_MTCO2 <- USA_OG_volume_MTC$max_CO2_injection[1] * emissions.CONV_C_CO2

    Cstorage_curves_dynamic <- A61.Cstorage_curves_dynamic %>%
      repeat_add_columns(GCAM_region_names) %>%
      rename(extractioncost = cost_2008USDtCO2) %>%
      mutate(extractioncost = extractioncost * gdp_deflator(1990,2008) * emissions.CONV_C_CO2) %>%
      left_join_error_no_match(OG_fluid_extraction_volume,by = c('region'))

    USA_max_CCS_rate_NETL <- Cstorage_curves_dynamic %>%
      filter(region == 'USA')

    USA_max_CCS_rate_NETL <- max(USA_max_CCS_rate_NETL$available_MtCO2_USA)


    fix_invalid_grades <- function(x, eps) {
      # x must already be ordered correctly (by grade)

      invalid <- function(v) {
        c(FALSE, v[-1] <= v[-length(v)])
      }

      iter <- 0
      max_iter <- length(x) * 10  # safety guard

      while (any(invalid(x))) {
        idx <- which(invalid(x))

        # bump invalid points just above previous value
        x[idx] <- x[idx - 1] + eps

        iter <- iter + 1
        if (iter > max_iter) {
          stop("Monotonic fix did not converge")
        }
      }

      x
    }

    eps <- 10^(-energy.DIGITS_RESOURCE)

    L261.CStorageCurvesDynamic <- Cstorage_curves_dynamic %>%
      mutate(available = max_CO2_injection * fraction  * USA_max_CCS_rate_NETL / USA_OG_volume_MTCO2, #scale injectivity back to US NETL data.  The result will be a supply curve that exactly matches NETL for USA, with other regions scaled based on relative O&G peak production volumes
             available = round(available,energy.DIGITS_RESOURCE),
             extractioncost = round(extractioncost,energy.DIGITS_COST)) %>%
      select(region, renewresource = resource, sub.renewable.resource = subresource, grade, available, extractioncost) %>%
      group_by(region,renewresource,sub.renewable.resource) %>%
      arrange(available, .by_group = TRUE) %>%
      #Rounding was leading to some invalid grades at the top and bottom of supply curves in regions with very small supply.
      #We identify those cases here and recalculate to ensure smooth monotonically increasing supply curves
      mutate(available = fix_invalid_grades(available, eps),
             available = round(available, energy.DIGITS_RESOURCE)) %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES[["GrdRenewRsrcCurves"]])
    #construct a supply curve based on fractions from NETL's saline storage cost model for the U.S. and then apply these fractions to max CO2 injectivity based on O&G volumetric flow rates

    k_rapid = 0.24 #shale gas growth in USA (EIA)
    k_med = 0.183 #flue gas desulphurization
    k_slow = 0.032 #gas pipeline growth rate from HATCH database

    CStorageCurvesDynamic_slow_growth <- L261.CStorageCurvesDynamic %>%
      mutate(scenario = 'slow growth rate',
             k = k_slow)

    CStorageCurvesDynamic_med_growth <- L261.CStorageCurvesDynamic %>%
      mutate(scenario = 'medium growth rate',
             k = k_med)

    CStorageCurvesDynamic_rapid_growth <- L261.CStorageCurvesDynamic %>%
      mutate(scenario = 'rapid growth rate',
             k = k_rapid) %>%
      group_by(region) %>%
      mutate(max_available = max(available)) %>%
      ungroup() %>%
      mutate(USA_OG_frac = max_available / max_available[region == 'USA'],
             available = if_else(USA_OG_frac <= 1/3, available * 3, available)) %>%
      select(-max_available,-USA_OG_frac)


    L261.CStorageCurvesDynamic <- bind_rows(CStorageCurvesDynamic_slow_growth,
                                            CStorageCurvesDynamic_rapid_growth,
                                            CStorageCurvesDynamic_med_growth)

    ## Calculate an efficiency parameter equal to how much of each region's implied storage capacity is expected to be consumed by planned + operational projects by 2030
    calibrated_eff_2030 <- IEA_data %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join(L261.CStorageCurvesDynamic, by = c('region')) %>%
      group_by(scenario,region) %>%
      mutate(capacity_MtCO2 = max(available) *  emissions.CONV_C_CO2) %>%
      ungroup() %>%
      mutate(efficiency = value / capacity_MtCO2) %>%
      mutate(efficiency = case_when(efficiency == 0 & year == 2030 ~ 0.001,
                                    is.na(efficiency) & year <= 2030 ~ 0.001,
                                    year <= MODEL_FINAL_BASE_YEAR ~ 0.001,
                                    efficiency > 1 ~ 1,
                                    TRUE~efficiency))

    # logistic fits for each region
    eff_post_2030 <- calibrated_eff_2030 %>%
      filter(year %in% MODEL_YEARS,
             grade == 'grade 7') %>%
      complete(year = c(year, MODEL_YEARS), nesting(region,scenario,k)) %>%
      group_by(scenario,region) %>%
      fill(efficiency, .direction = 'up') %>%
      mutate(C_2030 = efficiency[year == 2030],
             a = (1-C_2030)/C_2030,
             b = log(a)/-k,
             x0 = 2030 - b) %>%
      ungroup()

    L261.StubTechEff <- eff_post_2030 %>%
      mutate(efficiency = case_when(is.na(efficiency) & year > 2030 ~ (1/(1+exp(-k*(year - x0)))),
                                    year == 2035 ~ (1/(1+exp(-k*(year - x0)))),
                                    TRUE~efficiency),
             supplysector = 'ccs dynamic-capacity',
             subsector = 'ccs dynamic-capacity',
             stub.technology = 'ccs dynamic-capacity',
             minicam.energy.input = 'carbon-storage dynamic',
             market.name = region) %>%
      mutate(efficiency = round(efficiency,energy.DIGITS_EFFICIENCY),
             efficiency = if_else(efficiency == 0, 0.001,efficiency)) %>%
      select(c('scenario',LEVEL2_DATA_NAMES[['StubTechEff']])) -> L261.StubTechEff

    L261.TechPmult <- L261.StubTechEff %>%
      rename(technology = stub.technology,
             pMult = efficiency) %>%
      select(c('scenario',LEVEL2_DATA_NAMES[['TechPmult']]))

    # A
    # Create tables for carbon storage resource information
    # A61.rsrc_info provides carbon storage resource info (output unit, price unit, capacity factor, market, etc)
    A61.rsrc_info %>%
      # Expand table to incorporate GCAM region names (use ID to ensure correct region ordering)
      # We will use these specific region names to replace the broad term, regional, in the market column.
      repeat_add_columns(GCAM_region_names) %>%
      # Reset regional markets to the names of the specific regions
      mutate(market = replace(market, market == "regional", region[market == "regional"]),
             capacity.factor = as.numeric(capacity.factor)) %>%
      rename(output.unit = `output-unit`, price.unit = `price-unit`) ->
      L261.rsrc_info

    # Split different types of resources into separate tables

    # Create table reporting carbon storage information for unlimited resources only
    L261.rsrc_info %>%
      filter(resource_type == "unlimited-resource") %>%
      select(region, unlimited.resource = resource, output.unit, price.unit, market, capacity.factor) ->
      L261.UnlimitRsrc # This is a final ouput table.

    # Create table reporting carbon storage information for depletable resources only
    L261.rsrc_info %>%
      filter(resource_type == "resource") %>%
      select(region, resource = resource, output.unit, price.unit, market) ->
      L261.Rsrc # This is a final ouput table.

    L261.rsrc_info %>%
      filter(resource_type == "renewresource") %>%
      select(region, renewresource = resource, output.unit, price.unit, market) ->
      L261.DynamicRsrc # This is a final ouput table.

    L261.rsrc_info %>%
      filter(resource_type == "renewresource") %>%
      mutate(year=1975) %>%
      gather_years %>%
      complete(year = c(MODEL_YEARS), nesting(region, resource)) %>%
      select(region, renewresource=resource, year)  %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(price=0.001) ->
      L261.RsrcPrice

    # B
    # Supply curves of carbon storage resources
    # First, define number of decimal places
    DIGITS_COST <- 1

    # L161.RsrcCurves_MtC_R reports carbon storage resource supply curves by GCAM region.
    L161.RsrcCurves_MtC_R %>%
      # Match in GCAM region names using region ID
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(available = round(available, DIGITS_COST)) %>%
      select(region, resource = resource, subresource, grade, available, extractioncost) ->
      L261.RsrcCurves_C # This is a final output table.

    L261.RsrcCurves_C %>%
      select(region, resource = resource, subresource) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subresource,
             share.weight = 1.0) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      L261.ResTechShrwt_C

    L261.CStorageCurvesDynamic %>%
      select(region, resource = renewresource, subresource = sub.renewable.resource) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subresource,
             share.weight = 1.0) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      L261.DynamicResTechShrwt_C

    # Calculate three different supply curves of carbon storage resources: high, low, and lowest.
    # Multiply the extraction cost by its respective multiplier below.
    # Note that the multipliers were created for the SSPs and that high, low, and lowest is the level of CCS use and not cost.
    HI_CCS_COST_MULT <- 0.8

    LO_CCS_COST_MULT <- 3

    LOWEST_CCS_COST_MULT <- 10

    # Note that these will produce final output tables.
    # High supply curves of carbon storage resources
    L261.RsrcCurves_C_high <- mutate(L261.RsrcCurves_C, extractioncost = extractioncost * HI_CCS_COST_MULT)

    # Low supply curves of carbon storage resources
    L261.RsrcCurves_C_low <- mutate(L261.RsrcCurves_C, extractioncost = extractioncost * LO_CCS_COST_MULT)

    # Lowest supply curves of carbon storage resources
    L261.RsrcCurves_C_lowest <- mutate(L261.RsrcCurves_C, extractioncost = extractioncost * LOWEST_CCS_COST_MULT)


    # C
    # Carbon storage sector information
    A61.sector %>%
      mutate(logit.exponent = as.numeric(logit.exponent)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME),
                           GCAM_region_names = GCAM_region_names) ->
      L261.Supplysector_C  # This is a final output table.



    # D
    # Subsector information

    # Subsector logit exponents of carbon storage sector
    A61.subsector_logit %>%
      mutate(logit.exponent = as.numeric(logit.exponent)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME),
                           GCAM_region_names = GCAM_region_names) ->
      L261.SubsectorLogit_C # This is a final output table.


    # Subsector shareweights of carbon storage sectors
    A61.subsector_shrwt %>%
      mutate(share.weight = as.numeric(share.weight)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], LOGIT_TYPE_COLNAME),
                           GCAM_region_names = GCAM_region_names) ->
      L261.SubsectorShrwtFllt_C # This is a final output table.


    # E
    # Technology information
    # Identification of stub technologies of carbon storage
    # A61.globaltech_shrwt reports carbon storage technology shareweights
    # Note: assuming that the technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A61.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]],
                           GCAM_region_names = GCAM_region_names) %>%
      select(region, supplysector, subsector, stub.technology = technology) ->
      L261.StubTech_C # This is a final output table.

    # Energy inputs and coefficients of global technologies for carbon storage
    # A61.globaltech_coef reports carbon storage global technology coefficients
    A61.globaltech_coef %>%
      gather_years %>%
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology, minicam.energy.input)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(sector.name = supplysector, subsector.name = subsector, technology, year, minicam.energy.input, coefficient) ->
      L261.GlobalTechCoef_C

    # Adjustment to coefficients for losses
    L261.globaltech_losses <- gather_years(A61.globaltech_losses) %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, Non.CO2), year = MODEL_YEARS) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, Non.CO2) %>%
      mutate(multiplier = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      select(-value)

    L261.GlobalTechCoef_C <- left_join_error_no_match(L261.GlobalTechCoef_C, L261.globaltech_losses,
                                                    by = c(sector.name = "supplysector", subsector.name = "subsector", "technology", "minicam.energy.input", "year"),
                                                    ignore_columns = c("Non.CO2", "multiplier")) %>%
      mutate(coefficient = if_else(is.na(multiplier),
                                   coefficient,
                                   round(coefficient * (1 - multiplier), energy.DIGITS_COEFFICIENT))) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]])
    # This is a final output table.

    # Emissions coefficients
    # Emissions coefficients are read as region-specific data, so the default coefs need to be repeated by all regions
    L261.OutputEmissCoeff_C <- L261.globaltech_losses %>%
      mutate(emiss.coeff = round((multiplier), emissions.DIGITS_EMISS_COEF)) %>%
      rename(stub.technology = technology) %>%
      repeat_add_columns(tibble(GCAM_region_names["region"])) %>%
      select(LEVEL2_DATA_NAMES[["OutputEmissCoeff"]])

    L261.DeleteNonCO2 <- L261.OutputEmissCoeff_C %>%
      rename(period = year) %>%
      select(LEVEL2_DATA_NAMES[["DeleteNonCO2"]])

    # Costs of global technologies
    # A61.globaltech_cost reports carbon storage offshore storage cost (1975$/tCO2)
    A61.globaltech_cost %>%
      gather_years %>%
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology, minicam.non.energy.input)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector,subsector,technology,minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(sector.name = supplysector, subsector.name = subsector, technology, year, minicam.non.energy.input, input.cost) ->
      L261.GlobalTechCost_C # This is a final output table.

    # High costs of global technologies for carbon storage -- this prices out CCS
    L261.GlobalTechCost_C %>%
      mutate(subsector.name = "onshore carbon-storage",
             technology = "onshore carbon-storage") %>%
      bind_rows(L261.GlobalTechCost_C) %>%
      # Price out CCS by using storage cost that is very high (i.e., $10,000/tCO2)
      mutate(input.cost = 10000) -> # 1975$/tCO2
      L261.GlobalTechCost_C_High # This is a final output table.

    # Shareweights of global technologies for energy transformation
    A61.globaltech_shrwt %>%
      gather_years %>%
      group_by(supplysector, subsector, technology) %>%
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      arrange(year, .by_group = TRUE) %>%
      mutate(share.weight = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(sector.name = supplysector, subsector.name = subsector, technology, year, share.weight) ->
      L261.GlobalTechShrwt_C # This is a final output table.

    # Use zero shareweights for offshore storage
    L261.GlobalTechShrwt_C %>%
      filter(subsector.name == "offshore carbon-storage") %>%
      mutate(share.weight = 0) ->
      L261.GlobalTechShrwt_C_nooffshore # This is a final output table.

    L261.DynamicCstorageRsrcMax <- L261.CStorageCurvesDynamic %>%
      filter(grade == "grade 1") %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             maxSubResource = 1) %>%
      select(LEVEL2_DATA_NAMES[["maxSubResource"]])


    L261.StubTechShrwt <- L254.StubTranTechCalInput %>%
      filter(year == MODEL_FINAL_BASE_YEAR,
             sce == "CORE",
             supplysector %in% L261.GlobalTechCoef_C$minicam.energy.input) %>%
      group_by(region, supplysector, tranSubsector, year) %>%
      summarize(value = sum(calibrated.value)) %>%
      ungroup() %>%
      filter(value == 0) %>%
      select(-year) %>%
      left_join(L261.GlobalTechCoef_C, by = c("supplysector" = "minicam.energy.input")) %>%
      mutate(supplysector = sector.name, subsector = subsector.name, stub.technology = technology, share.weight = value) %>%
      select(LEVEL2_DATA_NAMES[["StubTechShrwt"]])

    L261.DeleteStubTech <- L261.StubTechShrwt %>%
      select(region,supplysector) %>%
      left_join(L261.GlobalTechCoef_C, by = c("supplysector" = "minicam.energy.input")) %>%
      distinct(region,supplysector = sector.name,subsector = subsector.name,stub.technology = technology)

    # ===================================================

    L261.Rsrc %>%
      add_title("Carbon storage information") %>%
      add_units("Output unit as listed (MtC), price unit as listed (1990$/tC)") %>%
      add_comments("Carbon storage resource information was expanded to include GCAM region names") %>%
      add_comments("and filtered for only depletable resources") %>%
      add_legacy_name("L261.Rsrc") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.rsrc_info") ->
      L261.Rsrc

    L261.DynamicRsrc %>%
      add_title("Carbon storage information") %>%
      add_units("Output unit as listed (MtC), price unit as listed (1990$/tC)") %>%
      add_comments("Carbon storage resource information was expanded to include GCAM region names") %>%
      add_comments("and filtered for only dynamic resources") %>%
      add_legacy_name("L261.DynamicRsrc") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.rsrc_info") ->
      L261.DynamicRsrc

    L261.UnlimitRsrc %>%
      add_title("Unlimited carbon storage information") %>%
      add_units("Output unit as listed (MtC), price unit as listed (1990$/tC), capacity factor is unitless") %>%
      add_comments("Carbon storage resource information was expanded to include GCAM region names") %>%
      add_comments("and filtered for only unlimited resources (i.e., offshore)") %>%
      add_legacy_name("L261.UnlimitRsrc") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.rsrc_info") ->
      L261.UnlimitRsrc

    L261.RsrcCurves_C %>%
      add_title("Supply curve of carbon storage resources") %>%
      add_units("Available in MtCO2, Extraction Cost in 1990$/tCO2") %>%
      add_comments("GCAM region names were added to the resource supply curves generated in level 1") %>%
      add_legacy_name("L261.RsrcCurves_C") %>%
      add_precursors("common/GCAM_region_names", "L161.RsrcCurves_MtC_R") ->
      L261.RsrcCurves_C

    L261.ResTechShrwt_C %>%
      add_title("Technology share-weights for the carbon storage resource") %>%
      add_units("NA") %>%
      add_comments("Mostly just to provide a shell of a technology for the resource to use") %>%
      same_precursors_as(L261.RsrcCurves_C) ->
      L261.ResTechShrwt_C

    L261.DynamicResTechShrwt_C %>%
      add_title("Technology share-weights for the carbon storage resource") %>%
      add_units("NA") %>%
      add_comments("Mostly just to provide a shell of a technology for the resource to use") %>%
      same_precursors_as(L261.RsrcCurves_C) ->
      L261.DynamicResTechShrwt_C

    L261.RsrcCurves_C_high %>%
      add_title("High supply curve of onshore carbon storage resources") %>%
      add_units("Available in MtCO2, Extraction Cost in 1990$/tCO2") %>%
      add_comments("A multiplier (based on high level of CCS use) was applied to the extraction cost to generate a high supply curve") %>%
      add_legacy_name("L261.RsrcCurves_C_high") %>%
      add_precursors("common/GCAM_region_names", "L161.RsrcCurves_MtC_R") ->
      L261.RsrcCurves_C_high

    L261.RsrcCurves_C_low %>%
      add_title("Low supply curve of onshore carbon storage resources") %>%
      add_units("Available in MtCO2, Extraction Cost in 1990$/tCO2") %>%
      add_comments("A multiplier (based on low level of CCS use) was applied to the extraction cost to generate a low supply curve") %>%
      add_legacy_name("L261.RsrcCurves_C_low") %>%
      add_precursors("common/GCAM_region_names", "L161.RsrcCurves_MtC_R") ->
      L261.RsrcCurves_C_low

    L261.RsrcCurves_C_lowest %>%
      add_title("Lowest supply curve of onshore carbon storage resources") %>%
      add_units("Available in MtCO2, Extraction Cost in 1990$/tCO2") %>%
      add_comments("A multiplier (based on lowest level of CCS use) was applied to the extraction cost to generate a lowest supply curve") %>%
      add_legacy_name("L261.RsrcCurves_C_lowest") %>%
      add_precursors("common/GCAM_region_names", "L161.RsrcCurves_MtC_R") ->
      L261.RsrcCurves_C_lowest

    L261.Supplysector_C %>%
      add_title("Carbon storage sector information") %>%
      add_units("Output, input, and price units are as listed; exponent is unitless") %>%
      add_comments("Carbon storage sector information was expanded to include GCAM region names") %>%
      add_legacy_name("L261.Supplysector_C") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.sector") ->
      L261.Supplysector_C

    L261.SubsectorLogit_C %>%
      add_title("Subsector logit exponents of carbon storage sector") %>%
      add_units("Unitless") %>%
      add_comments("Table on subsector logit exponents was expanded to include GCAM region names") %>%
      add_legacy_name("L261.SubsectorLogit_C") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.subsector_logit") ->
      L261.SubsectorLogit_C

    L261.SubsectorShrwtFllt_C %>%
      add_title("Subsector shareweights of carbon storage sectors") %>%
      add_units("Unitless") %>%
      add_comments("Table on subsector shareweights was expanded to include GCAM region names") %>%
      add_legacy_name("L261.SubsectorShrwtFllt_C") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.subsector_shrwt") ->
      L261.SubsectorShrwtFllt_C

    L261.StubTech_C %>%
      add_title("Identification of stub technologies of carbon storage") %>%
      add_units("Not Applicable") %>%
      add_comments("Technology list in the global shareweight table for carbon storage was expanded to include GCAM regions") %>%
      add_legacy_name("L261.StubTech_C") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.globaltech_shrwt") ->
      L261.StubTech_C

    L261.GlobalTechCoef_C %>%
      add_title("Carbon storage global technology coefficients across base model years") %>%
      add_units("Unitless") %>%
      add_comments("Global technology coefficients were interpolated across all base model years") %>%
      add_legacy_name("L261.GlobalTechCoef_C") %>%
      add_precursors("energy/A61.globaltech_coef") ->
      L261.GlobalTechCoef_C

    L261.GlobalTechCost_C %>%
      add_title("Carbon storage global technology costs across base model years") %>%
      add_units("1975$/tCO2") %>%
      add_comments("Global technology coefficients were interpolated across all base model years") %>%
      add_legacy_name("L261.GlobalTechCost_C") %>%
      add_precursors("energy/A61.globaltech_cost") ->
      L261.GlobalTechCost_C

    L261.GlobalTechCost_C_High %>%
      add_title("Carbon storage global technology costs across base model years (high price scenario)") %>%
      add_units("1975$/tCO2") %>%
      add_comments("Assigned onshore and offshore carbon storage technologies a high price to price out CCS") %>%
      add_legacy_name("L261.GlobalTechCost_C_High") %>%
      add_precursors("energy/A61.globaltech_cost") ->
      L261.GlobalTechCost_C_High

    L261.GlobalTechShrwt_C %>%
      add_title("Shareweights of carbon storage technologies across base model years") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights of global technologies for energy transformation were interpolated across all base model years") %>%
      add_legacy_name("L261.GlobalTechShrwt_C") %>%
      add_precursors("energy/A61.globaltech_shrwt") ->
      L261.GlobalTechShrwt_C

    L261.GlobalTechShrwt_C_nooffshore %>%
      add_title("Shareweights of offshore carbon storage technologies") %>%
      add_units("Unitless") %>%
      add_comments("Subset shareweight table for offshore only. Assigned them shareweights of zero") %>%
      add_legacy_name("L261.GlobalTechShrwt_C_nooffshore") %>%
      add_precursors("energy/A61.globaltech_shrwt") ->
      L261.GlobalTechShrwt_C_nooffshore

    L261.ResSubresourceProdLifetime %>%
      add_title("Average production lifetime for reserve subresource") %>%
      add_units("Years") %>%
      add_comments("Used to annualize production of the cumulative resource reserve") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.ResSubresourceProdLifetime") ->
      L261.ResSubresourceProdLifetime

    L261.ResReserveTechLifetime %>%
      add_title("Resource reserve technology lifetime") %>%
      add_units("Years") %>%
      add_comments("Resource well / mine lifetime over which the reserve will be produced / depleted") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.ResReserveTechLifetime") ->
      L261.ResReserveTechLifetime

    L261.ResReserveTechDeclinePhase %>%
      add_title("Resource reserve technology decline phase percent") %>%
      add_units("fraction") %>%
      add_comments("When the total reserve has been depleted to this percent the production") %>%
      add_comments("will move into a linear decline phase.") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.ResReserveTechDeclinePhase") ->
      L261.ResReserveTechDeclinePhase

    L261.ResReserveTechProfitShutdown %>%
      add_title("Resource reserve technology profit shutdown decider") %>%
      add_units("NA") %>%
      add_comments("Resource profit shutdown to characterize a well / mine's ability scale back") %>%
      add_comments("production under unprofitable conditions") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.ResReserveTechProfitShutdown") ->
      L261.ResReserveTechProfitShutdown

    L261.CStorageCurvesDynamic %>%
      add_title("Dynamic CCS curves based on annual rather than cumulative rates") %>%
      add_units("Available in MtC-yr^-1, Extraction Cost in 1990$/tC") %>%
      add_comments("Injectivity constraints based on NETL + historical O&G volumetric production data") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.Cstorage_curves_dynamic","L111.Prod_EJ_R_F_Yh") ->
      L261.CStorageCurvesDynamic

    L261.DynamicCstorageRsrcMax %>%
      add_title("Default max sub resource of dynamic CCS resources") %>%
      add_units("Unitless") %>%
      add_comments("Value of 1 assigned for all regions") %>%
      add_legacy_name("L261.DynamicCstorageRsrcMax") %>%
      same_precursors_as(L261.CStorageCurvesDynamic) ->
      L261.DynamicCstorageRsrcMax

    L261.RsrcPrice %>%
      add_title("Numerical zero price for dynamic storage resource") %>%
      add_units("1990$/tC") %>%
      add_comments("A61.rsrc_info written to all regions") %>%
      add_legacy_name("L261.RsrcPrice") %>%
      same_precursors_as(L261.rsrc_info) ->
      L261.RsrcPrice

    L261.OutputEmissCoeff_C %>%
      add_title("CO2 leakage coefficients") %>%
      add_units("kg of CO2 per kg CO2 input") %>%
      add_comments("calculated from the assumed losses") %>%
      same_precursors_as(L261.GlobalTechCoef_C) %>%
      add_precursors("energy/A61.globaltech_losses") ->
      L261.OutputEmissCoeff_C

    L261.DeleteNonCO2 %>%
      add_title("Delete nonCO2") %>%
      add_units("NA") %>%
      add_comments("need to do this so added nonCO2 coefs will work") %>%
      same_precursors_as(L261.OutputEmissCoeff_C) ->
      L261.DeleteNonCO2

    L261.StubTechEff %>%
      add_title("CCS efficiencies calibrated to near-term") %>%
      add_units("Unitless") %>%
      add_comments("Regionally calibrated scaling limits for CCS relative to maximum regional injection rate") %>%
      add_precursors("energy/IEA_CCUS_Projects_Database_2025","common/GCAM_region_names","common/iso_GCAM_regID") ->
      L261.StubTechEff

    L261.TechPmult %>%
      add_title("Scale down cost by factor of efficiency parameter") %>%
      add_units("Unitless") %>%
      add_comments("Make supply curves have same slope as original") %>%
      same_precursors_as(L261.StubTechEff) ->
      L261.TechPmult

    L261.ResReserveTechInvestmentInput %>%
      add_title("Placeholder values for capital tracking") %>%
      add_units("NA") %>%
      add_comments("NA") %>%
      same_precursors_as("L261.ResReserveTechDeclinePhase") ->
      L261.ResReserveTechInvestmentInput

      L261.StubTechShrwt %>%
        add_title("Zero out shareweights for transport sectors") %>%
        add_units("NA") %>%
        add_comments("NA") %>%
        add_precursors("L254.StubTranTechCalInput") ->
        L261.StubTechShrwt

      L261.DeleteStubTech %>%
        add_title("Delete technologies whos transport inputs have zero calibration values") %>%
        add_units("NA") %>%
        add_comments("NA") %>%
        add_precursors("L254.StubTranTechCalInput") ->
        L261.DeleteStubTech

    return_data(L261.Rsrc, L261.UnlimitRsrc, L261.RsrcCurves_C, L261.ResTechShrwt_C, L261.Supplysector_C, L261.SubsectorLogit_C, L261.SubsectorShrwtFllt_C, L261.StubTech_C, L261.GlobalTechCoef_C, L261.GlobalTechCost_C, L261.GlobalTechShrwt_C, L261.GlobalTechCost_C_High, L261.GlobalTechShrwt_C_nooffshore, L261.RsrcCurves_C_high, L261.RsrcCurves_C_low, L261.RsrcCurves_C_lowest,
                L261.ResSubresourceProdLifetime, L261.ResReserveTechLifetime, L261.ResReserveTechDeclinePhase, L261.ResReserveTechProfitShutdown,
                L261.ResReserveTechInvestmentInput,
                L261.CStorageCurvesDynamic,L261.DynamicCstorageRsrcMax,L261.DynamicRsrc,L261.DynamicResTechShrwt_C,L261.RsrcPrice,
                L261.StubTechEff,
                L261.TechPmult,
                L261.OutputEmissCoeff_C,L261.DeleteNonCO2,
                L261.StubTechShrwt,L261.DeleteStubTech)
  } else {
    stop("Unknown command")
  }
}
