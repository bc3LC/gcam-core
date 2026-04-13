# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L132.industry_cogen
#'
#' Prepare EIA electricity cogeneration data for detailed industry processing
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L132.EIA_non_util_elec_USAind}.
#' @details Prepare EIA data to use for detailed industry USA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by left_join mutate select semi_join summarise
#' @importFrom tidyr complete nesting
#' @author MAW July 2023
module_gcamusa_L132.industry_cogen <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/EIA_non_util_elec_2005",
             FILE = "gcam-usa/EIA_non_util_elec_2010",
             FILE = "gcam-usa/EIA_non_util_elec_2015",
             FILE = "gcam-usa/EIA_non_util_elec_2021",
             FILE = "gcam-usa/EIA_agg_gcam_fuels",
             FILE = "gcam-usa/EIA_sector_mapping",
             FILE = "gcam-usa/A_fuel_conv"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L132.EIA_non_util_elec_USAind"))
  } else if(command == driver.MAKE) {

    # silence check package notes
    year <- value <- output_tot <- grid_region <- market.name <-
      calOutputValue <- calibrated.value <- calibration <- technology <-
      efficiency <- fuel <- minicam.energy.input <- object <-
      output_tot <- region <- secondary.output <- sector <- state <-
      subs.share.weight <- subsector <- supplysector <- value <- x <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    # and make sure each df has a complete year column
    # NOTE: When adding new data, be sure to update "gcamusa.EIA_923_DATA_YEARS" constant in gcamusa section of constants.R
    EIA_non_util_elec_2005 <- get_data(all_data, "gcam-usa/EIA_non_util_elec_2005", strip_attributes = TRUE)
    EIA_non_util_elec_2010 <- get_data(all_data, "gcam-usa/EIA_non_util_elec_2010", strip_attributes = TRUE)
    EIA_non_util_elec_2015 <- get_data(all_data, "gcam-usa/EIA_non_util_elec_2015", strip_attributes = TRUE)
    EIA_non_util_elec_2021 <- get_data(all_data, "gcam-usa/EIA_non_util_elec_2021", strip_attributes = TRUE)
    EIA_agg_gcam_fuels <- get_data(all_data, "gcam-usa/EIA_agg_gcam_fuels", strip_attributes = TRUE)
    EIA_sector_mapping <- get_data(all_data, "gcam-usa/EIA_sector_mapping", strip_attributes = TRUE)
    A_fuel_conv <- get_data(all_data, "gcam-usa/A_fuel_conv", strip_attributes = TRUE)

    # ===================================================
    # Data Processing
    ### 1. Pre-process the data - Get all input data in the same format
    # list of column names for the processed dataframe
    header_cols <- c("state", "eia_sector", "NAICS_code", "fuel", "total_fuel_mmbtu", "total_elec_fuel_mmbtu", "net_gen_mwh")

    # Update the header_map for new years of data if necessary
    # Match original_col name with new_col name so that all columns of interest have the proper header
    # The proper headers are: "tribal_name", "st_usps_cd", "scc", "description", "total_emissions", "uom", and "eis_facility_site_id"
    header_map <- data.frame(orignal_col = c("State", "Plant State","NAICS Code", "EIA Sector Number", "Reported Fuel Type Code",
                                              "TOTAL FUEL CONSUMPTION MMBTUS", "Total Fuel Consumption MMBtu",
                                              "ELEC FUEL CONSUMPTION MMBTUS", "Elec Fuel Consumption MMBtu",
                                              "NET GENERATION (megawatthours)", "Net Generation (Megawatthours)"),
                             new_col = c("state", "state", "NAICS_code", "eia_sector", "fuel", "total_fuel_mmbtu", "total_fuel_mmbtu",
                                          "total_elec_fuel_mmbtu", "total_elec_fuel_mmbtu", "net_gen_mwh", "net_gen_mwh"), stringsAsFactors = FALSE)

    # make a list of the EIA dataframes
    objlist <- ls(pattern = "*EIA_non_util_elec_*")
    all_dfs <- ls()[sapply(ls(), function(x) any(class(get(x)) == 'data.frame'))]
    dfs_list <-  mget(Reduce(intersect, list(objlist, all_dfs)))

    # change the column names
    lapply(dfs_list, function(x) {
      nms <- intersect(names(x), header_map$orignal_col)
      names(x)[ match(nms, names(x)) ] <- header_map$new_col[ match(nms, header_map$orignal_col) ]
      x
    }) -> dfs_list

    # select relevant columns
    dfs_list_columns <- lapply(dfs_list, function(x) x %>% select(all_of(header_cols)))

    all_dfs <- dfs_list_columns %>%
      # not all datasets had a full "year" column
      # we will get the year from the dataframe ID
    bind_rows(.id = "column_label") %>%
      separate(column_label, c("W", "X", "Y", "Z", "year"), sep = "_") %>%
      mutate(year = as.integer(year)) %>%
      select(header_cols, "year")

    # Select co-gen EIA sectors (excluding independent power producers who's
    # primary business is the sale of electricity to the public)
    eia_cogen_sector_numbers <- c("5", "7")

    ### 2. Process Data
    # Create a data frame, based off raw EIA data that only contains information for
    # cogen related fuels (coal, gas, refined liquids, and biomass)
    # Convert units, apportion "other" fuels, drop renewables, add sector names, etc.

    # First select relevant EIA cogen sectors and generate fuel distribution ratios
    # for standard cogen fuels (will be used to apportion "other" fuel)
    L132.EIA_non_util_elec_USAind_detailedFuel <- all_dfs %>%
      filter(eia_sector %in% eia_cogen_sector_numbers) %>%
      # map to GCAM fuels
      left_join_error_no_match(EIA_agg_gcam_fuels, c("fuel" = "eia_fuel")) %>%
      select(-c("fuel", "agg_fuel")) %>%
      rename(fuel = gcam_fuel) %>%
      # Drop all fuels except standard cogen fuels
      filter(fuel %in% c("coal", "gas", "biomass", "refined liquids")) %>%
      select(-eia_sector, -NAICS_code) %>%
      # Calculate state totals
      group_by(state, fuel, year) %>%
      mutate(state_total_fuel_mmbtu = sum(total_fuel_mmbtu),
             state_total_elec_fuel_mmbtu = sum(total_elec_fuel_mmbtu),
             state_total_fuel_net_gen_mwh = sum(net_gen_mwh)) %>%
      ungroup() %>% select(-total_fuel_mmbtu, -total_elec_fuel_mmbtu, -net_gen_mwh)%>% distinct() %>%
      group_by(state, year) %>%
      mutate(state_total_mmbtu = sum(state_total_fuel_mmbtu),
             state_total_elec_mmbtu = sum(state_total_elec_fuel_mmbtu),
             state_total_net_gen_mwh = sum(state_total_fuel_net_gen_mwh)) %>%
      ungroup() %>%
      # Generate state-fuel shares considering zero-value cases
      # Note EIA "Other" fuel subcategories cannot logically be attributed entirely
      # to a single GCAM cogen fuel. Instead, we will reallocate this energy proportionally
      # on the basis of relative fuel shares
      mutate(state_fuel_pct = if_else(is.na(state_total_fuel_mmbtu/state_total_mmbtu)==TRUE, 0, state_total_fuel_mmbtu/state_total_mmbtu),
             state_elec_fuel_pct = if_else(is.na(state_total_elec_fuel_mmbtu/state_total_elec_mmbtu)==TRUE, 0, state_total_elec_fuel_mmbtu/state_total_elec_mmbtu),
             state_net_gen_mwh_pct =  if_else(is.na(state_total_fuel_net_gen_mwh/state_total_net_gen_mwh)==TRUE, 0, state_total_fuel_net_gen_mwh/state_total_net_gen_mwh))

    # Clean deatiled fuel data frame to only include cogen fuel distribution %
    L132.cogen_fuel_dist <- L132.EIA_non_util_elec_USAind_detailedFuel %>%
      select(state, year, fuel, state_fuel_pct, state_elec_fuel_pct, state_net_gen_mwh_pct)

    # Find state totals for "other" fuel chp inputs
    L132.other_fuel <- all_dfs %>%
      filter(eia_sector %in% eia_cogen_sector_numbers) %>%
      # map to GCAM fuels
      left_join_error_no_match(EIA_agg_gcam_fuels, c("fuel" = "eia_fuel")) %>%
      select(-c("fuel", "agg_fuel")) %>%
      rename(fuel = gcam_fuel) %>%
      # Drop all fuels except standard cogen fuels
      filter(fuel =="other") %>%
      select(-eia_sector, -NAICS_code) %>%
      # Calculate state totals
      group_by(state, fuel, year) %>%
      mutate(state_total_otherfuel_mmbtu = sum(total_fuel_mmbtu),
             state_total_elec_otherfuel_mmbtu = sum(total_elec_fuel_mmbtu),
             state_total_otherfuel_net_gen_mwh = sum(net_gen_mwh)) %>%
      ungroup() %>% select(-fuel, -total_fuel_mmbtu, -total_elec_fuel_mmbtu, -net_gen_mwh)%>% distinct()

    # Distribute out "other" fuel energy consumption and chp elec generation
    # on the basis of standard cogen fuel distribution % (fuel mix excluding "other" and renewable fuels)
    # and convert from default EIA units (mmbtu-HHV) to GCAM unites (EJ-LHV)
    L132.EIA_non_util_elec_USAind_otherfuel <- L132.cogen_fuel_dist %>%
      left_join(L132.other_fuel, by = c("state", "year")) %>% #Generates NA's when cogen fuel shares are present but other fuel is not
      mutate(state_total_otherfuel_mmbtu = if_else(is.na(state_total_otherfuel_mmbtu)==TRUE, 0, state_total_otherfuel_mmbtu),
             state_total_elec_otherfuel_mmbtu = if_else(is.na(state_total_elec_otherfuel_mmbtu)==TRUE, 0, state_total_elec_otherfuel_mmbtu),
             state_total_otherfuel_net_gen_mwh = if_else(is.na(state_total_otherfuel_net_gen_mwh)==TRUE, 0, state_total_otherfuel_net_gen_mwh),
             total_fuel_mmbtu = state_fuel_pct * state_total_otherfuel_mmbtu,
             total_elec_fuel_mmbtu = state_elec_fuel_pct *state_total_elec_otherfuel_mmbtu,
             net_gen_mwh = state_net_gen_mwh_pct * state_total_otherfuel_net_gen_mwh) %>%
      select(c("state", "fuel", "total_fuel_mmbtu", "total_elec_fuel_mmbtu", "net_gen_mwh", "year")) %>%
      left_join(A_fuel_conv, by = "fuel") %>%  #BBTU(HHV) to EJ(LHV) conversion coefficients
      mutate(total_fuel_Bbtu = total_fuel_mmbtu * CONV_MIL_BIL,
             total_elec_fuel_Bbtu = total_elec_fuel_mmbtu  * CONV_MIL_BIL,
             total_fuel_EJ  = total_fuel_Bbtu * conv_Bbtu_EJ,
             total_elec_fuel_EJ = total_elec_fuel_Bbtu * conv_Bbtu_EJ,
             eia_sector = 0,
             sector_name = "other_fuel",
             NAICS_code = 0) %>%
      select(c("state", "eia_sector", "sector_name", "NAICS_code", "fuel", "total_fuel_EJ", "total_elec_fuel_EJ", "net_gen_mwh", "year"))

    # Create a dataframe for cogen fuels with converted units and added sector names.
    L132.EIA_non_util_elec_USAind_cogenfuel <- all_dfs %>%
      filter(eia_sector %in% eia_cogen_sector_numbers) %>%
      # map to GCAM fuels
      left_join_error_no_match(EIA_agg_gcam_fuels, c("fuel" = "eia_fuel")) %>%
      select(-c("fuel", "agg_fuel")) %>%
      rename(fuel = gcam_fuel) %>%
      # Drop all fuels except standard cogen fuels
      filter(fuel %in% c("coal", "gas", "biomass", "refined liquids")) %>%
      # convert EIA mmbtu units to EJ for data system compatibility
      # Note: also switching from high heating value(HHV) to low heating value (LHV)
      # =====================================================================
      # When quantifying the thermal energy produced by complete combustion of a
      # given fuel type, there are two standard conventions HHV and LHV. Water
      # is contained within combustible fuels. HHV includes the energy required
      # to vaporize the water within the fuel. LHV does not account for this
      # heat of vaporization for the embedded water.
      # =====================================================================
      left_join(A_fuel_conv, by = "fuel") %>%  #BBTU(HHV) to EJ(LHV) conversion coefficients
      mutate(total_fuel_Bbtu = total_fuel_mmbtu * CONV_MIL_BIL,
             total_elec_fuel_Bbtu = total_elec_fuel_mmbtu  * CONV_MIL_BIL,
             total_fuel_EJ  = total_fuel_Bbtu * conv_Bbtu_EJ,
             total_elec_fuel_EJ = total_elec_fuel_Bbtu * conv_Bbtu_EJ) %>%
      left_join_error_no_match(EIA_sector_mapping %>% select(eia_sector, sector_name), by = "eia_sector") %>%
      select(c("state", "eia_sector", "sector_name", "NAICS_code", "fuel", "total_fuel_EJ", "total_elec_fuel_EJ", "net_gen_mwh", "year"))

    # Join the distributed "other" fuels with the facility level, processed cogen fuel data
    L132.EIA_non_util_elec_USAind <- bind_rows(L132.EIA_non_util_elec_USAind_cogenfuel, L132.EIA_non_util_elec_USAind_otherfuel)

    # Update Input Data Check
    if(max(L132.EIA_non_util_elec_USAind$year) != MODEL_FINAL_BASE_YEAR){
      stop(paste0("Outdated Data: update EIA Form 923 data to the Base Year (", MODEL_FINAL_BASE_YEAR, ")"))
    }

    # ===================================================
    # Produce outputs

    L132.EIA_non_util_elec_USAind %>%
      add_title("Electric cogeneration from non-utilities in the USA, by state and fuel") %>%
      add_units("Units vary, in column titles") %>%
      add_comments("Generated using data from EIA") %>%
      add_precursors("gcam-usa/EIA_non_util_elec_2005", "gcam-usa/EIA_non_util_elec_2010",
                     "gcam-usa/EIA_non_util_elec_2015", "gcam-usa/EIA_non_util_elec_2021",
                     "gcam-usa/EIA_agg_gcam_fuels",
                     "gcam-usa/EIA_sector_mapping", "gcam-usa/A_fuel_conv") ->
      L132.EIA_non_util_elec_USAind


    return_data(L132.EIA_non_util_elec_USAind)
  } else {
    stop("Unknown command")
  }
}
