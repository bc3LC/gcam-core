# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L123.Electricity
#'
#' Calculate electricity fuel consumption, electricity generation, and inputs and outputs of net ownuse
#' (the electricity used by production/transformation facilities) by state.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L123.in_EJ_state_elec_F}, \code{L123.out_EJ_state_elec_F}, \code{L123.in_EJ_state_ownuse_elec}, \code{L123.out_EJ_state_ownuse_elec}.
#' The corresponding file in the original data system was \code{LB123.Electricity.R} (gcam-usa level1).
#' @details By state, calculates electricity fuel consumption, electricity generation, and inputs and outputs of net ownuse.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join mutate select summarise transmute
#' @importFrom tidyr replace_na
#' @author RLH August 2017
module_gcamusa_L123.Electricity <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/NREL_us_re_technical_potential",
             "L123.in_EJ_R_elec_F_Yh",
             "L123.out_EJ_R_elec_F_Yh",
             "L126.in_EJ_R_elecownuse_F_Yh",
             "L126.out_EJ_R_elecownuse_F_Yh",
             "L101.inEIA_EJ_state_S_F",
             "L132.out_EJ_state_indchp_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L123.in_EJ_state_elec_F",
             "L123.out_EJ_state_elec_F",
             "L123.in_EJ_state_ownuse_elec",
             "L123.out_EJ_state_ownuse_elec"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    State <- state <- state_name <- GCAM_region_ID <- year <- value <- sector <-
      fuel <- CSP_GWh <- value.x <- value.y <- net_EJ_USA <- state_share <- NULL

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    NREL_us_re_technical_potential <- get_data(all_data, "gcam-usa/NREL_us_re_technical_potential") %>%
      # Remove TOTAL and add in state abbreviations
      filter(State != "TOTAL") %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state, state_name),
                               by = c("State" = "state_name"))
    L123.in_EJ_R_elec_F_Yh <- get_data(all_data, "L123.in_EJ_R_elec_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    L123.out_EJ_R_elec_F_Yh <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    L126.in_EJ_R_elecownuse_F_Yh <- get_data(all_data, "L126.in_EJ_R_elecownuse_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    L126.out_EJ_R_elecownuse_F_Yh <- get_data(all_data, "L126.out_EJ_R_elecownuse_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    L101.inEIA_EJ_state_S_F <- get_data(all_data, "L101.inEIA_EJ_state_S_F")
    L132.out_EJ_state_indchp_F <- get_data(all_data, "L132.out_EJ_state_indchp_F")

    # ===================================================
    # SEDS (EIA) indicates electricity generation technologies either in terms of fuel inputs or fuel outputs (not both)
    # ELECTRICITY_INPUT: coal, gas, oil, biomass
    # ELECTRICITY_OUTPUT: nuclear and renewables
    L123.pct_state_elec_F <- L101.inEIA_EJ_state_S_F %>%
      filter(sector %in% c("electricity_input", "electricity_output")) %>%
      # Compute each state's percentage, by fuel
      group_by(sector, fuel, year) %>%
      mutate(value = value / sum(value)) %>%
      ungroup() %>%
      # This is just PV solar, we will add in CSP next
      mutate(fuel = replace(fuel, fuel == "solar", "solar PV")) %>%
      replace_na(list(value = 0))

    # NOTE: SEDS does not disaggregate PV and CSP. Using solar shares for PV, and NREL data for CSP
    # Many states have zero CSP potential, and allocating production to these states will cause errors later on
    state_CSP_shares <- NREL_us_re_technical_potential %>%
      select(state, CSP_GWh) %>%
      # value = state share of total CSP potential
      transmute(state, value = CSP_GWh / sum(CSP_GWh))

    # Create L123.pct_state_elec_F values for CSP
    L123.pct_state_elec_CSP <- L123.pct_state_elec_F %>%
      select(-value) %>%
      filter(fuel == "solar PV") %>%
      mutate(fuel = "solar CSP") %>%
      left_join_error_no_match(state_CSP_shares, by = "state")

    L123.pct_state_elec_F <- bind_rows(L123.pct_state_elec_F, L123.pct_state_elec_CSP)

    # Electricity generation inputs by fuel and state
    # Allocating total energy input values to states using shares
    L123.in_EJ_state_elec_F <- L123.pct_state_elec_F %>%
      # L123.in_EJ_R_elec_F_Yh only has certain fuels
      filter(fuel %in% unique(L123.in_EJ_R_elec_F_Yh$fuel)) %>%
      left_join_error_no_match(L123.in_EJ_R_elec_F_Yh %>%
                                 select(fuel, year, value),
                               by = c("fuel", "year")) %>%
      # Multiplying state share by total value
      mutate(value = value.x * value.y,
             sector = "electricity generation") %>%
      select(-value.x, -value.y)

    # Electricity generation outputs by fuel and state
    # Allocating total electricity generation values to states using shares
    L123.out_EJ_state_elec_F <- L123.pct_state_elec_F %>%
      left_join_error_no_match(L123.out_EJ_R_elec_F_Yh %>%
                                 select(fuel, year, value),
                               by = c("fuel", "year")) %>%
      # Multiplying state share by total value
      mutate(value = value.x * value.y,
             sector = "electricity generation") %>%
      select(-value.x, -value.y)

    # ELECTRICITY - OWNUSE
    # First calculate the national own use quantity (loss = total generation - electricity delivered)
    L123.net_EJ_USA_ownuse <- L126.in_EJ_R_elecownuse_F_Yh %>%
      left_join_error_no_match(L126.out_EJ_R_elecownuse_F_Yh, by = c("sector", "fuel", "year")) %>%
      # Net value = input value - output value
      mutate(net_EJ_USA = value.x - value.y) %>%
      select(sector, year, net_EJ_USA)

    # The input of the electricity_net_ownuse sector is equal to sum of all generation (industrial CHP + electric sector).
    # This is computed first so that state generation shares (utility + CHP) can be used to distribute
    # the national ownuse loss below, keeping the ownuse allocation consistent with the CHP allocation.
    L123.in_EJ_state_ownuse_elec <- bind_rows(L123.out_EJ_state_elec_F, L132.out_EJ_state_indchp_F) %>%
      group_by(state, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(sector = "electricity ownuse",
             fuel = "electricity") %>%
      select(state, sector, fuel, year, value)

    # Distribute the national ownuse loss to states in proportion to each state's total electricity
    # generation (utility + CHP). This ensures the ownuse allocation is consistent with the CHP
    # state distribution (which uses EIA industrial fuel shares), so the grid-region electricity
    # balance closes correctly. Previously, a static EIA DirectUse_MWh share was used, which
    # diverged from the CHP state distribution and caused grid-region calibration mismatches.
    L123.net_EJ_state_ownuse_elec <- L123.in_EJ_state_ownuse_elec %>%
      group_by(sector, fuel, year) %>%
      mutate(state_share = value / sum(value)) %>%
      ungroup() %>%
      left_join_error_no_match(L123.net_EJ_USA_ownuse, by = c("sector", "year")) %>%
      mutate(value = state_share * net_EJ_USA) %>%
      select(state, sector, fuel, year, value)

    # Output of electricity_net_ownuse sector is equal to input minus ownuse "net" energy
    L123.out_EJ_state_ownuse_elec <- L123.in_EJ_state_ownuse_elec %>%
      left_join_error_no_match(L123.net_EJ_state_ownuse_elec, by = c("state", "sector", "fuel", "year")) %>%
      # Input value - net value
      mutate(value = value.x - value.y) %>%
      select(state, sector, fuel, year, value)
    # ===================================================

    # Produce outputs
    L123.in_EJ_state_elec_F %>%
      add_title("Electricity sector energy consumption by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("State fuel shares created from L101.inEIA_EJ_state_S_F multiplied by USA totals from L123.in_EJ_R_elec_F_Yh") %>%
      add_legacy_name("L123.in_EJ_state_elec_F") %>%
      add_precursors("L101.inEIA_EJ_state_S_F", "gcam-usa/NREL_us_re_technical_potential",
                     "gcam-usa/states_subregions", "L123.in_EJ_R_elec_F_Yh") ->
      L123.in_EJ_state_elec_F

    L123.out_EJ_state_elec_F %>%
      add_title("Electricity generation by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("State fuel shares created from L101.inEIA_EJ_state_S_F multiplied by USA totals from L123.out_EJ_R_elec_F_Yh") %>%
      add_legacy_name("L123.out_EJ_state_elec_F") %>%
      add_precursors("L101.inEIA_EJ_state_S_F", "gcam-usa/NREL_us_re_technical_potential",
                     "gcam-usa/states_subregions", "L123.out_EJ_R_elec_F_Yh") ->
      L123.out_EJ_state_elec_F

    L123.in_EJ_state_ownuse_elec %>%
      add_title("Input to electricity net ownuse by state") %>%
      add_units("EJ") %>%
      add_comments("Sum of all generation from L123.out_EJ_state_elec_F and L132.out_EJ_state_indchp_F") %>%
      add_legacy_name("L123.in_EJ_state_ownuse_elec") %>%
      add_precursors("L101.inEIA_EJ_state_S_F", "gcam-usa/NREL_us_re_technical_potential",
                     "gcam-usa/states_subregions", "L123.out_EJ_R_elec_F_Yh", "L132.out_EJ_state_indchp_F") ->
      L123.in_EJ_state_ownuse_elec

    L123.out_EJ_state_ownuse_elec %>%
      add_title("Output of electricity net ownuse by state") %>%
      add_units("EJ") %>%
      add_comments("Input values from L123.in_EJ_state_ownuse_elec subtracted by net values") %>%
      add_comments("Net ownuse loss distributed to states proportionally to total electricity generation (utility + CHP)") %>%
      add_legacy_name("L123.out_EJ_state_ownuse_elec") %>%
      add_precursors("L101.inEIA_EJ_state_S_F", "gcam-usa/NREL_us_re_technical_potential",
                     "gcam-usa/states_subregions", "L123.out_EJ_R_elec_F_Yh", "L132.out_EJ_state_indchp_F",
                     "L126.in_EJ_R_elecownuse_F_Yh", "L126.out_EJ_R_elecownuse_F_Yh")  ->
      L123.out_EJ_state_ownuse_elec

    return_data(L123.in_EJ_state_elec_F, L123.out_EJ_state_elec_F, L123.in_EJ_state_ownuse_elec, L123.out_EJ_state_ownuse_elec)
  } else {
    stop("Unknown command")
  }
}
