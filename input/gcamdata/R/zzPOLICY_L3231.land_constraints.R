# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L3231.land_constraints
#'
#' Produce land constraints for specified countries and years
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L3231.landConstrain_mngd_nodes}, \code{L3231.landConstrain_unmngd_nodes}, \code{L3231.landConstrain_constraint}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L3231.land_constraints <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_Land_Constraints",
             "L2231.LN3_MgdAllocation_noncrop",
             "L2231.LN3_UnmgdAllocation",
             "L222.LN2_MgdAllocation",
             "L222.LN2_UnmgdAllocation"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L3231.landConstrain_mngd_LN3",
             "L3231.landConstrain_unmngd_LN3",
             "L3231.landConstrain_mngd_LN2",
             "L3231.landConstrain_unmngd_LN2",
             "L3231.landConstrain"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_Land_Constraints <- get_data(all_data, "policy/A_Land_Constraints")
    L2231.LN3_MgdAllocation_noncrop <- get_data(all_data, "L2231.LN3_MgdAllocation_noncrop")
    L2231.LN3_UnmgdAllocation <- get_data(all_data, "L2231.LN3_UnmgdAllocation")
    L222.LN2_MgdAllocation <- get_data(all_data, "L222.LN2_MgdAllocation")
    L222.LN2_UnmgdAllocation <- get_data(all_data, "L222.LN2_UnmgdAllocation")

    L3231.landConstrain <- A_Land_Constraints %>%
      gather_years(value_col = "constraint") %>%
      filter(!is.na(constraint)) %>%
      group_by(xml, region, LandLeafs, policy.portfolio.standard, LandNode1_to_remove) %>%
      complete(nesting(xml, region, LandLeafs, policy.portfolio.standard, LandNode1_to_remove),
               year = seq(min(year), max(year), 5)) %>%
      # If group only has one, approx_fun doesn't work, so we use this workaround
      mutate(constraint_NA = approx_fun(year, constraint)) %>%
      ungroup %>%
      mutate(constraint = if_else(!is.na(constraint_NA), constraint_NA, constraint),
             policyType = "subsidy",
             market = region) %>%
      select(-constraint_NA)

    # Set filters for LandNode1s to remove and LandLeafs to keep
    LandNode1_filters <- na.omit(distinct(L3231.landConstrain, LandNode1_to_remove))$LandNode1_to_remove
    LandNode1_remove <- strsplit(LandNode1_filters, ";") %>% unlist() %>% paste(collapse="|")

    LandLeafs_to_keep <- distinct(L3231.landConstrain, region, LandLeafs_to_keep = LandLeafs, policy.portfolio.standard) %>%
      mutate(LandLeafs_to_keep = gsub(";", "|", LandLeafs_to_keep))

    # Get managed land nodes and add land-constraint-policy name
    L3231.landConstrain_mngd_LN3 <- L2231.LN3_MgdAllocation_noncrop %>%
      inner_join(LandLeafs_to_keep, by = "region") %>%
      # Filter if LandLeafs_to_keep is in LandLeaf
      dplyr::rowwise() %>%
      filter(grepl(LandLeafs_to_keep, LandLeaf)) %>%
      ungroup %>%
      distinct(region, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, LandLeaf, policy.portfolio.standard) %>%
      left_join_error_no_match(distinct(L3231.landConstrain, region, policy.portfolio.standard),
                               by = c("region", "policy.portfolio.standard")) %>%
      # Remove any undesired land nodes
      filter(!grepl(LandNode1_remove, LandNode1)) %>%
      rename(land.constraint.policy = policy.portfolio.standard)

    # Now repeat for unmanaged land nodes
    L3231.landConstrain_unmngd_LN3 <- L2231.LN3_UnmgdAllocation %>%
      inner_join(LandLeafs_to_keep, by = "region") %>%
      # Filter if LandLeafs_to_keep is in LandLeaf
      dplyr::rowwise() %>%
      filter(grepl(LandLeafs_to_keep, UnmanagedLandLeaf)) %>%
      ungroup %>%
      distinct(region, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, UnmanagedLandLeaf, policy.portfolio.standard) %>%
      left_join_error_no_match(distinct(L3231.landConstrain, region,policy.portfolio.standard),
                               by = c("region", "policy.portfolio.standard")) %>%
      # Remove any undesired land nodes
      filter(!grepl(LandNode1_remove, LandNode1)) %>%
      rename(land.constraint.policy = policy.portfolio.standard)

    L3231.landConstrain_mngd_LN2 <- L222.LN2_MgdAllocation %>%
      inner_join(LandLeafs_to_keep, by = "region") %>%
      # Filter if LandLeafs_to_keep is in LandLeaf
      dplyr::rowwise() %>%
      filter(grepl(LandLeafs_to_keep, LandLeaf)) %>%
      ungroup %>%
      distinct(region, LandAllocatorRoot, LandNode1, LandNode2, LandLeaf, policy.portfolio.standard) %>%
      left_join_error_no_match(distinct(L3231.landConstrain, region, policy.portfolio.standard),
                               by = c("region", "policy.portfolio.standard")) %>%
      # Remove any undesired land nodes
      filter(!grepl(LandNode1_remove, LandNode1)) %>%
      rename(land.constraint.policy = policy.portfolio.standard)

    L3231.landConstrain_unmngd_LN2 <- L222.LN2_UnmgdAllocation %>%
      inner_join(LandLeafs_to_keep, by = "region") %>%
      # Filter if LandLeafs_to_keep is in LandLeaf
      dplyr::rowwise() %>%
      filter(grepl(LandLeafs_to_keep, UnmanagedLandLeaf)) %>%
      ungroup %>%
      distinct(region, LandAllocatorRoot, LandNode1, LandNode2, UnmanagedLandLeaf, policy.portfolio.standard) %>%
      left_join_error_no_match(distinct(L3231.landConstrain, region,policy.portfolio.standard),
                               by = c("region", "policy.portfolio.standard")) %>%
      # Remove any undesired land nodes
      filter(!grepl(LandNode1_remove, LandNode1)) %>%
      rename(land.constraint.policy = policy.portfolio.standard)

    # Produce outputs
    L3231.landConstrain %>%
      select(-LandLeafs, -LandNode1_to_remove) %>%
      add_title("Afforestation constraints by region/year", overwrite = T) %>%
      add_units("thous km2") %>%
      add_precursors("policy/A_Land_Constraints") ->
      L3231.landConstrain

    L3231.landConstrain_mngd_LN3 %>%
      add_title("Managed LandNode3 to apply land constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_comments("Removed any nodes listed as LandNode1_to_remove in A_Land_Constraints") %>%
      add_precursors("L2231.LN3_MgdAllocation_noncrop",
                     "policy/A_Land_Constraints")  ->
      L3231.landConstrain_mngd_LN3

    L3231.landConstrain_unmngd_LN3 %>%
      add_title("Unmanaged LandNode3 to apply land constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_comments("Removed any nodes listed as LandNode1_to_remove in A_Land_Constraints") %>%
      add_precursors("L2231.LN3_UnmgdAllocation",
                     "policy/A_Land_Constraints")  ->
      L3231.landConstrain_unmngd_LN3

    L3231.landConstrain_mngd_LN2 %>%
      add_title("Managed LandNode2 to apply land constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_comments("Removed any nodes listed as LandNode1_to_remove in A_Land_Constraints") %>%
      add_precursors("L222.LN2_MgdAllocation",
                     "policy/A_Land_Constraints")  ->
      L3231.landConstrain_mngd_LN2

    L3231.landConstrain_unmngd_LN2 %>%
      add_title("Unmanaged LandNode2 to apply land constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_comments("Removed any nodes listed as LandNode1_to_remove in A_Land_Constraints") %>%
      add_precursors("L222.LN2_UnmgdAllocation",
                     "policy/A_Land_Constraints")  ->
      L3231.landConstrain_unmngd_LN2

    return_data(L3231.landConstrain_mngd_LN3, L3231.landConstrain_unmngd_LN3,
                L3231.landConstrain_mngd_LN2, L3231.landConstrain_unmngd_LN2,
                L3231.landConstrain)
  } else {
    stop("Unknown command")
  }
}
