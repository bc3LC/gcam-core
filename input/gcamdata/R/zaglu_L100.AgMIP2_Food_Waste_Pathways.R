# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L100.AgMIP2_Food_Waste_Pathways
#'
#' Process AgMIP intake data to generate food waste shares and scenarios
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L100.AgMIP_FoodWaste_Share_Pathway_SSP}
#' @details Process AgMIP diet data to generat dietary change scenarios.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else inner_join left_join mutate rename select
#' @importFrom tidyr  complete drop_na gather nesting spread replace_na
#' @importFrom tibble tibble
#' @author XZ 2022
module_aglu_L100.AgMIP2_Food_Waste_Pathways <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      #FILE = "common/iso_GCAM_regID",
      FILE = "aglu/AgMIP/GCAM_AgMIP_food_group_mapping",
      "FAO_Food_Macronutrient_All_2010_2019",
      "L101.Pop_thous_Scen_R_Yfut",
      "L201.Pop_gSSP2",
      "L102.gdp_mil90usd_Scen_R_Y",
      "L100.FAO_SUA_APE_balance",
      "L100.AgMIP_EL2_intake_targets_foodgroup_r")

  MODULE_OUTPUTS <-
    c("L100.AgMIP_FoodWaste_Share_Pathway_SSP")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    L100.AgMIP_EL2_intake_targets_foodgroup_r ->
      AgMIP_foodgrouptargets_GCAM

    # 2. Generate food waste in base year for GCAM ----
    # GCAM has only food supply (waste + intake)
    # We will use intake from AgMIP_foodgrouptargets_GCAM to back out waste shares
    # However, this is highly uncertain. So we will using more aggregated groups (FruitsVeg, animal, other) and
    # assume waste shares are the same within the group

    # using the gcamdata data (2020)
    ## Derive waste shares based on GCAM-FAO 2020 vs. Marco S. data ----
    FAO_Food_Macronutrient_All_2010_2019 %>%
      #filter(year %in% aglu.MODEL_MACRONUTRIENT_YEARS) %>%
      filter(year == 2020) %>%
      # Aggregate to region and GCAM commodity
      dplyr::group_by_at(vars(GCAM_region_ID, GCAM_commodity, year, macronutrient)) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      # Mean over aglu.MODEL_MACRONUTRIENT_YEARS
      dplyr::group_by_at(vars(GCAM_region_ID, GCAM_commodity, macronutrient)) %>%
      summarise(value = mean(value), .groups = "drop") %>%
      spread(macronutrient, value) ->
      DF_Macronutrient_FoodItem1


    DF_Macronutrient_FoodItem1 %>%
      transmute(GCAM_region_ID, GCAM_commodity, value = MKcal/1000000) ->
      GCAM_FAO_Diet2020

    GCAM_AgMIP_food_group_mapping %>%
      distinct(GCAM_food_agg) %>% pull %>% c(.,"NEC") -> COMM_GCAM_food_agg

    GCAM_FAO_Diet2020 %>%
      left_join_error_no_match(
        # 2020 population is the same across SSPs
        L101.Pop_thous_Scen_R_Yfut %>%
          filter(year == 2020, scenario == "gSSP2") %>%
          rename(totalPop = value) %>% select(-scenario),
        by = "GCAM_region_ID") %>%
      mutate(value = value / totalPop * 1000000000 /365) %>%
      left_join(GCAM_AgMIP_food_group_mapping %>%
                  distinct(GCAM_commodity = GCAM_food_commodities, GCAM_food_agg),
                by = "GCAM_commodity") %>%
      # keep NEC
      mutate(GCAM_food_agg = if_else(is.na(GCAM_food_agg), GCAM_commodity, GCAM_food_agg)) %>%
      dplyr::group_by_at(vars(-GCAM_commodity, -value)) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      mutate(GCAM_food_agg = factor(GCAM_food_agg,
                                    levels = rev(COMM_GCAM_food_agg))) %>%
      select(-totalPop) %>% mutate(measure = "FAO2020_Supply") %>%
      select(-year)->
      GCAM_FAO_Diet2020_1

    GCAM_FAO_Diet2020_1 %>%
      rename(sector = GCAM_food_agg) %>%
      bind_rows(
        AgMIP_foodgrouptargets_GCAM %>% select(-diet_scenario, -unit) %>%
          rename(sector = AgMIP_food_group_agg) ) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      GCAM_AgMIP_Supply_Intake2020


    ## Compare GCAM food supply vs. intake ----
    # library(ggplot2)
    # GCAM_AgMIP_Supply_Intake2020 %>% #filter(sector == "Grains") %>%
    #   #filter(GCAM_region_ID %in% c(1, 2, 7, 11, 12, 18)) %>%
    #   ggplot() + facet_wrap(~region) +
    #   geom_bar(aes(x = measure, y = value, fill = sector),
    #            stat = "identity", color = "black", size = 0.4) +
    #   labs(y = "kcal/ca/d", x = "Scenario")  +
    #   theme_bw() +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p;p
    #
    # ggsave(plot = p, "figures/CalorieSupplyToEL2Intake.png", width = 20, height = 20 )


    ## ** GCAM_AgMIP_Supply_Intake2020----

    ## How large is the waste in 2019 by food group? ----


    L102.gdp_mil90usd_Scen_R_Y %>%
      filter(scenario == "gSSP2") %>%
      mutate(value = value / gdp_deflator(1990, 2020)) %>% filter(year >= 2015) %>%
      left_join_error_no_match(
        L101.Pop_thous_Scen_R_Yfut %>%
          filter( scenario == "gSSP2") %>%
          rename(totalPop = value) %>% select(-scenario) %>%
          bind_rows(
            L201.Pop_gSSP2 %>% filter(year == 2015) %>%
              left_join_error_no_match(GCAM_region_names, by = "region") %>%
              select(GCAM_region_ID, year, totalPop) ),
        by = c("GCAM_region_ID", "year")
      ) %>%
      mutate(pcGDP = value /totalPop * 1000) %>%
      filter(year == 2020) ->
      pcGDP_2020_2020USD

    # quick global check
    GCAM_AgMIP_Supply_Intake2020 %>% #filter(GCAM_region_ID == 11) %>%
      dplyr::group_by_at(vars(-region, -GCAM_region_ID, -sector, -value)) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      spread(measure, value) %>%
      mutate(
        Waste = FAO2020_Supply - intake2020,
        TotalWasteShare = 1-intake2020/FAO2020_Supply)

    # Globally 28.7% food waste!.... sounds high
    # 29.5 when updated to 2020
    # 27.2 when adding back fat_ani (previously removed); This value is likely more reasonable to start with
    # E.g., the intake in Gatto & Chepeliev 2014 was 2480 in China; this is 2278 here
    # Our supply matches FAOSTAT (~3284)! so 30.6% waste share for China

    GCAM_AgMIP_Supply_Intake2020 %>%
      dplyr::group_by_at(vars(-sector, -value)) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      spread(measure, value) %>%
      mutate(
        Waste = FAO2020_Supply - intake2020,
        TotalWasteShare = 1-intake2020/FAO2020_Supply) %>%
      left_join_error_no_match(
        pcGDP_2020_2020USD %>%
          select(GCAM_region_ID, pcGDP, pop = totalPop, GDP = value), by = "GCAM_region_ID"
      ) -> Waste_pcGDP

    ## First check total waste share across GCAM regions ----
    # Waste_pcGDP %>%
    #   ggplot() +
    #   geom_hline(yintercept = 0.3, color = "blue") +
    #   geom_bar(aes(x = reorder(region, -TotalWasteShare ),
    #                y = TotalWasteShare, fill = TotalWasteShare), size = 0.5, color = "black",
    #            stat = "identity") +
    #   viridis::scale_fill_viridis()+
    #   labs(y = "Share", Title = "Food calorie waste share") +
    #   theme_bw() +
    #   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) -> p;p

    ## Check sectoral level intake vs. supply ----

    GCAM_AgMIP_Supply_Intake2020 %>%
      # We kept OtherMeat_Fish in GCAM mapping previously
      mutate(sector = as.character(sector)) %>% #distinct(sector)
      mutate(sector = if_else(sector %in% c("OtherMeat_Fish"), "Fish", sector)) %>%
      mutate(sector = if_else(sector %in% c("NEC", "Others"), "OtherNEC", sector)) %>%
      #mutate(sector = if_else(sector %in% c("NEC", "Others", "Fish", "OtherMeat_Fish"), "OtherNEC", sector)) %>%
      dplyr::group_by_at(vars(-value)) %>% summarize(value = sum(value),.groups = "drop") %>%
      filter(GCAM_region_ID != 30) %>%
      spread(measure, value) %>%
      mutate(WasteShare = 1- intake2020/FAO2020_Supply) ->
      SectoralWasteShare

    # Fruit & Veg are fine
    # SectoralWasteShare %>%
    #   filter(sector %in% c("FruitsVeg") ) %>%
    #   ggplot() + facet_wrap(~sector) +
    #   geom_bar(aes(x = reorder(region, -WasteShare ),
    #                y = WasteShare, fill = WasteShare), size = 0.5, color = "black",
    #            stat = "identity") +
    #   viridis::scale_fill_viridis()+
    #   labs(y = "Share", x= "Region", Title = "Food calorie waste share: Fruits & Vegetable") +
    #   theme_bw() +
    #   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) -> p;p

    # Derive waste share for Dairy, Rum and NonRum together

    # SectoralWasteShare%>%
    #   filter(sector %in% c("Ruminant", "NonRuminant", "Dairy", "Fish") ) %>%
    #   group_by(region, GCAM_region_ID) %>%
    #   mutate(FAO2020_Supply = sum(FAO2020_Supply), intake2020 = sum(intake2020),
    #          WasteShare = 1- intake2020/FAO2020_Supply) %>%
    #   ggplot() + facet_wrap(~sector) +
    #   geom_bar(aes(x = reorder(region, -WasteShare ),
    #                y = WasteShare, fill = WasteShare), size = 0.5, color = "black",
    #            stat = "identity") +
    #   viridis::scale_fill_viridis()+
    #   labs(y = "Share", x= "Region", Title = "Food calorie waste share: PlantProtein") +
    #   theme_bw() +
    #   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) -> p;p

    # Derive waste share for Dairy, Rum and NonRum together

    # SectoralWasteShare%>%
    #   filter(!sector %in% c("Ruminant", "NonRuminant", "Dairy", "FruitsVeg") ) %>%
    #   group_by(region, GCAM_region_ID) %>%
    #   mutate(FAO2020_Supply = sum(FAO2020_Supply), intake2020 = sum(intake2020),
    #          WasteShare = 1- intake2020/FAO2020_Supply) %>%
    #   ggplot() + facet_wrap(~sector) +
    #   geom_bar(aes(x = reorder(region, -WasteShare ),
    #                y = WasteShare, fill = WasteShare), size = 0.5, color = "black",
    #            stat = "identity") +
    #   viridis::scale_fill_viridis()+
    #   labs(y = "Share", x= "Region", Title = "Food calorie waste share") +
    #   theme_bw() +
    #   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) -> p;p


    SectoralWasteShare %>% #
      mutate(WasteSector = if_else(sector%in% c("FruitsVeg"), "FruitsVeg", sector),
             WasteSector = if_else(sector%in% c("Ruminant", "NonRuminant", "Dairy", "Fish"), "Animal", WasteSector),
             WasteSector = if_else(!WasteSector%in% c("Animal", "FruitsVeg",
                                                      "OtherNEC"),
                                   "Others", WasteSector)) %>%
      filter(WasteSector!= "OtherNEC") %>%
      group_by(region, GCAM_region_ID, WasteSector) %>%
      summarize(FAO2020_Supply = sum(FAO2020_Supply),
                intake2020 = sum(intake2020), .groups = "drop") %>%
      mutate( WasteShare = 1- intake2020/FAO2020_Supply) ->
      SectoralWasteShare_agg


    SectoralWasteShare_agg %>%
      select(region, GCAM_region_ID, WasteSector, WasteShare) %>%
      spread(WasteSector, WasteShare) %>%
      # Ensure animal > other crops
      mutate(Animal = pmax(Animal, Others)) %>%
      gather(WasteSector, WasteShare, -region, -GCAM_region_ID) %>%
      # Adding 4% minimium
      mutate(WasteShare = pmax(0.04, WasteShare)) ->
      SectoralWasteShare_agg_updated

    #Adding Taiwan per China
    SectoralWasteShare_agg_updated %>%
      bind_rows(
        SectoralWasteShare_agg_updated %>%
          filter(GCAM_region_ID == 11) %>%
          mutate(GCAM_region_ID = 30, region = "Taiwan")
      ) ->
      SectoralWasteShare_agg_updated


    # SectoralWasteShare_agg_updated %>%
    #   ggplot() + facet_wrap(~WasteSector) +
    #   geom_bar(aes(x = reorder(region, -WasteShare ),
    #                y = WasteShare, fill = WasteShare), size = 0.5, color = "black",
    #            stat = "identity") +
    #   viridis::scale_fill_viridis()+
    #   labs(y = "Share", x= "Region", Title = "Food calorie waste share") +
    #   theme_bw() +
    #   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) -> p;p
    # ggsave("figures/FoodWaste_Share.png", p, width = 12, height = 8)


    ## Map back to GCAM sectors ----
    GCAM_AgMIP_food_group_mapping %>%
      select(GCAM_commodity = GCAM_food_commodities, WasteSector) %>% distinct() %>%
      left_join(
        SectoralWasteShare_agg_updated, by = "WasteSector"
      ) %>% select(-WasteSector, -region) ->
      GCAM_FoodWaste_Share

    # We were trying to generate future waste share scenarios
    # E.g., vs. GDP pc
    # But no strong/significant relationship at sectoral levels

    #  Done ***Waste share data ----


    # 3. Generate food waste pathways for GCAM (how waste share change by 2100?) ----

    SectoralWasteShare_agg_updated %>%
      left_join_error_no_match(
        pcGDP_2020_2020USD %>%
          select(GCAM_region_ID, pcGDP, pop = totalPop, GDP = value), by = "GCAM_region_ID"
      ) -> Waste_pcGDP_Sector_2020

    # ## Check total waste share vs. pc GDP across GCAM regions ----
    # Waste_pcGDP_Sector_2020 %>% filter(region != "Taiwan") %>%
    #   ggplot(aes(x = log(pcGDP), y = WasteShare)) +
    #   facet_wrap(~WasteSector) +
    #   geom_point(aes(size = pop, fill = region), shape = 21) +
    #   #stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)
    #   geom_smooth(aes(weight = pop), method = "lm") +
    #   labs(title = "Relationship betweel food calorie waste share and log GDP in 2020 across GCAM regions",
    #        x = "Log (Per capita GDP)", y = "Waste Share", fill = "GCAM region", size = "Population")+
    #   theme_bw() -> p;p
    #
    # Waste_pcGDP %>%
    #   ggplot(aes(x = log(pcGDP), y = TotalWasteShare)) +
    #   geom_point(aes(size = pop, fill = region), shape = 21) +
    #   geom_smooth(aes(weight = pop), method = "lm") +
    #   labs(title = "Relationship betweel food calorie waste share and log GDP in 2020 across GCAM regions",
    #        x = "Log (Per capita GDP)", y = "Waste Share", fill = "GCAM region", size = "Population")+
    #   theme_bw() -> p;p
    #
    # lm_eqn <- function(){
    #   m <- lm(TotalWasteShare ~ log(pcGDP), weights = pop,
    #           Waste_pcGDP);
    #   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
    #                    list(a = format(unname(coef(m)[1]), digits = 2),
    #                         b = format(unname(coef(m)[2]), digits = 2),
    #                         r2 = format(summary(m)$r.squared, digits = 3)))
    #   as.character(as.expression(eq));
    # }
    #
    # p1 <- p + geom_text(x = 8, y = 0.1, label = lm_eqn(), parse = TRUE);p1
    #
    # ggsave(filename = "figures/FoodWasteShare_pcGDP.png", plot = p1, width = 12, height = 8)

    # The coefficient here is 0.038


    # Generate future waste scenario per income growth across SSPs

    L101.Pop_thous_Scen_R_Yfut %>%
      filter(year == 2020) %>% mutate(year = 2015) %>%
      left_join(
        L201.Pop_gSSP2 %>% filter(year == 2015) %>%
          left_join_error_no_match(GCAM_region_names, by = "region"),
        by = c("GCAM_region_ID", "year") ) %>%
      mutate(value = totalPop) %>%
      select(names(L101.Pop_thous_Scen_R_Yfut)) %>%
      bind_rows(L101.Pop_thous_Scen_R_Yfut) ->
      L101.Pop_thous_Scen_R_Yfut_w2015

    # Note that 2015 is in the data as well
    # so we will compute water share in 2015 based on 2020
    L102.gdp_mil90usd_Scen_R_Y %>%
      mutate(value = value / gdp_deflator(1990, 2020)) %>% filter(year >= 2015) %>%
      left_join_error_no_match(
        L101.Pop_thous_Scen_R_Yfut_w2015 %>%
          rename(totalPop = value),
        by = c("GCAM_region_ID", "scenario", "year")
      ) %>%
      mutate(pcGDP = value /totalPop * 1000) ->
      pcGDP_2020USD_SSPs

    pcGDP_2020USD_SSPs %>%
      filter(year %in% seq(2015, 2100, 5)) %>%
      select(scenario, GCAM_region_ID, year, pcGDP) %>%
      left_join(
        Waste_pcGDP_Sector_2020 %>% select(region, GCAM_region_ID,WasteSector, WasteShare) %>%
          group_by(WasteSector) %>%
          mutate(MaxRegWasteShare = max(WasteShare)) %>% ungroup(),
        by = "GCAM_region_ID"
      ) %>% #filter(GCAM_region_ID == 11, WasteSector == "Others", scenario == "gSSP2") %>%
      group_by(scenario, GCAM_region_ID, WasteSector) %>%
      mutate(logpcGDP = log(pcGDP), logpcGDP_lag = lag(log(pcGDP)),
             # 0.038 came from the regression above
             ShareAdder = 0.038*(logpcGDP - logpcGDP_lag)) %>%
      replace_na(list(ShareAdder = 0)) %>%
      mutate(ShareAdder = cumsum(ShareAdder),
             # note that this would be starting 2020; so need to rebase to 2020
             # add the WasteShare is 2020
             ShareAdder_base2020 =  ShareAdder - ShareAdder[year ==2020]) %>%
      mutate(WasteShareDynamic = WasteShare + ShareAdder_base2020)  %>%
      ungroup() %>%
      # set a ceiling at initial max region waste share per sector group
      mutate(WasteShareDynamic = pmin(WasteShareDynamic, MaxRegWasteShare)) %>%
      select(scenario, GCAM_region_ID, year, WasteSector, WasteShare = WasteShareDynamic) ->
      SectoralWasteShare_agg_updated_future_SSP

    ## Map back to GCAM sectors ----
    GCAM_AgMIP_food_group_mapping %>%
      select(GCAM_commodity = GCAM_food_commodities, WasteSector) %>% distinct() %>%
      full_join(
        # 2015 is calculated!! so can be different from 2020
        SectoralWasteShare_agg_updated_future_SSP,
        by = "WasteSector"
      ) %>% select(-WasteSector) ->
      GCAM_FoodWaste_Share_Pathway_SSP0

    GCAM_FoodWaste_Share_Pathway_SSP0 %>%
      # create template for waste scenarios
      mutate(HalfWaste2050 = WasteShare,
             HalfWaste2100 = WasteShare,
             StaticWaste = WasteShare) %>%
      group_by(scenario, GCAM_region_ID, GCAM_commodity) %>%
      #filter(GCAM_commodity == "Beef", GCAM_region_ID == 1, scenario == "gSSP1") %>%

      # Note that 2025 should be the same across scenarios!
      # Half Waste 2050
      mutate(HalfWaste2050 = if_else(year == 2050, 0.5 * HalfWaste2050, HalfWaste2050),
             HalfWaste2050 = if_else(year >= 2050, HalfWaste2050[year == 2050], HalfWaste2050),
             HalfWaste2050 = if_else(year %in% 2030:2045, NA_real_, HalfWaste2050) ) %>%
      # linear decrease by 2050 from 2020
      mutate(HalfWaste2050 = approx_fun(year, HalfWaste2050)) %>%

      #Half Waste 2100
      mutate(HalfWaste2100 = if_else(year == 2100, 0.5 * HalfWaste2100, HalfWaste2100),
             HalfWaste2100 = if_else(year >= 2100, HalfWaste2100[year == 2100], HalfWaste2100),
             HalfWaste2100 = if_else(year %in% 2030:2095, NA_real_, HalfWaste2100) ) %>%
      # linear decrease by 2100 from 2020
      mutate(HalfWaste2100 = approx_fun(year, HalfWaste2100)) %>%

      # Static Waste
      mutate(StaticWaste  = if_else(year == 2100, StaticWaste[year == 2025], StaticWaste),
             StaticWaste = if_else(year %in% 2030:2095, NA_real_, StaticWaste) ) %>%
      mutate(StaticWaste = approx_fun(year, StaticWaste)) %>%

      ungroup() ->
      L100.AgMIP_FoodWaste_Share_Pathway_SSP



    #***Done Waste Pathways----


    # Produce outputs ----
    #********************************* ----

    L100.AgMIP_FoodWaste_Share_Pathway_SSP %>%
      add_title("Food waste share in base data and future pathways across SSPs") %>%
      add_units("NA") %>%
      add_comments("Generated based on AgMIP intake data and FAO supply data, and cross-sectional relationship between per capital income and waste share") %>%
      add_precursors(MODULE_INPUTS) ->
      L100.AgMIP_FoodWaste_Share_Pathway_SSP


    # Done & return data----
    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
