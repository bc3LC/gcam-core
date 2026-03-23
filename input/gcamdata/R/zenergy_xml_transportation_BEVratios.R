# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_transportation_BEVratios_xml
#'
#' Construct XML data structure for \code{transportation_UCD_*.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_UCD_*.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_UCD_CORE.xml} (energy XML).
module_energy_transportation_BEVratios <- function(command, ...) {
  # The below variable (trn_SPP) controls which scenario to run, as only one scenario can be run at a time.
  # This is a special case, and the way this is executed will likely change in the future.


  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.Supplysector_trn",
             "L254.FinalEnergyKeyword_trn",
             "L254.tranSubsectorLogit",
             "L254.tranSubsectorShrwtFllt",
             "L254.tranSubsectorInterp",
             "L254.tranSubsectorSpeed",
             "L254.tranSubsectorSpeed_passthru",
             "L254.tranSubsectorSpeed_noVOTT",
             "L254.tranSubsectorSpeed_nonmotor",
             "L254.tranSubsectorVOTT",
             "L254.tranSubsectorFuelPref",
             "L254.StubTranTech",
             "L254.StubTech_passthru",
             "L254.StubTech_nonmotor",
             "L254.GlobalTechShrwt_passthru",
             "L254.GlobalTechShrwt_nonmotor",
             "L254.GlobalTechCoef_passthru",
             "L254.GlobalRenewTech_nonmotor",
             "L254.GlobalTranTechInterp",
             "L254.GlobalTranTechShrwt",
             "L254.GlobalTranTechSCurve",
             "L254.StubTranTechCalInput",
             "L254.StubTranTechLoadFactor",
             "L254.StubTranTechCost",
             "L254.StubTechTrackCapital",
             "L254.StubTranTechCoef",
             "L254.StubTechCalInput_passthru",
             "L254.StubTechProd_nonmotor",
             "L254.PerCapitaBased_trn",
             "L254.PriceElasticity_trn",
             "L254.IncomeElasticity_trn",
             "L254.BaseService_trn"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    xml_files<- c("transportation_EVcost_Conservative.xml","transportation_EVcost_Mid.xml","transportation_EVcost_Advanced.xml")#,"transportation_EVcost_test.xml")
    names(xml_files) <- rep("XML", length(xml_files))
    return(xml_files)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost")
    L254.tranSubsectorVOTT <- get_data(all_data, "L254.tranSubsectorVOTT")

    modes <- c("Heavy truck","Light truck","Medium truck",
               "Large Car and Truck","Car","Mini Car","2W and 3W",
               "Bus", # all the above use ATB ratio data
               "Domestic Ship", "Domestic Aviation") # just using relative improvements compared to CORE ratios

    ratio <- L254.StubTranTechCost |> filter(tranSubsector %in% modes,stub.technology %in% c("BEV","Liquids")) |>
      pivot_wider(names_from = stub.technology,values_from = input.cost) |> mutate(value=BEV/Liquids)

    map <- read.csv("inst/extdata/energy/mappings/ATB_UCD_mapping.csv")
    atb <- read.csv("inst/extdata/energy/ATB_vehicles.csv")
    atb_ratio <- atb |> filter(metric=="Modeled Vehicle Price (2022$)")

    # for cases where "Conservative" does not exist, use "Constant" scenario
    atb_ratio <- atb_ratio |> mutate(scenario = case_when(
      scenario == "Constant" ~ "Conservative",
      .default = scenario
    ))
    atb_rat <- NULL
    #calculate ratios of vehicle cost data for Advanced, Mid and Conservative scenario
    for (i in seq(1,dim(map)[1])){
      tmp <- atb_ratio |> filter(scenario=="Advanced",vehicle_powertrain==map[i,4],vehicle_detail==map[i,5],fuel_category==map[i,6],vehicle_class==map[i,8])|>
        select(year,value) |> left_join(atb_ratio |> filter(scenario=="Advanced",vehicle_powertrain==map[i,9],vehicle_detail==map[i,10],fuel_category==map[i,11],vehicle_class==map[i,8])|>
                                          select(year,value)|>rename(denominator=value),by = join_by(year)) |> mutate(value=value/denominator)
      atb_rat <- atb_rat |> rbind(tmp |> select(-denominator) |> mutate(scenario="Advanced",tranSubsector=map[i,1],region=map[i,2]))
    }

    for (i in seq(1,dim(map)[1])){
      tmp <- atb_ratio |> filter(scenario=="Mid",vehicle_powertrain==map[i,4],vehicle_detail==map[i,5],fuel_category==map[i,6],vehicle_class==map[i,8])|>
        select(year,value) |> left_join(atb_ratio |> filter(scenario=="Mid",vehicle_powertrain==map[i,9],vehicle_detail==map[i,10],fuel_category==map[i,11],vehicle_class==map[i,8])|>
                                          select(year,value)|>rename(denominator=value),by = join_by(year)) |> mutate(value=value/denominator)
      atb_rat <- atb_rat |> rbind(tmp |> select(-denominator) |> mutate(scenario="Mid",tranSubsector=map[i,1],region=map[i,2]))
    }

    for (i in seq(1,dim(map)[1])){
      tmp <- atb_ratio |> filter(scenario=="Conservative",vehicle_powertrain==map[i,4],vehicle_detail==map[i,5],fuel_category==map[i,6],vehicle_class==map[i,8])|>
        select(year,value) |> left_join(atb_ratio |> filter(scenario=="Conservative",vehicle_powertrain==map[i,9],vehicle_detail==map[i,10],fuel_category==map[i,11],vehicle_class==map[i,8])|>
                                          select(year,value)|>rename(denominator=value),by = join_by(year)) |> mutate(value=value/denominator)
      atb_rat <- atb_rat |> rbind(tmp |> select(-denominator) |> mutate(scenario="Conservative",tranSubsector=map[i,1],region=map[i,2]))
    }

 #extrapolate ratio values to close gap to 0.65 frontier partially by 2100
    atb_rat <- atb_rat |> rbind(atb_rat |> filter(year==2050) |> mutate(year=2100,value=(value-0.65)/2+0.65))
    library(zoo)
    atb_rat <- atb_rat |>
    group_by(scenario,tranSubsector,region) |>
      complete(year = seq(2050,2100,5)) |>
      mutate(value = na.approx(value)) |>
      ungroup()

    #add assumption for domestic shipping and aviation:
    atb_rat <- atb_rat |> rbind(
      atb_rat |> filter(tranSubsector=="Car", year >2034) |>
        mutate(tranSubsector ="Domestic Ship", value = case_when(
          scenario=="Conservative" ~ 3.8 - 1.5*(year-2035)/70, #reaching 2.3 in 2105
          scenario=="Mid" ~ 3.5 - 2*(year-2035)/70, #reaching 1.5 in 2105
          scenario=="Advanced" ~ 3.1 - 2.3*(year-2035)/70 #reaching 0.8 in 2105
        )))|> #and for domestic Aviation sector
      rbind(
          atb_rat |> filter(tranSubsector=="Car", year >2034) |>
            mutate(tranSubsector ="Domestic Aviation", value = case_when(
              scenario=="Conservative" ~ 1.3 - 0.35*(year-2035)/70, #reaching 0.95 in 2105
              scenario=="Mid" ~ 1.2 - 0.4*(year-2035)/70, #reaching 0.8 in 2105
              scenario=="Advanced" ~ 1.1 - 0.45*(year-2035)/70 #reaching 0.65 shortly after 2105
            ))
    )

    #double check that scenario ordering is always correct (correcting the few cases with ATB inconsistencies)
    atb_rat <- atb_rat|>pivot_wider(names_from = scenario) |> dplyr::rowwise() |> mutate(lowest=min(Advanced,Conservative,Mid),
                        medi=median(c(Advanced,Conservative,Mid)),highest=max(Advanced,Conservative,Mid)) |>
                        select(-Advanced,-Conservative,-Mid) |> rename(Advanced=lowest,Mid=medi,Conservative=highest)|>
                        pivot_longer(cols=c(Advanced,Conservative,Mid),names_to = 'scenario')
    #double check that regional ordering is always correct (correcting the few cases where different ATB categories cross over in ratio over time)
    atb_rat <- atb_rat|>pivot_wider(names_from = region) |> dplyr::rowwise() |> mutate(lowest=min(Low,Medium,High),
                                                                                     medi=median(c(Low,Medium,High)),highest=max(Low,Medium,High)) |>
      select(-Low,-High,-Medium) |> rename(Low=lowest,Medium=medi,High=highest)|>
      pivot_longer(cols=c(Low,Medium,High),names_to = 'region')

#make mapping of GCAM regions to 3 groups of cost assumptions
    map_reg <- data.frame(gcam = c(c("USA"),
                                    c("Canada","Russia","EU-12","EU-15","European Free Trade Association")
                                    ),
                          region = c(c("High"),
                                   rep("Medium",5)))

    map_reg <- map_reg |> rbind(data.frame(gcam = setdiff(unique(ratio$region),unique(map_reg$gcam)),
                                           region = rep("Low",26)))
#  optional diagnostic plots
    # library(ggplot2)
    # for(i in c("2W and 3W","Mini Car","Car","Large Car and Truck","Light truck","Medium truck","Heavy truck","Bus","Domestic Ship","Domestic Aviation")){
    # ggplot()+
    #     # geom_line(data=atb_rat |> filter(region %in% c("High","Medium","Low"),scenario %in% c("Mid","Advanced","Conservative"),tranSubsector==i),aes(x=year,y=value,linetype=region),color="white")+
    #   geom_line(data=ratio |> filter(region %in% c("USA","EU-15","China"),sce %in% c("SSP1","CORE"),tranSubsector==i),aes(x=year,y=value,linetype=region,color=sce))+
    #     geom_line(data=atb_rat |> filter(region %in% c("High","Medium","Low"),scenario %in% c("Mid","Advanced","Conservative"),tranSubsector==i),aes(x=year,y=value,linetype=region,color=scenario))+
    #   # scale_y_continuous(limits=c(0.5,3))+
    #   geom_hline(yintercept=1)+geom_hline(yintercept=0)+geom_vline(xintercept=2050)+scale_linetype_manual(values=c("dotted","dashed","solid","dotted","dashed","solid"))+ggtitle(i)+theme_bw()
    #
    # ggsave(filename=paste0("../ratio",i,"_0312.png"), width=7.34,height = 5.69)
    #   }

    atb_rat <- left_join(map_reg,atb_rat,by = join_by(region),relationship = "many-to-many") #
    atb_rat <- atb_rat |> rename(group=region,region=gcam)

    #adjust atb to have all scenarios existing in L254, but renamed scen names, and
    # rename atb variable scenario to sce to match with L254.Stub:
    #Advanced , Mid , Conservative , 'test' = all set to 100 (so basically keeping values identical to before)
    atb_rat <- atb_rat |> filter(scenario %in% c("Mid","Advanced")) |>rbind(
      atb_rat |> filter(scenario =="Advanced") |> mutate(scenario="test",value =100)) |> rbind(
        atb_rat |> filter(scenario =="Conservative"))  |> rename(sce=scenario,ratio=value)

    #prepare ratio to be used for calculation of new values
    #get rid of value (ratio between BEV and Liquids price plotted before)
    ratio <- ratio |> select(-value)
    #use only "CORE" scenario values, but have the 4 times, with CORE, SSP1, SSP3 and SSP5
    ratio <- ratio |> filter(sce=="CORE") |> mutate(sce="test")|>
      rbind(ratio |> filter(sce=="CORE") |> mutate(sce="Conservative"))|>
      rbind(ratio |> filter(sce=="CORE") |> mutate(sce="Mid"))|>
      rbind(ratio |> filter(sce=="CORE") |> mutate(sce="Advanced"))

    # make atb ratio be minimum of atb_rat and the ratio before
    atb_rat <- left_join(ratio |> filter(year>2025),atb_rat |> filter(year>2025),
                         by = join_by(region, tranSubsector, year, sce)) |>
      dplyr::rowwise()|> #
      mutate(BEV=min(BEV,ratio*Liquids)) |>
      # mutate(BEV=ratio*Liquids) |>
      ungroup()



    # ggplot()+
    #   geom_line(data=ratio,aes(x=year,y=BEV,linetype=tranSubsector,color=sce),alpha=0.5)+
    #   geom_line(data=atb_rat,aes(x=year,y=BEV,linetype=tranSubsector,color=sce))+
    #   facet_wrap(~region,nrow=5) #+ scale_y_continuous(limits=c(0.5,2))
    #
    # ggplot()+
    #   geom_line(data=ratio|>filter(region=="USA"),aes(x=year,y=BEV,linetype=tranSubsector,color=sce),alpha=0.2)+
    #   geom_line(data=atb_rat|>filter(region=="USA"),aes(x=year,y=BEV,linetype=tranSubsector,color=sce))+
    #   facet_wrap(~region,nrow=5)# + scale_y_continuous(limits=c(0.5,2))

    #prepare data to add for add-on files
    L254.StubTranTechCost <- L254.StubTranTechCost |> filter(stub.technology=="Liquids") |>rbind(
      atb_rat |> select(-Liquids,-group,-ratio) |> mutate(stub.technology="BEV") |> rename(input.cost=BEV)
    ) |> filter(stub.technology=="BEV")

    # ratio <- L254.StubTranTechCost |> filter(tranSubsector %in% modes,stub.technology %in% c("BEV","Liquids")) |>
    #   pivot_wider(names_from = stub.technology,values_from = input.cost) |> mutate(value=BEV/Liquids)

    ret_data <- c()
    curr_env <- environment()

#Write out files
      for (i in c("Conservative","Mid","Advanced")){ #,"test"
      xml_name <- paste0("transportation_EVcost_", i, ".xml")
      #Read SSP specific data
      L254.StubTranTechCost_SSP <- L254.StubTranTechCost %>%  filter(sce== i)%>% filter(year>MODEL_FIRST_FUTURE_YEAR)
      # L254.tranSubsectorVOTT <- L254.tranSubsectorVOTT |> filter(sce==1)


      #Create xmls
      create_xml(xml_name) %>%
        add_xml_data(L254.StubTranTechCost_SSP, "StubTranTechCost") %>%
        add_precursors("L254.StubTranTechCost")  %>%
        assign(xml_name, ., envir = curr_env)


      ret_data <- c(ret_data, xml_name)
      }



    # write_csv(atb_rat, "atb_rat.csv")


    #Return all xmls
    ret_data %>%
      paste(collapse = ", ") %>%
      paste0("return_data(", ., ")") %>%
      parse(text = .) %>%
      eval()


  } else {
    stop("Unknown command")
  }
}
