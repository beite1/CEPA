BacktestingResults <- function(wkdir, rawdir,
                               year=2020, horizon_length=70, first_horizon=11,
                               first_time=45,last_horizon=136, last_time=54, days = 365) {
  ## Metadata
  
  `%notin%` <- Negate(`%in%`)
  
  nodes <- tibble::tibble(node = 1:9,
                  name = c("NSW", "QLD", "SA", "TAS", "VIC", "QNI", "ML", "M1", "M2"))
  time_mapping <- tibble::tibble(horizon = c(rep(first_horizon, each = horizon_length - first_time + 1),
                                     rep((first_horizon + 1):(last_horizon - 1), each = horizon_length),
                                     rep(last_horizon, each = last_time)),
                         period = c(first_time:horizon_length, rep(1:70, times = last_horizon - first_horizon - 1), 1:last_time), 
                         date_time = seq(lubridate::ymd_h(paste0(year, "-1-1 0")), by = "hour", length.out = 8760)) %>%
    mutate(horizon = paste0("r", horizon))
  
  if (year%%4 == 0) {days = 366
  time_mapping <- tibble::tibble(horizon = c(rep(first_horizon, each = horizon_length - first_time + 1),
                                     rep((first_horizon + 1):(last_horizon - 1), each = horizon_length),
                                     rep(last_horizon, each = last_time)),
                         period = c(first_time:horizon_length, rep(1:70, times = last_horizon - first_horizon - 1), 1:last_time), 
                         date_time = seq(lubridate::ymd_h(paste0(year, "-1-1 0")), by = "hour", length.out = 8784)[-c(1393:1416)]) %>%
    mutate(horizon = paste0("r", horizon))} # needs updating in model
  
  
  ##################################################################################################################################
  ##################################      Modeled data     ########################################################################
  ##################################################################################################################################
  
  
  ## Get files from the working directory set in the function.
  input_files <- list.files(wkdir) %>% tibble::as_tibble() %>%
    mutate(ext = stringr::str_extract(value,"[^.]+$"),start=stringr::str_extract(value,"^[^_]+(?=_)")) %>%
    filter(ext=="xlsx",start=="INPUT") %>% select(value) %>% arrange()
  
  meta_data <- readxl::read_excel(paste0(wkdir,'/',input_files$value[1]), sheet = "DATA", cellranger::cell_cols("A:AP")) %>%
    select(STATION, DUID, REGION, REGION_KEY, CAPACITY, FUEL_TYPE_TEXT,
           INITIAL_ENERGY_STOCK_IN_DAMS, HYDRO_DAM_CAPACITY, VARCOST) %>%
    rename("unit" = DUID)
  
  region_key <- meta_data %>%
    select(REGION, REGION_KEY) %>%
    distinct()
  
  results_file <- list.files(wkdir) %>% tibble::as_tibble() %>%
    mutate(ext = stringr::str_extract(value,"[^.]+$"),start=stringr::str_extract(value,"Outputs")) %>%
    filter(ext=="xlsx",start=="Outputs") %>% select(value) 
  
  if(length(results_file) > 1) {
    print(results_file)
    ind <- readline("What is the index of the correct results file?")
    results_file <- results_file[,ind]
  } 
  
  ## Read in data files from OutputsForReporting
  Electricity_dispatch <- readxl::read_excel(paste0(wkdir,'/',results_file$value), sheet = "Electricity_dispatch")
  SystemMarginalPrice <- readxl::read_excel(paste0(wkdir,'/',results_file$value), sheet = "SystemMarginalPrice")
  IC_Flows_Hourly <- readxl::read_excel(paste0(wkdir,'/',results_file$value), sheet = "IC_Flows_Hourly", col_names = FALSE) #incomplete?
  Charge_Units <- readxl::read_excel(paste0(wkdir,'/',results_file$value), sheet = "Charge_Units")
  Discharge_Units <- readxl::read_excel(paste0(wkdir,'/',results_file$value), sheet = "Discharge_Units")
  Storage_Level <- readxl::read_excel(paste0(wkdir,'/',results_file$value), sheet = "Storage_Level")
  
  ## Read in data file from INPUT_DATA_GENERATION
  Exogenous_generation <- purrr::map_df(input_files$value[-1], ~readxl::read_excel(paste0(wkdir,'/',.x), sheet = "DataEntry_1") %>%
                                   mutate(REGION_KEY = as.numeric(stringr::str_extract(basename(.x), "(?<=_ZONAL_)."))))
  
  ## Convert the horizon output from model to date_time 
  Electricity_dispatch <- Electricity_dispatch %>%
    inner_join(time_mapping, by = c(...1 = "horizon", ...2 = "period")) %>%
    select(-c(1:2)) %>%
    tidyr::pivot_longer(-date_time, names_to = "unit", values_to = "output") %>%
    left_join(meta_data, by = "unit")
  
  Exogenous_generation <- Exogenous_generation %>%
    filter(Time >= 721, Time <= (8760+720)) %>% #revisit, intended to extract middle 12 months
    mutate(date_time = lubridate::ymd_h(paste0(year, "-1-1 0")) + lubridate::hours(Time - 721)) %>%
    left_join(region_key, by = "REGION_KEY") %>%
    select(date_time, REGION, Demand, Wind, Solar, Hydro) %>%
    tidyr::pivot_longer(-c(date_time, REGION), names_to = "FUEL_TYPE_TEXT", values_to = "output")
  
  Charge_Units <- Charge_Units %>%
    mutate(date_time = seq(lubridate::ymd_h(paste0(year, "-1-1 0")), by = "hour", length.out = nrow(Charge_Units))) %>%
    select(-c(1:2)) %>%
    tidyr::pivot_longer(-date_time, names_to = "unit", values_to = "charge")
  
  Discharge_Units <- Discharge_Units %>%
    mutate(date_time = seq(lubridate::ymd_h(paste0(year, "-1-1 0")), by = "hour", length.out = nrow(Discharge_Units))) %>%
    select(-c(1:2)) %>%
    tidyr::pivot_longer(-date_time, names_to = "unit", values_to = "discharge")
  
  Charge_Discharge <- Charge_Units %>% 
    left_join(Discharge_Units, by = c("date_time", "unit"))  %>%
    filter(charge > 0.0001 | discharge > 0.0001) %>%
    mutate(across(c(charge, discharge), ~round(., digits = 3)),
           output = discharge - charge) %>%
    left_join(meta_data, by = "unit")
  
  rm(Charge_Units, Discharge_Units)
  
  SystemMarginalPrice <- SystemMarginalPrice %>%
    inner_join(time_mapping, by = c(...1 = "horizon", ...2 = "period")) %>%
    select(-c(1:2)) %>%
    tidyr::pivot_longer(-date_time, names_to = "region", values_to = "spot_price") %>%
    mutate(region = as.numeric(region)) %>%
    left_join(region_key, by = c("region" = "REGION_KEY")) %>%
    filter(!is.na(REGION))
  
  ############################## Price Variables ###########################################
  
  DATA <- SystemMarginalPrice %>% select(-region) %>% tidyr::spread(REGION,spot_price)
  AvPrice <- colMeans(DATA[c(2:6)])
  DailyAvPrice <- DATA %>% mutate(across("date_time",~strftime(., format = "%D", tz="GMT"))) %>%
    group_by(date_time) %>% summarise(across(everything(), mean))
  MonthAvPrice <- DATA %>% mutate(across("date_time",~strftime(., format = "%m", tz="GMT"))) %>%
    group_by(date_time) %>% summarise(across(everything(), mean))
  
  price.list <- list(DATA = DATA, AvPrice = AvPrice, DailyAvPrice = DailyAvPrice, MonthAvPrice = MonthAvPrice)
  
  ############################## Interconnector/Flow Variables #############################
  
  #Extract first two rows to create column names
  cols <- IC_Flows_Hourly[1:2,-1:-2] %>%
    t() %>%
    tibble::as_tibble(.name_repair = "universal") %>% rename(V1 = ...1, V2 = ...2) %>%
    left_join(nodes, by = c("V1" = "node")) %>%
    left_join(nodes, by = c("V2" = "node")) %>%
    mutate(link = paste0(V1, "_", V2),
           link_name = paste0(name.x, "_", name.y)) %>%
    pull(link_name)
  cols <- c("horizon", "period", cols)
  colnames(IC_Flows_Hourly) <- cols
  
  #Add time stamp and manipulate
  suppressWarnings(IC_Flows_Hourly <- IC_Flows_Hourly %>%
    mutate(across(everything(), ~replace(., is.na(.), 0))) %>%
    inner_join(time_mapping, by = c("horizon", "period")) %>%
    select(-c(1:2)) %>%
    tidyr::pivot_longer(-date_time, names_to = "link", values_to = "flow") %>%
    mutate(from_virtual = sub("_.*", "", link), #from (all nodes)
           to_virtual = sub(".*_", "", link)) %>% #to (all nodes)
    mutate(from_to = forcats::fct_recode(link, NSW_QLD = "NSW_QNI",
                                QLD_NSW = "QLD_QNI",
                                SA_VIC = "SA_ML",
                                TAS_VIC = "TAS_M1",
                                TAS_VIC = "TAS_M2",
                                VIC_SA = "VIC_ML",
                                VIC_TAS = "VIC_M1",
                                VIC_TAS = "VIC_M2",
                                QLD_NSW = "QNI_NSW",
                                NSW_QLD = "QNI_QLD",
                                VIC_SA = "ML_SA",
                                SA_VIC = "ML_VIC",
                                VIC_TAS = "M1_TAS",
                                VIC_TAS = "M2_TAS",
                                TAS_VIC = "M1_VIC",
                                TAS_VIC = "M2_VIC")) %>%
    mutate(from = sub("_.*", "", from_to), #from (only physical nodes)
           to = sub(".*_", "", from_to)) #to (only physical nodes)
  )
  #Add regional prices
  DATA <- IC_Flows_Hourly %>%
    left_join(SystemMarginalPrice, by = c("date_time", "from" = "REGION")) %>%
    left_join(SystemMarginalPrice, by = c("date_time", "to" = "REGION")) %>%
    rename(price_from = spot_price.x, price_to = spot_price.y) %>%
    select(-region.x, -region.y)
  
  #Determine annual flows between regions
  AnnualFlows <- DATA %>%
    filter(to_virtual %notin% c("QNI", "ML", "M1", "M2")) %>% #Removing duplicate flows
    group_by(from_to) %>%
    summarise(annual_flow = round(sum(flow)/1000, digits = 1)) %>% tidyr::spread(from_to,annual_flow) #in GWh
  # Daily flow rates between regions
  DailyFlows <- DATA %>%
    filter(to_virtual %notin% c("QNI", "ML", "M1", "M2")) %>% mutate(across("date_time",~strftime(., format = "%D", tz="GMT"))) %>%
    group_by(date_time,from_to) %>% summarise(
      daily_flow = round(sum(flow)/1000,3),
      .groups = "keep"
    ) %>% tidyr::spread(from_to,daily_flow)
  # Monthly flow rates between regions
  MonthlyFlows <- DATA %>%
    filter(to_virtual %notin% c("QNI", "ML", "M1", "M2")) %>% mutate(across("date_time",~strftime(., format = "%m", tz="GMT"))) %>%
    group_by(date_time,from_to) %>% summarise(
      monthly_flow = round(sum(flow)/1000,3),
      .groups = "keep"
    ) %>% tidyr::spread(from_to,monthly_flow)
  
  # Region Total flows
  FlowIn <- DATA %>% filter(to_virtual %notin% c("QNI", "ML", "M1", "M2")) %>%
    group_by(date_time, to) %>% summarise(In = sum(flow),.groups="keep") %>% rename("REGION" = "to")
  FlowOut <- DATA %>% filter(to_virtual %notin% c("QNI", "ML", "M1", "M2")) %>%
    group_by(date_time, from) %>% summarise(Out = sum(flow),.groups="keep") %>% rename("REGION" = "from")
  FlowInOut <- full_join(FlowIn,FlowOut,by = c("date_time", "REGION")) %>% mutate(Net = In - Out)
  
  rm(FlowIn,FlowOut)
  
  DailyFlowInOut <- FlowInOut %>% ungroup() %>% mutate(across("date_time",~strftime(., format = "%D", tz="GMT"))) %>%
    group_by(date_time,REGION) %>% summarise(
      daily_in = round(sum(In)/1000,3),
      daily_out = round(sum(Out)/1000,3),
      daily_net = round(sum(Net)/1000,3),
      .groups = "keep"
    )
  MonthFlowInOut <- FlowInOut %>% ungroup() %>% mutate(across("date_time",~strftime(., format = "%m", tz="GMT"))) %>%
    group_by(date_time,REGION) %>% summarise(
      month_in = round(sum(In)/1000,3),
      month_out = round(sum(Out)/1000,3),
      month_net = round(sum(Net)/1000,3),
      .groups = "keep"
    )
  AnnualFlowInOut <- FlowInOut %>% ungroup() %>%
    group_by(REGION) %>% summarise(
      annual_in = round(sum(In)/1000,3),
      annual_out = round(sum(Out)/1000,3),
      annual_net = round(sum(Net)/1000,3),
      .groups = "keep"
    )
  
  # Revenues for each link
  IC_Revenue <- DATA %>%
    filter(to_virtual %notin% c("QNI", "ML", "M1", "M2")) %>%
    mutate(price_diff = price_to - price_from,
           revenue = price_diff * flow) %>%
    group_by(link) %>%
    summarise(annual_revenue = sum(revenue, na.rm = TRUE)) %>% tidyr::spread(link,annual_revenue)
  
  ic.list <- list(DATA = DATA, AnnualFlows = AnnualFlows, DailyFlows = DailyFlows, MonthlyFlows = MonthlyFlows, Revenue = IC_Revenue,
                  FlowInOut = FlowInOut, DailyFIO = DailyFlowInOut, MonthlyFIO = MonthFlowInOut, AnnualFIO = AnnualFlowInOut)
  
  ########################### Generation x Dispatch variables ##############################################
  
  DATA <- Electricity_dispatch %>% group_by(date_time,REGION,FUEL_TYPE_TEXT) %>%
    summarise(output = sum(output),.groups = "drop") %>% bind_rows(Exogenous_generation) %>%
    arrange(date_time,REGION,FUEL_TYPE_TEXT) %>% tidyr::spread(FUEL_TYPE_TEXT,output) %>%
    full_join(FlowInOut, by = c("date_time", "REGION")) %>% select(-In,-Out) %>%
    rename("Imports" = "Net") %>% mutate(across(!date_time, ~replace(., is.na(.), 0))) %>%
    mutate(across(!c(date_time,REGION), ~ifelse(abs(.) < 0.0001, 0, .))) %>%
    mutate(across(!c(date_time,REGION), ~round(.,3)))
  
  DailyGen <- DATA %>% mutate(across("date_time",~strftime(., format = "%D", tz="GMT"))) %>%
    group_by(date_time,REGION) %>% summarise(across(everything(),~round(sum(.)/1000,3)),.groups="drop")
  
  MonthlyGen <- DATA %>% mutate(across("date_time",~strftime(., format = "%m", tz="GMT"))) %>%
    group_by(date_time,REGION) %>% summarise(across(everything(),~round(sum(.)/1000,3)),.groups="keep")
  
  AnnualGen <- DATA %>% select(-date_time) %>%
    group_by(REGION) %>% summarise(across(everything(),~round(sum(.)/1000,3)),.groups="keep")
  
  gen.list <- list(DATA = DATA, DailyGen = DailyGen, MonthlyGen = MonthlyGen, AnnualGen = AnnualGen)
  
  ###################### Weighted Average Price ########################################################
  
  weights <- DATA %>% select(-Demand) %>% mutate(weight = rowSums(across(where(is.numeric)))) %>%
    select(date_time,REGION,weight) %>% tidyr::spread(REGION,weight) %>% rename_with(~paste0(.,"_W"),.cols=!date_time)
  
  if(days==366) {weights <- weights[-(1393:1416),]}
  
  LW_AvPrice <- purrr::map2_dfc(price.list$DATA[c(2:6)],weights[,c(2:6)],~weighted.mean(.x,.y))
  Daily_AvPrice <- price.list$DATA %>% full_join(weights, by = "date_time") %>% 
    mutate(across("date_time",~strftime(., format = "%D", tz="GMT"))) %>%
    group_by(date_time) %>% summarise(
      NSW = NSW %*% NSW_W / sum(NSW_W),
      QLD = QLD %*% QLD_W / sum(QLD_W),
      SA = SA %*% SA_W / sum(SA_W),
      TAS = TAS %*% TAS_W / sum(TAS_W),
      VIC = VIC %*% VIC_W / sum(VIC_W),
      .groups="drop")
  
  Monthly_AvPrice <- price.list$DATA %>% full_join(weights, by = "date_time") %>% 
    mutate(across("date_time",~strftime(., format = "%m", tz="GMT"))) %>%
    group_by(date_time) %>% summarise(
      NSW = NSW %*% NSW_W / sum(NSW_W),
      QLD = QLD %*% QLD_W / sum(QLD_W),
      SA = SA %*% SA_W / sum(SA_W),
      TAS = TAS %*% TAS_W / sum(TAS_W),
      VIC = VIC %*% VIC_W / sum(VIC_W),
      .groups="drop")
  
  price.list <- append(price.list, list(LW_AvPrice = LW_AvPrice, Daily_LW_AvPrice = Daily_AvPrice, Monthly_LW_AvPrice = Monthly_AvPrice))
  
  ###################### List of variables/data from model #################################################
  
  MOD.Results <- list(Price = price.list, IC_Flows = ic.list, Gen_Dis = gen.list)
  
  ##################################################################################################################################
  ##################################      Real data from NeoPoints      ############################################################
  ##################################################################################################################################
  
  suppressWarnings(List <- try(read.csv(paste0(rawdir,"/Metadata_",year,".csv"))[-1]))
  if ("try-error" %in% class(List)) {
    List <- as.data.frame(read.csv(paste0('https://www.neopoint.com.au/Service/Csv?f=107+Information%5CGenerators&from=',
                                          year,'-01-01+00%3A00&period=Yearly&instances=&section=-1&key=CEPA21F')))
    colnames(List)[1] <- "DUID"
    write.csv(List,paste0(rawdir,"/Metadata_",year,".csv"))
  }

  for (state in paste0(region_key$REGION,1)){
    message(paste("Collecting data for",state,"in year",year))
    suppressWarnings(GenData <- try(read.csv(paste0(rawdir,"/Generators_",state,"_",year,".csv"))[,-1], silent = T))
    if ("try-error" %in% class(GenData)) {
      message("Need to download data from Neopoints. This may take a few minutes.")
      df1 <- as.data.frame(data.table::fread(paste0('https://www.neopoint.com.au/Service/Csv?f=103+Generation%5CRegion+%2F+Plant+Total+Cleared+5min&from=', 
                                        year,'-01-01+00%3A00&period=Quarterly&instances=',state,'%3BGenerator&section=-1&key=CEPA21F')))[,-1]
      df2 <- as.data.frame(data.table::fread(paste0('https://www.neopoint.com.au/Service/Csv?f=103+Generation%5CRegion+%2F+Plant+Total+Cleared+5min&from=', 
                                        year,'-04-01+00%3A00&period=Quarterly&instances=',state,'%3BGenerator&section=-1&key=CEPA21F')))[,-1]
      df3 <- as.data.frame(data.table::fread(paste0('https://www.neopoint.com.au/Service/Csv?f=103+Generation%5CRegion+%2F+Plant+Total+Cleared+5min&from=', 
                                        year,'-07-01+00%3A00&period=Quarterly&instances=',state,'%3BGenerator&section=-1&key=CEPA21F')))[,-1]
      df4 <- as.data.frame(data.table::fread(paste0('https://www.neopoint.com.au/Service/Csv?f=103+Generation%5CRegion+%2F+Plant+Total+Cleared+5min&from=', 
                                        year,'-10-01+00%3A00&period=Quarterly&instances=',state,'%3BGenerator&section=-1&key=CEPA21F')))[,-1]
      GenData <- bind_rows(df1,df2,df3,df4)
      colnames(GenData) <- sapply(strsplit(colnames(GenData),"\\."),getElement,1)
      GenData[is.na(GenData)] <- 0
      GenData$date_time <- rep(seq(lubridate::ymd_h(paste0(year, "-1-1 0")), by = "hour", length.out = days*24),each=12)
      GenData <- GenData %>% group_by(date_time) %>% summarise(across(everything(),mean)) %>% 
        pivot_longer(!date_time, names_to = "station", values_to = "output") %>% left_join(List,by = c("station"="DUID")) %>%
        select(date_time,station,.CO2E_ENERGY_SOURCE,output) %>% rename("Tech"=".CO2E_ENERGY_SOURCE") %>% ungroup() %>%
        group_by(date_time,Tech) %>% summarise(output = round(sum(output),3),.groups = "keep") %>% tidyr::spread(Tech,output)
      GenData$REGION <- stringr::str_remove(state,"1")
      write.csv(GenData,file=paste0(rawdir,"/Generators_",state,"_",year,".csv"))
    }
    if (state == "NSW1") Generators <- GenData
    else Generators <- Generators %>% bind_rows(GenData)
    
    Generators <- Generators %>% arrange(date_time) %>% replace(.,is.na(.),0)
  }

  suppressWarnings(Prices <- try(read.csv(paste0(rawdir,"/Price_",year,".csv"))[-1],silent=T))
  if ("try-error" %in% class(Prices)) {
    Prices <- as.data.frame(read.csv(paste0('http://neopoint.com.au/Service/Csv?f=101+Prices%5CDispatch+Prices+5min&from=',
    year,'-01-01+00%3A00&period=Yearly&instances=&section=-1&key=CEPA21F'))) %>% rename_with(~stringr::str_remove(.,"1.Price_5min"))
    Prices$date_time <- rep(seq(lubridate::ymd_h(paste0(year, "-1-1 0")), by = "hour", length.out = days*24),each=12)
    Prices <- Prices %>% group_by(date_time) %>% select(-DateTime) %>% summarise(across(everything(),mean))
    write.csv(Prices,file=paste0(rawdir,"/Price_",year,".csv"))
  }
  
  suppressWarnings(ICFlow <- try(read.csv(paste0(rawdir,"/ICFlow_",year,".csv"))[-1],silent=T))
  if ("try-error" %in% class(Prices)) {
    ICFlow <- as.data.frame(read.csv(paste0('http://neopoint.com.au/Service/Csv?f=106+Flows+and+Constraints%5CAll+Interconnector+Flow+5min&from=',
                                            year,'-01-01+00%3A00&period=Yearly&instances=&section=-1&key=CEPA21F'))) %>% select(-DateTime)
    colnames(ICFlow) = c("NSW_QLD", "NSW_QLD1", "TAS_VIC", "VIC_SA", "VIC_SA1", "VIC_NSW")                                          
    ICFlow$date_time <- rep(seq(lubridate::ymd_h(paste0(year, "-1-1 0")), by = "hour", length.out = days*24),each=12)
    ICFlow <- ICFlow %>% group_by(date_time) %>% summarise(across(everything(),mean)) %>%
      pivot_longer(!date_time,names_to = "from_to", values_to = "flow")  %>%
      mutate(from_to = stringr::str_remove(from_to,"1"),
        from = sub("_.*", "", from_to),
        to = sub(".*_", "", from_to))
    write.csv(ICFlow,file=paste0(rawdir,"/ICFlow_",year,".csv"))
  }
  
  ############################## Actual Price Variables ###########################################
  
  DATA <- Prices
  AvPrice <- colMeans(DATA[c(2:6)])
  DailyAvPrice <- DATA %>% mutate(across("date_time",~strftime(., format = "%D", tz="GMT"))) %>%
    group_by(date_time) %>% summarise(across(everything(), mean))
  MonthAvPrice <- DATA %>% mutate(across("date_time",~strftime(., format = "%m", tz="GMT"))) %>%
    group_by(date_time) %>% summarise(across(everything(), mean))
  
  price.list <- list(DATA = DATA, AvPrice = AvPrice, DailyAvPrice = DailyAvPrice, MonthAvPrice = MonthAvPrice)
  
  ############################## Actual Interconnector/Flow Variables #############################

  DATA <- ICFlow %>%
    left_join(pivot_longer(Prices,!date_time,names_to = "region",values_to = "spot_price"), by = c("date_time", "from" = "region")) %>%
    left_join(pivot_longer(Prices,!date_time,names_to = "region",values_to = "spot_price"), by = c("date_time", "to" = "region")) %>%
    rename(price_from = spot_price.x, price_to = spot_price.y)
  
  #Determine annual flows between regions (Only net)
  AnnualFlows <- DATA %>%
    group_by(from_to) %>%
    summarise(annual_flow = round(sum(flow)/1000, digits = 1)) %>% tidyr::spread(from_to,annual_flow) #in GWh
  # Daily flow rates between regions
  DailyFlows <- DATA %>%
    mutate(across("date_time",~strftime(., format = "%D", tz="GMT"))) %>%
    group_by(date_time,from_to) %>% summarise(
      daily_flow = round(sum(flow)/1000,3),
      .groups = "keep"
    ) %>% tidyr::spread(from_to,daily_flow)
  # Monthly flow rates between regions
  MonthlyFlows <- DATA %>%
    mutate(across("date_time",~strftime(., format = "%m", tz="GMT"))) %>%
    group_by(date_time,from_to) %>% summarise(
      monthly_flow = round(sum(flow)/1000,3),
      .groups = "keep"
    ) %>% tidyr::spread(from_to,monthly_flow)
  
  # Region Total flows
  FlowIn <- DATA %>%
    group_by(date_time, to) %>% summarise(In = sum(flow),.groups="keep") %>% rename("REGION" = "to")
  FlowOut <- DATA %>%
    group_by(date_time, from) %>% summarise(Out = sum(flow),.groups="keep") %>% rename("REGION" = "from")
  FlowInOut <- full_join(FlowIn,FlowOut,by = c("date_time", "REGION")) %>% replace_na(list(In = 0, Out = 0)) %>% 
    mutate(Net = In - Out) %>% select(-In,-Out)
  
  rm(FlowIn,FlowOut)
  
  DailyFlowInOut <- FlowInOut %>% ungroup() %>% mutate(across("date_time",~strftime(., format = "%D"))) %>%
    group_by(date_time,REGION) %>% summarise(
      daily_net = round(sum(Net)/1000,3),
      .groups = "keep"
    )
  MonthFlowInOut <- FlowInOut %>% ungroup() %>% mutate(across("date_time",~strftime(., format = "%m"))) %>%
    group_by(date_time,REGION) %>% summarise(
      month_net = round(sum(Net)/1000,3),
      .groups = "keep"
    )
  AnnualFlowInOut <- FlowInOut %>% ungroup() %>%
    group_by(REGION) %>% summarise(
      annual_net = round(sum(Net)/1000,3),
      .groups = "keep"
    )
  
  ic.list <- list(DATA = DATA, AnnualFlows = AnnualFlows, DailyFlows = DailyFlows, MonthlyFlows = MonthlyFlows,
                  FlowInOut = FlowInOut, DailyFIO = DailyFlowInOut, MonthlyFIO = MonthFlowInOut, AnnualFIO = AnnualFlowInOut)
  
  ########################### Generation x Dispatch variables ##############################################
  
  DATA <- Generators %>% full_join(FlowInOut, by = c("date_time", "REGION")) %>%
    rename("Imports" = "Net") %>% mutate(across(everything(), ~replace(., is.na(.), 0))) %>%
    mutate(across(!c(REGION,date_time), ~ifelse(abs(.) < 0.0001, 0, .))) %>%
    mutate(across(!c(REGION,date_time), ~round(.,3))) %>% select(-V1)
  
  DailyGen <- DATA %>% ungroup()%>% mutate(across("date_time",~strftime(., format = "%D", tz="GMT"))) %>%
    group_by(date_time,REGION) %>% summarise(across(everything(),~round(sum(.)/1000,3)),.groups="keep")
  
  MonthlyGen <- DATA %>% ungroup()%>% mutate(across("date_time",~strftime(., format = "%m", tz="GMT"))) %>%
    group_by(date_time,REGION) %>% summarise(across(everything(),~round(sum(.)/1000,3)),.groups="keep")
  
  AnnualGen <- DATA %>% ungroup() %>% select(-date_time) %>%
    group_by(REGION) %>% summarise(across(everything(),~round(sum(.)/1000,3)),.groups="keep")
  
  gen.list <- list(DATA = DATA, DailyGen = DailyGen, MonthlyGen = MonthlyGen, AnnualGen = AnnualGen)
  
  ###################### Weighted Average Price ########################################################
  
  weights <- DATA %>% mutate(weight = rowSums(across(where(is.numeric)))) %>%
    select(date_time,REGION,weight) %>% tidyr::spread(REGION,weight) %>% rename_with(~paste0(.,"_W"),.cols=!date_time)
  
  LW_AvPrice <- purrr::map2_dfc(price.list$DATA[c(2:6)],weights[,c(2:6)],~weighted.mean(.x,.y))
  Daily_AvPrice <- price.list$DATA %>% full_join(weights, by = "date_time") %>% 
    mutate(across("date_time",~strftime(., format = "%D", tz="GMT"))) %>%
    group_by(date_time) %>% summarise(
      NSW = NSW %*% NSW_W / sum(NSW_W),
      QLD = QLD %*% QLD_W / sum(QLD_W),
      SA = SA %*% SA_W / sum(SA_W),
      TAS = TAS %*% TAS_W / sum(TAS_W),
      VIC = VIC %*% VIC_W / sum(VIC_W),
      .groups="drop")
  
  Monthly_AvPrice <- price.list$DATA %>% full_join(weights, by = "date_time") %>% 
    mutate(across("date_time",~strftime(., format = "%m", tz="GMT"))) %>%
    group_by(date_time) %>% summarise(
      NSW = NSW %*% NSW_W / sum(NSW_W),
      QLD = QLD %*% QLD_W / sum(QLD_W),
      SA = SA %*% SA_W / sum(SA_W),
      TAS = TAS %*% TAS_W / sum(TAS_W),
      VIC = VIC %*% VIC_W / sum(VIC_W),
      .groups="drop")
  
  price.list <- append(price.list, list(LW_AvPrice = LW_AvPrice, Daily_LW_AvPrice = Daily_AvPrice, Monthly_LW_AvPrice = Monthly_AvPrice))
  
  ###################### List of actual variables/data #################################################
  
  Actual.Results <- list(Price = price.list, IC_Flows = ic.list, Gen_Dis = gen.list)
  
  Results <- list(Model = MOD.Results,Actual = Actual.Results)
}


