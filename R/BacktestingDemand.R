# This is a function for creating entering demand profiles for exogenous wind, 
# solar and hydro and residual demand into the INPUT_DATA_ZONAL files for backtesting

library(dplyr)
library(openxlsx)
library(magrittr)
library(tidyr)
library(data.table)
library(purrr)

BacktestingDemand <- function(wkdir, year = 2020, states = c("NSW1", "QLD1", "SA1", "TAS1", "VIC1"),
                              HydroExo = TRUE, horizon=100, cutoff=30) {
  
  days = 365
  if (year%%4 == 0) {days = 366}

  suppressWarnings(valid_path <- try(list.dirs(wkdir),silent=T))
  
  if ("try-error" %in% class(valid_path)) {
    stop("The file path entered is not valid.")
  }
  
  suppressWarnings(List <- try(read.csv(paste0(wkdir,"/Metadata_",year,".csv"))))
  if ("try-error" %in% class(List)) {
    List <- as.data.frame(read.csv(paste0('https://www.neopoint.com.au/Service/Csv?f=107+Information%5CGenerators&from=',
                                            year,'-01-01+00%3A00&period=Yearly&instances=&section=-1&key=CEPA21F')))
    colnames(List)[1] <- "DUID"
    write.csv(List,paste0(wkdir,"/Metadata_",year,".csv"))
  }
  x <- 1:length(states)
  files <- function(state,num) {
    message(paste("Collecting data for",state,"in year",year))
    suppressWarnings(Demand <- try(read.csv(paste0(wkdir,"/Demand_",state,"_",year,".csv"))[,-1], silent = T))
    suppressWarnings(Generators <- try(read.csv(paste0(wkdir,"/Generators_",state,"_",year,".csv"))[,-1], silent = T))
    if ("try-error" %in% class(Demand)) {
      Demand <- read.csv(paste0("https://www.neopoint.com.au/Service/Csv?f=102+Demand%5CDemand+5min+by+Region&from=",
                                year,"-01-01+00%3A00&period=Yearly&instances=",state,"&section=-1&key=CEPA21F"),
                         col.names = c("DateTime","Demand","Predispatch"))
      Demand$date_time <- seq(lubridate::ymd_h(paste0(year,"-1-1 0")), by = "hour", length.out = days*24)
      Demand <- Demand %>% group_by(date_time) %>% summarise(hourly_demand = mean(Demand))
      write.csv(Demand,file=paste0(wkdir,"/Demand_",state,"_",year,".csv"))
    }
    if ("try-error" %in% class(Generators)) {
      message("Need to download data from Neopoints. This may take a few minutes.")
      df1 <- as.data.frame(data.table::fread(paste0('https://www.neopoint.com.au/Service/Csv?f=103+Generation%5CRegion+%2F+Plant+Total+Cleared+5min&from=', 
                                        year,'-01-01+00%3A00&period=Quarterly&instances=',state,'%3BGenerator&section=-1&key=CEPA21F')))[,-1]
      df2 <- as.data.frame(data.table::fread(paste0('https://www.neopoint.com.au/Service/Csv?f=103+Generation%5CRegion+%2F+Plant+Total+Cleared+5min&from=', 
                                        year,'-04-01+00%3A00&period=Quarterly&instances=',state,'%3BGenerator&section=-1&key=CEPA21F')))[,-1]
      df3 <- as.data.frame(data.table::fread(paste0('https://www.neopoint.com.au/Service/Csv?f=103+Generation%5CRegion+%2F+Plant+Total+Cleared+5min&from=', 
                                        year,'-07-01+00%3A00&period=Quarterly&instances=',state,'%3BGenerator&section=-1&key=CEPA21F')))[,-1]
      df4 <- as.data.frame(data.table::fread(paste0('https://www.neopoint.com.au/Service/Csv?f=103+Generation%5CRegion+%2F+Plant+Total+Cleared+5min&from=', 
                                        year,'-10-01+00%3A00&period=Quarterly&instances=',state,'%3BGenerator&section=-1&key=CEPA21F')))[,-1]
      Generators <- bind_rows(df1,df2,df3,df4)
      rm(df1,df2,df3,df4)
      colnames(Generators) <- sapply(strsplit(colnames(Generators),"\\."),getElement,1)
      Generators[is.na(Generators)] <- 0
      Generators$date_time <- seq(lubridate::ymd_h(paste0(year,"-1-1 0")), by = "hour", length.out = days*24)
      Generators <- Generators %>% group_by(date_time) %>% summarise(across(tidyr::everything(),mean)) %>%
        tidyr::pivot_longer(!date_time, names_to = "station", values_to = "output") %>% left_join(List, by=c("station"="DUID")) %>%
        select(date_time,station,.CO2E_ENERGY_SOURCE,output) %>% rename("Tech"=".CO2E_ENERGY_SOURCE") %>%
        ungroup() %>% group_by(date_time,Tech) %>% summarise(output = round(sum(output),3),.groups="keep") %>%
        tidyr::spread(Tech,output)
      Generators$REGION <- stringr::str_remove(state,"1")
      write.csv(Generators,file=paste0(wkdir,"/Generators_",state,"_",year,".csv"))
    }
    message("Updating INPUT DATA ZONAL workbook.")
    GenerationFile = system.file("extdata", paste0("INPUT_DATA_ZONAL_",num,".xlsx"), package = "CEPA")
    wb <- loadWorkbook(GenerationFile)
    DATA <- readWorkbook(wb,sheet="DataEntry_1",skipEmptyCols = FALSE)
    n = nrow(DATA) - (nrow(Demand)+720) - 1
    if (is.null(Generators$Wind)) Generators$Wind = 0
    if (is.null(Generators$Solar)) Generators$Solar = 0
    if (is.null(Generators$Hydro)) Generators$Hydro = 0
    DATA$Demand <- c(Demand$hourly_demand[1:720],Demand$hourly_demand,Demand$hourly_demand[(nrow(Demand)-n):nrow(Demand)])
    DATA$Wind <- c(Generators$Wind[1:720],Generators$Wind,Generators$Wind[(nrow(Generators)-n):nrow(Generators)])
    DATA$Solar <- c(Generators$Solar[1:720],Generators$Solar,Generators$Solar[(nrow(Generators)-n):nrow(Generators)])
    DATA$Hydro <- c(Generators$Hydro[1:720],Generators$Hydro,Generators$Hydro[(nrow(Generators)-n):nrow(Generators)])
    DATA$Residual.demand <- DATA$Demand - DATA$Wind - DATA$Solar - DATA$Hydro
    DATA$DEMAND <- DATA$Residual.demand
    
    DEMAND <- readWorkbook(wb,sheet="DEMAND")
    ORU <- readWorkbook(wb,sheet="OPERATING_RESERVE_UP")
    ORD <- readWorkbook(wb,sheet="OPERATING_RESERVE_DOWN")
    
    for (i in 2:ncol(DEMAND)) {
      if (i==2) {
        DEMAND[,i] = DATA$DEMAND[((i-2)*horizon):((i-1)*horizon)]
        ORU[,i] = DATA$OPERATING_RESERVE_UP[((i-2)*horizon):((i-1)*horizon)]
        ORD[,i] = DATA$OPERATING_RESERVE_DOWN[((i-2)*horizon):((i-1)*horizon)]
      }
      else {
        DEMAND[,i] = DATA$DEMAND[((i-2)*(horizon - cutoff) + 1):((i-1)*(horizon - cutoff)+cutoff)]
        ORU[,i] = DATA$OPERATING_RESERVE_UP[((i-2)*(horizon - cutoff) + 1):((i-1)*(horizon - cutoff)+cutoff)]
        ORD[,i] = DATA$OPERATING_RESERVE_DOWN[((i-2)*(horizon - cutoff) + 1):((i-1)*(horizon - cutoff)+cutoff)]
      }
    }
    
    
    writeData(wb,sheet="DataEntry_1",DATA)
    writeData(wb,sheet="DEMAND",DEMAND)
    writeData(wb,sheet="OPERATING_RESERVE_UP",ORU)
    writeData(wb,sheet="OPERATING_RESERVE_DOWN",ORD)
    saveWorkbook(wb,paste0(getwd(),"/INPUT_DATA_ZONAL_",num,".xlsx"),overwrite = TRUE)
    message("New INPUT_DATA_ZONAL file has been created and saved. Moving onto the next state.")
  }
  purrr::map2(states,x,~files(.x,.y))
  message("INPUT DATA ZONAL files updated but they must be converted to .xlsm before being used in the model.")
}




