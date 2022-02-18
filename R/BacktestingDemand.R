# This is a function for creating entering demand profiles for exogenous wind, 
# solar and hydro and residual demand into the INPUT_DATA_ZONAL files for backtesting

library(readxl)
library(dplyr)
library(openxlsx)
library(magrittr)
library(tidyr)
library(stringr)
library(data.table)
library(purrr)

BacktestingDemand <- function(year = 2020, states = c("NSW1", "QLD1", "SA1", "TAS1", "VIC1"),
                              ZonalFile1 = "INPUT_DATA_ZONAL_1.xlsm",
                              ZonalFile2 = "INPUT_DATA_ZONAL_2.xlsm",
                              ZonalFile3 = "INPUT_DATA_ZONAL_3.xlsm",
                              ZonalFile4 = "INPUT_DATA_ZONAL_4.xlsm",
                              ZonalFile5 = "INPUT_DATA_ZONAL_5.xlsm",
                              HydroExo = TRUE) {
  x <- 1:5
  files <- function(state,num) {
    Demand <- try(read.csv(paste0("Demand_",state,"_",year,".csv"))[,-1], silent = T)
    Generators <- try(read.csv(paste0("Generators_",state,"_",year,".csv"))[,-1], silent = T)
    if ("try-error" %in% class(Demand)) {
      Demand <- read.csv(paste0("https://www.neopoint.com.au/Service/Csv?f=102+Demand%5CDemand+5min+by+Region&from=",
                                year,"-01-01+00%3A00&period=Yearly&instances=",state,"&section=-1&key=CEPA21F"),
                         col.names = c("DateTime","Demand","Predispatch"))
      Demand$Hour <- rep(1:(nrow(Demand)/12),each=12)
      Demand <- Demand %>% group_by(Hour) %>% summarise(hourly_demand = mean(Demand))
      write.csv(Demand,file=paste0("Demand_",state,"_",year,".csv"))
    }
    if ("try-error" %in% class(Generators)) {
      df1 <- as.data.frame(fread(paste0('https://www.neopoint.com.au/Service/Csv?f=103+Generation%5CRegion+%2F+Plant+Total+Cleared+5min&from=', 
                                        year,'-01-01+00%3A00&period=Quarterly&instances=',state,'%3BGenerator&section=-1&key=CEPA21F')))[,-1]
      print("1/4 read in")
      df2 <- as.data.frame(fread(paste0('https://www.neopoint.com.au/Service/Csv?f=103+Generation%5CRegion+%2F+Plant+Total+Cleared+5min&from=', 
                                        year,'-04-01+00%3A00&period=Quarterly&instances=',state,'%3BGenerator&section=-1&key=CEPA21F')))[,-1]
      print("2/4 read in")
      df3 <- as.data.frame(fread(paste0('https://www.neopoint.com.au/Service/Csv?f=103+Generation%5CRegion+%2F+Plant+Total+Cleared+5min&from=', 
                                        year,'-07-01+00%3A00&period=Quarterly&instances=',state,'%3BGenerator&section=-1&key=CEPA21F')))[,-1]
      print("3/4 read in")
      df4 <- as.data.frame(fread(paste0('https://www.neopoint.com.au/Service/Csv?f=103+Generation%5CRegion+%2F+Plant+Total+Cleared+5min&from=', 
                                        year,'-10-01+00%3A00&period=Quarterly&instances=',state,'%3BGenerator&section=-1&key=CEPA21F')))[,-1]
      print("all read in")
      Generators <- rbind(df1,df2,df3,df4,fill=T)
      rm(df1,df2,df3,df4)
      colnames(Generators) <- sapply(strsplit(colnames(Generators),"\\."),getElement,1)
      Generators[is.na(Generators)] <- 0 
      Generators <- Generators[-nrow(Generators),]
      List <- as.data.frame(read.csv(paste0('https://www.neopoint.com.au/Service/Csv?f=107+Information%5CGenerators&from=',
                                            year,'-01-01+00%3A00&period=Yearly&instances=&section=-1&key=CEPA21F')))
      colnames(List)[1] <- "DUID"
      List <- List[List$.REGIONID==state,c("DUID", ".REGIONID", ".CO2E_ENERGY_SOURCE")]
      DUID = c("Date",colnames(Generators))
      Generators$Hydro = rowSums(Generators[,DUID %in% List$DUID[List$.CO2E_ENERGY_SOURCE == "Hydro"]])
      Generators$Solar = rowSums(Generators[,DUID %in% List$DUID[List$.CO2E_ENERGY_SOURCE == "Solar"]])
      Generators$Wind = rowSums(Generators[,DUID %in% List$DUID[List$.CO2E_ENERGY_SOURCE == "Wind"]])
      Generators$Hour <- rep(1:(nrow(Generators)/12),each=12)
      Generators <- Generators %>% group_by(Hour) %>% summarise(
        hydro_gen = mean(Hydro),
        solar_gen = mean(Solar),
        wind_gen = mean(Wind)
      )
      write.csv(Generators,file=paste0("Generators_",state,"_",year,".csv"))
    }
    wb <- loadWorkbook(paste0("INPUT_DATA_ZONAL_",num,".xlsm"))
    DATA <- readWorkbook(wb,sheet="DataEntry_1",skipEmptyRows = FALSE)
    print(DATA)
    n = abs(nrow(DATA)-nrow(Demand))
    DATA$Demand <- c(Demand$hourly_demand,rep(NA,n))
    DATA$Wind <- c(Generators$wind_gen,rep(NA,n))
    DATA$Solar <- c(Generators$solar_gen,rep(NA,n))
    DATA$Hydro <- c(Generators$hydro_gen,rep(NA,n))
    DATA$Residual.demand <- DATA$Demand - DATA$Wind - DATA$Solar - DATA$Hydro
    DATA$DEMAND <- DATA$Residual.demand
    
    writeData(wb,sheet="DataEntry_1",DATA)
    saveWorkbook(wb,paste0("INPUT_DATA_ZONAL_",num,".xlsm"),overwrite = TRUE,returnValue = TRUE)
  }
  map2(states,x,~files(.x,.y))
  
}


