#This is a function for creating a set of generation files from an assumptions log.

library(readxl)
library(dplyr)
library(openxlsx)
library(magrittr)
library(tidyr)
library(stringr)

CreateGeneration <- function(AssumptionsLog = system.file("extdata", "AssumptionsLog.xlsx", package = "CEPA"),
                             GenerationFile = system.file("extdata", "INPUT_DATA_GENERATION - Blank.xlsx", package = "CEPA"),
                             Year = "2021", wkdir = getwd()){

  #Read in the data

  SRMC <- readxl::read_xlsx(AssumptionsLog, sheet = "SRMC") %>%
    tidyr::pivot_longer(cols = starts_with("SRMC"), names_to = "YearLong", values_to = "VARCOST") %>%
    dplyr::mutate(year = stringr::str_extract(YearLong, pattern = "(\\d+)")) %>% dplyr::filter(year == Year) %>%
    dplyr::select(-YearLong)

  EndoCapacity <- readxl::read_xlsx(AssumptionsLog, sheet = "EndoCapacity") %>%
    tidyr::pivot_longer(cols = starts_with("Cap "), names_to = "YearLong", values_to = "CAPACITY") %>%
    dplyr::mutate(year = stringr::str_extract(YearLong, pattern = "(\\d+)")) %>% dplyr::filter(year == Year) %>%
    dplyr::select(-YearLong)

    GenerationSpecs <- readxl::read_xlsx(AssumptionsLog, sheet = "GenerationSpecs")

  #Create a combined DATA

    DATA <- dplyr::left_join(GenerationSpecs, EndoCapacity, by = c('STATION','DUID','REGION','REGION_KEY')) %>%
      dplyr::left_join(SRMC, by = c('STATION','DUID','REGION','REGION_KEY','year'))

  #Filter for available date
  #Select relevant columns for DATA to construct the Generation input sheet

    DATA <- dplyr::filter(DATA, AvaliableDate <= Year & ExpectedRetirementYear >= Year)

    DATA <- dplyr::select(DATA, STATION, DUID, REGION, REGION_KEY, CAPACITY, `CARBON INTENSITY`,
                   EFFICIENCY_LOSS, GEN_TECH_TEXT, GEN_TECH, GEN_TYPE, HYDRO_UNITS_KEY,
                   INITIAL_ENERGY_STOCK_IN_DAMS, HYDRO_DAM_CAPACITY, HYDRO_DISCHARGE_CAPACITY,
                   COST_WATER_SPILL, FUEL_TYPE_TEXT, FUEL_TYPE, HEATRATE_GJ, HEATRATE,
                   MAXP_FACTOR, MIN_DOWNTIME, MIN_UPTIME, MINP_ISP, MINP_GHD, MINP_FACTOR,
                   RAMPDOWN_ISP, RAMPDOWN_GHD, RAMPDOWNFACTOR, RAMPUP_ISP, RAMPUP_GHD,
                   RAMPUPFACTOR, SHUTRAMPFACTOR, STARTCOST_ISP, STARTCOST_GHD, STARTCOST,
                   STARTRAMPFACTOR, VARCOST, INITIAL_STORAGE_STOCK, MAX_OUTPUT_HOURS,
                   MIN_OUTPUT_HOURS, MAX_STORAGE, COMPLETE)

  #Load the gen wokrbook

    wb <- loadWorkbook(GenerationFile)

    writeData(wb, sheet = "DATA", x=DATA, startCol = 1, startRow = 1)

    saveWorkbook(wb, paste0(wkdir,"/INPUT_DATA_GENERATION.xlsx"), overwrite = TRUE)

}
