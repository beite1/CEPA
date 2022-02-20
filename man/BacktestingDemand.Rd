\name{BacktestingDemand}
\alias{BacktestingDemand}
\title{BacktestingDemand}
\description{
This function will generate the INPUT_DATA_ZONAL files by downloading the data from Neopoints (if it has not already been downloaded). You will be prompted to enter where the raw data is saved on your local device.
}
\usage{
BacktestingDemand()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wkdir}{Enter the location for the 'Demand' and 'Generation' files.}
  \item{year}{ Default = 2020. Set the year you want to backtest.
  
}
}
\author{
Mitchell Scott
}
\note{
The resulting files are xlsx. Unfortunately the model requires xlsm files. You will need to open each of the INPUT_DATA_ZONAL files and resave them as xlsm before loading them into the model
}

\examples{
## Not Run ##
library(devtools)
install_github("beite1/CEPA")
library(CEPA)

setwd("~/Documents/CEPA_R/Backtesting")
BacktestingDemand(getwd())
{
  }
}

