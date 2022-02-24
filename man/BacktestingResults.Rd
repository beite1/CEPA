\name{BacktestingResults}
\alias{BacktestingResults}
\title{
BacktestingResults
}
\description{
This function will take the input and output modelling files and real data from Neopoints, presenting results on prices, generation by technology and interconnector flows. The output from this function is a list of lists - as symmetric as possible for actual and modelled data.
}
\usage{
BacktestingResults(wkdir,rawdir,year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wkdir}{Enter the location of the 'INPUT' and 'Output' files from modeling. This should be the "MODEL IN-OUT" folder in the DISER project folder.}
  \item{rawdir}{Enter the location of the actual 'Demand', 'Generation', 'Price' and 'ICFlow' data. This should be the "Raw Data" folder in the DISER project folder.}
  \item{year}{Default = 2020. Set the year you want to backtest.}
%%     ~~Describe \code{x} here~~
}
\value{
Output is a list of lists. At the first level it branches into "Actual" and "Model". At the second level it branches into price, IC flows and generation. At the final level there is the DATA frame plus daily, monthly and annual summaries.
}
\author{
Mitchell Scott
}

\examples{
## Not Run ##
library(devtools)
install_github("beite1/CEPA")
library(CEPA)

wkdir = "C:/Users/mitchell.scott/CEPA/4132_DISER_MarinusLinkRevenueOptions - Documents/WS1_MarketModel/MODEL IN-OUT"
rawdir = "C:/Users/mitchell.scott/CEPA/4132_DISER_MarinusLinkRevenueOptions - Documents/WS1_MarketModel/Raw Data"

BacktestingResults(wkdir,rawdir)
}

