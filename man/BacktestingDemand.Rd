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
  \item{year}{ Default = 2020. Set the year you want to backtest.
%%     ~~Describe \code{x} here~~
}
}
\author{
Mitchell Scott
}
\note{
The resulting files are xlsx. Unfortunately the model requires xlsm files. You will need to open each of the INPUT_DATA_ZONAL files and resave them as xlsm before loading them into the model
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
BacktestingDemand()

## You will receive a prompt:

# Please enter the file path that points to the 'Demand' and 'Generation' input files.
# For example 'C:/Users/mitchell.scott/CEPA/4132_DISER_MarinusLinkRevenueOptions - Documents/WS1_MarketModel/Raw Data'.
# File path:
{
  }
}

