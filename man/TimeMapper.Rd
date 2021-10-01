\name{TimeMapper}
\alias{TimeMapper}
\title{TimeMapper}
\usage{
timeMapper(spot_years, horizon_length, first_horizon, last_horizon, first_time, last_time)
}
\arguments{

  \item{spot_years}{A vector of spot years.}

  \item{horizon_length}{A number representing the horizon length.}

  \item{first_horizon}{A number representing the first horizon of interest.}

  \item{last_horizon}{A number representing the last horizon of interest.}

  \item{first_time}{A number representing the first period.}

  \item{last_time}{A number representing the last period.}

}

\description{

This function creates a mapping between model horizons and periods and human understood time.

}
\examples{
spot_years <- 2021:2049

horizon_length <- 70
first_horizon <- 11
last_horizon <- 136

first_time <- 45
last_time <- 54

time_mapping <- timeMapper(spot_years, horizon_length, first_horizon, last_horizon, first_time, last_time)

}
