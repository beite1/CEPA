\name{CreateGeneration}
\alias{CreateGeneration}
\title{CreateGeneration}
\usage{
CreateGeneration(AssumptionsLog = "AssumptionsLog.xlsx", GenerationFile = "INPUT_DATA_GENERATION - Blank.xlsx", Year = "2021")
}
\arguments{

  \item{AssumptionsLog}{A version of the curent assumptions log. A version is provided with this package.}

  \item{GenerationFile}{A blank generation file.}

  \item{Year}{The year you want to create a generation file for.}

  \item{wkdir}{The working directory where you would like to save the INPUT_DATA_GENERATION file.}

}

\description{

This function creates a generation file to be used with the AIMMS solver. The inputs are an assumptions log (in .xlsx format), a blank generation file (.xlsx) and the year you want to create a file for.

}
\examples{

  setwd("~/DATA IN/GENERATION INPUT")

CreateGeneration(wkdir = getwd())

}
