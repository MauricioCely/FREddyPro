\name{fillTimestamp}
\alias{fillTimestamp}
\title{
  Fill timestamp gaps
}
\description{
  Identify where there is a discontinuity in the timestamp and fill it
  with NA values
}
\usage{
fillTimestamp(data, timestamp = "timestamp", flux = FALSE)
}
\arguments{
  \item{data}{
    The data frame to use
}
  \item{timestamp}{
    A string with the timestamp column name
  }
  \item{flux}{
    Logical. If TRUE then the data frame is a standard EddyPro full
    output.
  }
}
\author{
  Georgios Xenakis
}

\examples{
## Load the data
data(fluxes)

## Clean fluxes
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,timesList=3,distCor=TRUE,
                   thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))

## Remove some line as an example
fluxes=fluxes[-c(832,833,834,835,840,953),]

## Create timestamp from date and time
fluxes=createTimestamp(fluxes)

## Now fill the gap timestmaps with NA's
fluxes=fillTimestamp(fluxes,flux=TRUE)
}
