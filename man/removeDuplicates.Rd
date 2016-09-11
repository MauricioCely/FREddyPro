\name{removeDuplicates}
\alias{removeDuplicates}
\title{
  Remove duplicates
}
\description{
  The function looks at the timestamp of the data and identifies rows
  with same timestamp and removes one of the two
}
\usage{
removeDuplicates(data)
}
\arguments{
  \item{data}{
    Data frame 
  }
}
\author{
  Georgios Xenakis
}
\examples{
## Load the data
data(fluxes)

## Remove duplicates
fluxes<-removeDuplicates(fluxes)
}
