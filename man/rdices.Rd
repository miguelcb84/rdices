\name{rdices}
\alias{rdices}
\title{Random rolls of dices of a certain number of sides}
\usage{
  rdices(times, nside, ndice)
}
\arguments{
  \item{times}{number of observations}

  \item{nside}{number of sides of the dices}

  \item{ndice}{number of dices rolled in each observation}
}
\value{
  \code{rdices} returns a vector with the sum of the
  results of the dices for each observation
}
\description{
  Random rolls of dices of a certain number of sides
}
\examples{
require(dices)
# Make a hundred observations of two dices of six sides
roll(100, 6,2)
}
\author{
  Miguel Coronado (miguelcb84@gmail.com)
}

