\name{makedice}
\alias{makedice}
\title{Create a dice function that rolls a dice of the given number of sides.}
\usage{
  makedice(nside)
}
\arguments{
  \item{nside}{number of sides of the dice that will be
  created}
}
\value{
  function
}
\description{
  Create a dice function that rolls a dice of the given
  number of sides.
}
\examples{
require(dices)
# create a six-sides dice
dice <- makedice(2)
dice(1) # roll it once
dice(2) # roll it twice more
}
\author{
  Miguel Coronado (miguelcb84@gmail.com)
}

