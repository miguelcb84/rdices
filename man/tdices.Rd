\name{tdices}
\alias{tdices}
\title{Generate all different possible combinations of a roll.}
\usage{
  tdices(nside, ndice)
}
\arguments{
  \item{nside}{number of sides of the dices}

  \item{ndice}{number of dices rolled in each observation}
}
\value{
  A vector with the sums of the results of each combination
}
\description{
  Generate all different possible combinations of a roll.
}
\details{
  The density information is included in the results, so
  each results appears as many times accordint to the
  theoretical probability.
}
\examples{
require(dices)
# All possible results
res <- tdice(6,3)

table(res) # statistical table

plot(res) # plot the results
plot(table(res)) # plot the results
}
\author{
  Miguel Coronado (miguelcb84@gmail.com)
}

