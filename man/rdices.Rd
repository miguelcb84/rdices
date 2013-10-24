\name{rdices}
\alias{rdices}
\title{Density, distribution function, statistical space and random generation
for distribution based on dice rolling.}
\usage{
  \code{tdices(nside, ndice)}

  \code{rdices(times, nside, ndice)}

  \code{ddices(x, nside, ndice)}

  \code{pdices(q, nside, ndice)}
}
\arguments{
  \item{nside}{number of sides of the dices.}

  \item{ndice}{number of dices rolled in each observation.}

  \item{times}{number of observations. If
  \code{length(times) > 1}, the length is taken to be the
  number required.}

  \item{x}{vector of quantiles.}

  \item{q}{vector of quantiles.}
}
\value{
  \code{tdices} A vector with the sums of the results of
  each combination

  \code{rdices} returns a vector with the sum of the
  results of the dices for each observation

  The legth of the result is determined by \code{times} for
  \code{rbinom}, the number of different result in the
  space for \code{tdices}, and is the length of x or q for
  the other functions.
}
\description{
  Density, distribution function, statistical space and
  random generation for distribution based on dice rolling.
}
\details{
  \code{tdices} generates all different possible
  combinations of a roll for the given number of dices of
  the number sides.  The density information is included in
  the results, so each results appears as many times
  accordint to the theoretical probability.

  \code{rdices} generates random rolls of the given number
  of dices of the number sides.

  The random variable for dice rolling is defined by the
  number of dices and their number of sides.

  Density is given by the frequency of appearance of a
  given result in the statistical outcome returned by
  \code{tdices} (the statistical space), i.e the ratio of
  times a result appear in the space and the total number
  of results.
}
\examples{
require(dices)
# All possible results
res <- tdice(6,3)

table(res) # statistical table
plot(res) # plot the results
plot(table(res)) # plot the results

# Make a hundred observations of two dices of six sides
rdices(100, 6,2)
rdices(rep(0, len=100), 6,2)

# What is the probability of obtaining an exact result of 7 when rolling two dices?
ddices(7, 6, 2)

# Compute P(5 <= X <= 8) for X, a random variable of \\code{Dices(ndice=2, nside=6)}
sum(ddices(5:8, 6, 2))

# Compute P(X < 8) for X, a random variable of \\code{Dices(ndice=2, nside=6)}
pdices(7, 6, 2)

# Compute P(X > 8) for X, a random variable of \\code{Dices(ndice=2, nside=6)}
1 - pdices(8, 6, 2)
}
\author{
  Miguel Coronado \email{miguelcb84@gmail.com}
}

