\name{roll}
\alias{roll}
\title{Roll the dice a number of times given and sum the result obtained}
\usage{
  \code{roll(nside, ndice, times)}

  \code{roll(nside, n)}
}
\arguments{
  \item{nside}{number of sides of the dice to roll}

  \item{ndice}{numer of dices to roll}

  \item{times}{number of rolls -how many times the
  experiment is repeated}

  \item{n}{number of dices to roll. Optionally this param
  may be a vector to set more than more experiment}
}
\value{
  \code{roll} returns the sum of the results of each dice
  rolled. In case the function was called with a vector of
  experiments as param n, a vector ir returned with the
  results for each experiment
}
\description{
  Roll the dice a number of times given and sum the result
  obtained
}
\details{
  It makes a roll where the number of dices and the type of
  them is given. All dices must be of the same type, i.e.
  they all have the same number od sides.

  Alternatively, in order to make more than one roll, a
  vector of numerics may be given as \code{n} param. The
  number of each element indicates the number of dices
  rolled in each experiment. For instance, in order to roll
  two dices four times, use the param \code{rep(2, len=4)}
}
\examples{
require(dices)
# Make a roll with two dices of six sides
roll(6,2)
roll(6,2,1)

# Make four rolls with two dices of six sides
roll(6, rep(2, len=4))
roll(6, 2, 4)

# Make four rolls each one with one dice more starting with one
roll(6, 1:4)
}
\author{
  Miguel Coronado (miguelcb84@gmail.com)
}

