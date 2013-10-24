\name{roll}
\alias{roll}
\title{Roll the dice a number of times given and sum the result obtained}
\usage{
  roll(nside, ndice, times)
}
\arguments{
  \item{nside}{Number of sides of the dice to roll}

  \item{n}{number of dices to roll. Alternatively, this
  param may be a vector: each elemen contains the number of
  dices that will be rolled for that experiment. For
  instance, in order to roll 2 dices four times, use the
  param \code{rep(2, len=4)}}
}
\value{
  the sum of the results of each dice rolled. In case the
  function was called with a vector of experiments as param
  n, a vector ir returned with the results for each
  experiment
}
\description{
  Roll the dice a number of times given and sum the result
  obtained
}

