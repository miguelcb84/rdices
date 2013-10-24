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
  A function
}
\description{
  Create a dice function that rolls a dice of the given
  number of sides.
}
\details{
  The function that makedice produce may be called with
  just one argument for the number of times the dice is
  rolled. The value that fucntion return is a vector with
  the resulting value of each roll.

  For more complex rolls with more than one dice see
  \code{\link{roll}} function.
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
\seealso{
  \code{\link{roll}}
}

