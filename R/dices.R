#' Create a dice function that rolls a dice of the given number of sides.
#'
#' @param nside number of sides of the dice that will be created 
#' 
#' @details The function that makedice produce may be called with just 
#'  one argument for the number of times the dice is rolled. The value
#'  that fucntion return is a vector with the resulting value of each roll.
#'  
#'  For more complex rolls with more than one dice see \code{\link{roll}} function.
#' 
#' @examples 
#' require(dices)
#' # create a six-sides dice
#' dice <- makedice(2)
#' dice(1) # roll it once
#' dice(2) # roll it twice more
#' 
#' @author Miguel Coronado (miguelcb84@@gmail.com)
#' @return A function
#' 
#' @seealso \code{\link{roll}} 
#' 
makedice <- function(nside){
  dice <- function(n = 1){
    sample(1:nside, n, replace = TRUE)
  }
  dice
  
}

#' Roll the dice a number of times given and sum the result obtained
#' 
#' @usage
#'  \code{roll(nside, ndice, times)}
#'  
#'  \code{roll(nside, n)} 
#' 
#' @param nside number of sides of the dice to roll
#' @param ndice numer of dices to roll
#' @param times number of rolls -how many times the experiment is repeated
#' @param n number of dices to roll. Optionally this param may be a
#'  vector to set more than more experiment
#' 
#' @details It makes a roll where the number of dices and the type of them 
#'  is given. All dices must be of the same type, i.e. they all have the same 
#'  number od sides.
#'  
#'  
#'  Alternatively, in order to make more than one roll, a vector of numerics
#'  may be given as \code{n} param. The number of each element indicates the 
#'  number of dices rolled in each experiment. For instance, in order to 
#'  roll two dices four times, use the param \code{rep(2, len=4)}
#'   
#' @author Miguel Coronado (miguelcb84@@gmail.com)
#' @return \code{roll} returns the sum of the results of each dice rolled. In case the function 
#'  was called with a vector of experiments as param n, a vector ir returned
#'  with the results for each experiment
#'  
#' @examples
#' require(dices)
#' # Make a roll with two dices of six sides
#' roll(6,2)
#' roll(6,2,1)
#' 
#' # Make four rolls with two dices of six sides
#' roll(6, rep(2, len=4))
#' roll(6, 2, 4)
#' 
#' # Make four rolls each one with one dice more starting with one
#' roll(6, 1:4)
#' 
roll <- function(nside, n){
  dice <- makedice(nside)
  roll <- lapply(X=n, FUN=dice)
  sapply(roll, FUN=sum)
}

roll <- function(nside, ndice, times){
  dice <- makedice(nside)
  X <- rep(ndice, len=times)
  roll <- lapply(X=X, FUN=dice)
  sapply(roll, FUN=sum)
}


#' Density, distribution function, statistical space and random generation 
#' for distribution based on dice rolling.
#' 
#' @usage
#'  \code{tdices(nside, ndice)}
#'  
#'  \code{rdices(times, nside, ndice)}
#'  
#'  \code{ddices(x, nside, ndice)}
#'  
#'  \code{pdices(q, nside, ndice)}
#' 
#' @param nside number of sides of the dices.
#' @param ndice number of dices rolled in each observation.
#' @param times number of observations. If \code{length(times) > 1}, the length 
#'  is taken to be the number required.
#' @param x vector of quantiles.
#' @param q vector of quantiles.
#' 
#' @details 
#'  \code{tdices} generates all different possible combinations of a roll for 
#'  the given number of dices of the number sides.
#'  The density information is included in the results, so each 
#'  results appears as many times accordint to the theoretical probability.
#'  
#'  \code{rdices} generates random rolls of the given number of dices of the 
#'  number sides.
#' 
#'  The random variable for dice rolling is defined by the number of dices and
#'  their number of sides. 
#'  
#'  Density is given by the frequency of appearance of a given result in the 
#'  statistical outcome returned by \code{tdices} (the statistical space), i.e 
#'  the ratio of times a result appear in the space and the total number of 
#'  results.  
#' 
#' @author Miguel Coronado \email{miguelcb84@@gmail.com}
#' @return 
#' \code{tdices} A vector with the sums of the results of each combination
#' 
#' \code{rdices} returns a vector with the sum of the results of the 
#'  dices for each observation
#' 
#' The legth of the result is determined by \code{times} for \code{rbinom}, 
#' the number of different result in the space for \code{tdices},
#' and is the length of x or q for the other functions.
#' 
#' @examples
#' require(dices)
#' # All possible results
#' res <- tdice(6,3)
#' 
#' table(res) # statistical table
#' plot(res) # plot the results
#' plot(table(res)) # plot the results
#' 
#' # Make a hundred observations of two dices of six sides
#' rdices(100, 6,2)
#' rdices(rep(0, len=100), 6,2)
#' 
#' # What is the probability of obtaining an exact result of 7 when rolling two dices?
#' ddices(7, 6, 2)
#' 
#' # Compute P(5 <= X <= 8) for X, a random variable of \code{Dices(ndice=2, nside=6)}
#' sum(ddices(5:8, 6, 2))
#' 
#' # Compute P(X < 8) for X, a random variable of \code{Dices(ndice=2, nside=6)}
#' pdices(7, 6, 2)
#' 
#' # Compute P(X > 8) for X, a random variable of \code{Dices(ndice=2, nside=6)}
#' 1 - pdices(8, 6, 2)
tdices <- function(nside, ndice){
  combs <- nside^ndice
  data <- rep(0, combs*ndice)
  
  for(i in 1:ndice){
    #alloc <- ((i-1)*nside+1):(i*nside)
    alloc <- ((i-1)*combs+1):(i*combs)
    data[alloc] <- rep(1:nside, 
                       each=nside^(ndice-i), 
                       len=combs)
  }
  
  data <- matrix(data, ncol=ndice)
  table(apply(data, 1, sum))
}

#' Density, distribution function, statistical space and random generation 
#' for distribution based on dice rolling.
#' 
#' @usage
#'  \code{tdices(nside, ndice)}
#'  
#'  \code{rdices(times, nside, ndice)}
#'  
#'  \code{ddices(x, nside, ndice)}
#'  
#'  \code{pdices(q, nside, ndice)}
#' 
#' @param nside number of sides of the dices.
#' @param ndice number of dices rolled in each observation.
#' @param times number of observations. If \code{length(times) > 1}, the length 
#'  is taken to be the number required.
#' @param x vector of quantiles.
#' @param q vector of quantiles.
#' 
#' @details 
#'  \code{tdices} generates all different possible combinations of a roll for 
#'  the given number of dices of the number sides.
#'  The density information is included in the results, so each 
#'  results appears as many times accordint to the theoretical probability.
#'  
#'  \code{rdices} generates random rolls of the given number of dices of the 
#'  number sides.
#' 
#'  The random variable for dice rolling is defined by the number of dices and
#'  their number of sides. 
#'  
#'  Density is given by the frequency of appearance of a given result in the 
#'  statistical outcome returned by \code{tdices} (the statistical space), i.e 
#'  the ratio of times a result appear in the space and the total number of 
#'  results.  
#' 
#' @author Miguel Coronado \email{miguelcb84@@gmail.com}
#' @return 
#' \code{tdices} A vector with the sums of the results of each combination
#' 
#' \code{rdices} returns a vector with the sum of the results of the 
#'  dices for each observation
#' 
#' The legth of the result is determined by \code{times} for \code{rbinom}, 
#' the number of different result in the space for \code{tdices},
#' and is the length of x or q for the other functions.
#' 
#' @examples
#' require(dices)
#' # All possible results
#' res <- tdice(6,3)
#' 
#' table(res) # statistical table
#' plot(res) # plot the results
#' plot(table(res)) # plot the results
#' 
#' # Make a hundred observations of two dices of six sides
#' rdices(100, 6,2)
#' rdices(rep(0, len=100), 6,2)
#' 
#' # What is the probability of obtaining an exact result of 7 when rolling two dices?
#' ddices(7, 6, 2)
#' 
#' # Compute P(5 <= X <= 8) for X, a random variable of \code{Dices(ndice=2, nside=6)}
#' sum(ddices(5:8, 6, 2))
#' 
#' # Compute P(X < 8) for X, a random variable of \code{Dices(ndice=2, nside=6)}
#' pdices(7, 6, 2)
#' 
#' # Compute P(X > 8) for X, a random variable of \code{Dices(ndice=2, nside=6)}
#' 1 - pdices(8, 6, 2)
rdices <- function(times, nside, ndice){
  if(length(times)>1) times <- length(times)
  roll(nside, ndice, times)
}

#' Density, distribution function, statistical space and random generation 
#' for distribution based on dice rolling.
#' 
#' @usage
#'  \code{tdices(nside, ndice)}
#'  
#'  \code{rdices(times, nside, ndice)}
#'  
#'  \code{ddices(x, nside, ndice)}
#'  
#'  \code{pdices(q, nside, ndice)}
#' 
#' @param nside number of sides of the dices.
#' @param ndice number of dices rolled in each observation.
#' @param times number of observations. If \code{length(times) > 1}, the length 
#'  is taken to be the number required.
#' @param x vector of quantiles.
#' @param q vector of quantiles.
#' 
#' @details 
#'  \code{tdices} generates all different possible combinations of a roll for 
#'  the given number of dices of the number sides.
#'  The density information is included in the results, so each 
#'  results appears as many times accordint to the theoretical probability.
#'  
#'  \code{rdices} generates random rolls of the given number of dices of the 
#'  number sides.
#' 
#'  The random variable for dice rolling is defined by the number of dices and
#'  their number of sides. 
#'  
#'  Density is given by the frequency of appearance of a given result in the 
#'  statistical outcome returned by \code{tdices} (the statistical space), i.e 
#'  the ratio of times a result appear in the space and the total number of 
#'  results.  
#' 
#' @author Miguel Coronado \email{miguelcb84@@gmail.com}
#' @return 
#' \code{tdices} A vector with the sums of the results of each combination
#' 
#' \code{rdices} returns a vector with the sum of the results of the 
#'  dices for each observation
#' 
#' The legth of the result is determined by \code{times} for \code{rbinom}, 
#' the number of different result in the space for \code{tdices},
#' and is the length of x or q for the other functions.
#' 
#' @examples
#' require(dices)
#' # All possible results
#' res <- tdice(6,3)
#' 
#' table(res) # statistical table
#' plot(res) # plot the results
#' plot(table(res)) # plot the results
#' 
#' # Make a hundred observations of two dices of six sides
#' rdices(100, 6,2)
#' rdices(rep(0, len=100), 6,2)
#' 
#' # What is the probability of obtaining an exact result of 7 when rolling two dices?
#' ddices(7, 6, 2)
#' 
#' # Compute P(5 <= X <= 8) for X, a random variable of \code{Dices(ndice=2, nside=6)}
#' sum(ddices(5:8, 6, 2))
#' 
#' # Compute P(X < 8) for X, a random variable of \code{Dices(ndice=2, nside=6)}
#' pdices(7, 6, 2)
#' 
#' # Compute P(X > 8) for X, a random variable of \code{Dices(ndice=2, nside=6)}
#' 1 - pdices(8, 6, 2)
ddices <- function(x, nside, ndice){
  data <- tdices(nside, ndice)
  data <- table(data)
  
#  dens <- function(x){
#    ret <- 0
#    for(k in 1:length(data)){
#      if(names(data[k])==x)
#        ret <- data[k]/(nside^ndice)
#      ret <- as.numeric(ret)
#    }
#    ret
#  }  
#  sapply(x, dens)
  
  df <- data.frame(data)
  df$NFreq <- df$Freq/(nside^ndice)
  
  ret <- sapply(x, function(x) { df$NFreq[df$data==x] })
  ret <- as.numeric(ret)
  ret[is.na(ret)] <- 0
  ret
}

#' Density, distribution function, statistical space and random generation 
#' for distribution based on dice rolling.
#' 
#' @usage
#'  \code{tdices(nside, ndice)}
#'  
#'  \code{rdices(times, nside, ndice)}
#'  
#'  \code{ddices(x, nside, ndice)}
#'  
#'  \code{pdices(q, nside, ndice)}
#' 
#' @param nside number of sides of the dices.
#' @param ndice number of dices rolled in each observation.
#' @param times number of observations. If \code{length(times) > 1}, the length 
#'  is taken to be the number required.
#' @param x vector of quantiles.
#' @param q vector of quantiles.
#' 
#' @details 
#'  \code{tdices} generates all different possible combinations of a roll for 
#'  the given number of dices of the number sides.
#'  The density information is included in the results, so each 
#'  results appears as many times accordint to the theoretical probability.
#'  
#'  \code{rdices} generates random rolls of the given number of dices of the 
#'  number sides.
#' 
#'  The random variable for dice rolling is defined by the number of dices and
#'  their number of sides. 
#'  
#'  Density is given by the frequency of appearance of a given result in the 
#'  statistical outcome returned by \code{tdices} (the statistical space), i.e 
#'  the ratio of times a result appear in the space and the total number of 
#'  results.  
#' 
#' @author Miguel Coronado \email{miguelcb84@@gmail.com}
#' @return 
#' \code{tdices} A vector with the sums of the results of each combination
#' 
#' \code{rdices} returns a vector with the sum of the results of the 
#'  dices for each observation
#' 
#' The legth of the result is determined by \code{times} for \code{rbinom}, 
#' the number of different result in the space for \code{tdices},
#' and is the length of x or q for the other functions.
#' 
#' @examples
#' require(dices)
#' # All possible results
#' res <- tdice(6,3)
#' 
#' table(res) # statistical table
#' plot(res) # plot the results
#' plot(table(res)) # plot the results
#' 
#' # Make a hundred observations of two dices of six sides
#' rdices(100, 6,2)
#' rdices(rep(0, len=100), 6,2)
#' 
#' # What is the probability of obtaining an exact result of 7 when rolling two dices?
#' ddices(7, 6, 2)
#' 
#' # Compute P(5 <= X <= 8) for X, a random variable of \code{Dices(ndice=2, nside=6)}
#' sum(ddices(5:8, 6, 2))
#' 
#' # Compute P(X < 8) for X, a random variable of \code{Dices(ndice=2, nside=6)}
#' pdices(7, 6, 2)
#' 
#' # Compute P(X > 8) for X, a random variable of \code{Dices(ndice=2, nside=6)}
#' 1 - pdices(8, 6, 2)
pdices <- function(q, nside, ndice){
  # Generate the data
  data <- tdices(nside, ndice)
  # Create statistical table
  data <- table(data)
  
  # Convert into data.frame
  df <- data.frame(data)
  # Make index numeric so that we can subset using numerical comparisons
  df$data <- as.numeric(as.character(df$data))
  # Normalize frequency
  df$NFreq <- df$Freq/(nside^ndice) #
  
  # 
  ret <- lapply(q, function(x) { df$NFreq[df$data<=x] })
  ret <- sapply(ret, sum)
  ret[is.na(ret)] <- 0
  ret
}