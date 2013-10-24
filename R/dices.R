#' Create a dice function that rolls a dice of the given number of sides.
#'
#' @param nside number of sides of the dice that will be created 
#' @examples 
#' require(dices)
#' # create a six-sides dice
#' dice <- makedice(2)
#' dice(1) # roll it once
#' dice(2) # roll it twice more
#' 
#' @author Miguel Coronado (miguelcb84@@gmail.com)
#' @return function
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


#' Random rolls of dices of a certain number of sides
#' 
#' @param times number of observations
#' @param nside number of sides of the dices
#' @param ndice number of dices rolled in each observation
#' 
#' @author Miguel Coronado (miguelcb84@@gmail.com)
#' @return \code{rdices} returns a vector with the sum of the results of the 
#'  dices for each observation
#'  
#' @examples
#' require(dices)
#' # Make a hundred observations of two dices of six sides
#' roll(100, 6,2)
#' 
rdices <- function(times, nside, ndice){
  roll(nside, ndice, times)
}

#' Generate all different possible combinations of a roll.
#' @param nside number of sides of the dices
#' @param ndice number of dices rolled in each observation
#' 
#' @details The density information is included in the results, so each 
#'  results appears as many times accordint to the theoretical probability.
#' 
#' @author Miguel Coronado (miguelcb84@@gmail.com)
#' @return A vector with the sums of the results of each combination
#' 
#' @examples
#' require(dices)
#' # All possible results
#' res <- tdice(6,3)
#' 
#' table(res) # statistical table
#' 
#' plot(res) # plot the results
#' plot(table(res)) # plot the results
#' 
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


#' Density distribution 
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


#' Distribution function 
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