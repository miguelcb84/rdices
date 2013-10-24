#' Create a dice function that rolls a dice of the given number of sides.
#'
#' @param nside number of sides of the dices that will be created
#' @return function
makedice <- function(nside){
  dice <- function(n = 1){
    sample(1:nside, n, replace = TRUE)
  }
  dice
  
}

#' Roll the dice a number of times given and sum the result obtained
#' 
#' @param nside Number of sides of the dice to roll
#' @param n number of dices to roll. Alternatively, this param may be a
#'  vector: each elemen contains the number of dices that will be rolled for 
#'  that experiment. For instance, in order to roll 2 dices four times, use 
#'  the param \code{rep(2, len=4)}
#' @return the sum of the results of each dice rolled. In case the function 
#'  was called with a vector of experiments as param n, a vector ir returned
#'  with the results for each experiment
roll <- function(nside, n){
  dice <- makedice(nside)
  roll <- lapply(X=n, FUN=dice)
  #print(roll)
  sapply(roll, FUN=sum)
}


roll <- function(nside, ndice, times){
  dice <- makedice(nside)
  X <- rep(ndice, len=times)
  roll <- lapply(X=X, FUN=dice)
  #print(roll)
  sapply(roll, FUN=sum)
}


rdices <- function(times, nside, ndice){
  roll(nside, ndice, times)
}



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
  apply(data, 1, sum)
}

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

pdices <- function(x, nside, ndice){
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
  ret <- lapply(x, function(x) { df$NFreq[df$data<=x] })
  ret <- sapply(ret, sum)
  ret[is.na(ret)] <- 0
  ret
}