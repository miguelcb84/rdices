Dices R Package
========================================================

This package provides several functions related to statistical calculation with *dices* and *rolls*. We define a roll, as a observation of the results obtained in the throw of pool of dices of the same type. We define the type of a dice by the number of sides.

Formally, the definition of the discrete random distribution, *dices* by the probability of obtain a certain result in a roll, which is the sum of all the results of the dices involved in the roll. For instance, let be the random variable X = dices(nsides=6, ndices=2). The probability of obtaining a sum of seven is denoted P( X = 7 ). The probability of obtaining a result greater than ten is denoted P( X > 10 ). 

We define the *dimension of the space*, as the number of possible different results in the roll. It is important to point out that we are not talking about the sum but the specific result on each dice so having a roll with a result of 3 on the first dice and 4 on the second, is different from a roll of 4 in the first dice and 3 in the second, when talking about the dimension of the space. The *range* the the distribution is the set of possitive natural values that the rolls may sum, which is different from the dimension of the space. The length of the range is the difference between the maximun and the mininum of the range.

Dice package offers several functions, some of them statistically more formal (the *Xdice* family), and some other may be considered as helpers or casual functions (*makedice* and *roll*).

Usage overview
-----------------------------------------------

    makedice (nside)
    roll (nside, n)
    roll (nside, ndice, times)
    tdices (nside, ndice)
    rdices (times, ndice, nside)
    ddides (x, ndice, nside)
    pdices (q, ndice, nside)

### Arguments

+ **nside** number of sides of the dices.
+ **ndice** number of dices rolled in each observation.
+ **times** number of observations. If length(times) > 1, the length is taken to be the number required.
+ **x** vector of quantiles.
+ **q** vector of quantiles.

For further explanation of the each function see the R documentation.

### Examples

All examples in this file are executable R code. This file is written using specific R markadown language, that can be executed when rendered.

Helper Functions
----------------------------------------------

### makedice

It is a simple function creator that takes the number of the side os a dice and return a function that can be use to roll the dice a certain number of times.

    makedice (nside)

```{r}
library(dices)
dice <-  makedice(6)
dice(10)
```

With greater number of roll you may empirically study a dice rolling:
```{r}
set.seed(1)
roll <- dice(1000) 
table(roll) # All results are similar
summary(roll)
```

### roll

You may use roll function to throw a poll of dices. Two sintaxes are defined. 

    roll (nside, ndice, times)
    roll (nside, n)
    
```{r}
set.seed(1)
roll(6,2, 10)

set.seed(1)
roll(6, rep(2, len=10))
```

The difference relies on the way the size of the pool is specified. With the roll (nside, ndice, times) sintax, you specify the size of the poll with *ndice* and the number of rolls with parameter *times*. The roll (nside, n) sintax uses the vector *n* to specifie the size of the poll on each different thorw. Despite being wordy, this syntax let you select roll poll with different size.

```{r}
roll(6, 1:10) # Roll 10 times, each one with a dice more than the former roll, starting by one dice.
```


Statistical functions
--------------------------------------

### tdices

    tdices(nside, ndice)
    
tdices generates all different possible combinations of roll results, i.e it generates the space of results. The density information is included in the results, so each results appears as many times according to the theoretical probability -as explained when definig the dimension of the space.

```{r}
# All possible results
res <- tdices(6,3)

dim <- length(res)     # dimension of the space
ran <- range(res)      # range of results
ran.dim <- length(ran) # dimension the range

summary(res) # Summary
table(res) # Statistical table
```


```{r fig.width=11, fig.height=5}
par(mfrow=c(1,2))
plot(res, main="Space") # plot the results
plot(table(res), main="Aggregated results") # plot the results
```

### rdices

    rdices(times, nside, ndice)

rdices generates random rolls of the given number of dices of the number sides.


```{r}
# Make twenty observations of rolls of two dices of six sides
set.seed(1)
rdices(20, 6,2)
set.seed(1)
rdices(rep(0, len=20), 6,2)
```

### ddices

     ddices(x, nside, ndice)
     
Density of the dices random variable, as previously defined.

It can be used to obtain the probability of obtaining an exact result of 7 when rolling two dices.
```{r}
ddices(7, 6, 2)
```

I can compute the probability given by P(5 <= X <= 8) for X ~ Dices(nside=6, ndice=2).
```{r}
sum(ddices(5:8, 6, 2))
# or alternatively
pdices(8, 6, 2) - pdices(4, 6, 2)
# or
diff(pdices(c(4,8), 6, 2))
```


### pdices

    pdices(q, nside, ndice)
    
Distribution function for the dices random variable, as previously defined.

Compute P(X < 8) for X, a random variable of \code{Dices(ndice=2, nside=6)}
```{r}
pdices(7, 6, 2)
```

Compute P(X > 8) for X, a random variable of \code{Dices(ndice=2, nside=6)}
```{r}
1 - pdices(8, 6, 2)
```

You can use it to print the density and distribution function.
```{r fig.width=7, fig.height=6}
# Print density and distribution
p <- pdices(1:20,6,3)
d <- ddices(1:20,6,3)
par(mar=c(3, 6, 3, 3))
plot(d, type="o", ylab=NA, xlab=NA)
par(new=TRUE)
plot(p, type="o", ylab=NA, axes=FALSE)
axis(side=2, line=2.5, ylim=pretty(range(p)))
```


