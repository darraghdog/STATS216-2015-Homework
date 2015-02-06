### 5.R.R1######################



require(ISLR)
require(boot)

setwd("C:/Users/Think/Google Drive/stats216/OnlineClass/Ch5")
load("5.R.RData")

fit <- lm(y ~ ., data = Xy)
coef(summary(fit))

### 5.R.R3######################

# function to obtain SE from the data 
rsq <- function(formula, data, indices) {
  d <- Xy[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(coef(summary(fit))[, 1])
} 
# bootstrapping with 1000 replications 
boot(data=Xy, statistic=rsq, 
                R=1000, formula=y ~ .)

### 5.R.R4######################

# Finally, use the block bootstrap to estimate s.e.(??^1). Use blocks of 
# 100 contiguous observations, and resample ten whole blocks with replacement
# then paste them together to construct each bootstrap time series. 
# For example, one of your bootstrap resamples could be:
# There's a pretty basic error in your code. Take a look at it more closely, please. Here's a hint: 
# it doesn't have to do with the tsboot function.


boot.fn.ts = function(data){
  fit <- lm(y ~ ., data)
  return(coef(fit))
}

#Now use block bootstrapp using the tsboot function
tsboot(Xy, boot.fn.ts, R = 1000, sim = "fixed", l = 100)

################################