p <- 0.5
n <- 10000
k <- 1000
coinFlips <- array( (runif(n*k) <= p), dim=c(n,k)) 
# 0+ coerces Boolean to numeric
headsTotal <- colSums(coinFlips)
# 0..n binomial rv sample, size k

epsilon <- 0.01 
mu <- p
prob <- sum( abs( headsTotal/n - mu ) > epsilon )/k
cat(sprintf("Empirical probability: %f \n", prob ))

## NAME: lawlargenumbers.R
##
## USAGE: within R, at interactive prompt
##        source("lawlargenumbers.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: Experiment of flipping a coin n times, 
##              and repeat the experiment k times.  
##		Check probability of deviation from mean is 
##		less than one.
## DIAGNOSTICS: None
## CONFIGURATION AND ENVIRONMENT: None
## DEPENDENCIES: None
## INCOMPATIBILITIES:  None known
## PROVENANCE:  Created 
## BUGS AND LIMITATIONS:  None known
## FEATURES AND POTENTIAL IMPROVEMENTS:  None at this time
## Note: Profiling shows the majority of time is spent on the line
## 	  array( 0+(runif(n*k) <= p), dim=c(n,k)) 
##	  with over 50% spent on the function runif
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 Tue Dec  4, 2012  5:18 AM
## KEYWORDS: Coin flips, binomial random variable.

