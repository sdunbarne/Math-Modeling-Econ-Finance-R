p <- 0.5
n <- 10000
k <- 1000
coinFlips <- array( 0+(runif(n*k) <= p), dim=c(n,k)) 
     # 0+ coerces Boolean to numeric
headsTotal <- colSums(coinFlips)
# 0..n binomial rv sample, size k

mu <- p
sigma <- sqrt(p*(1-p))
a <- 0.5
Zn <- (headsTotal - n*mu)/(sigma * sqrt(n))
prob <- sum( 0+(Zn < a) )/k
theoretical <- pnorm(a, mean=0, sd=1)
cat(sprintf("Empirical probability: %f \n", prob ))
cat(sprintf("Central Limit Theorem estimate: %f \n", theoretical))

## NAME: centrallimittheorem.R
##
## USAGE: within R, at interactive prompt
##        source("centrallimittheorem.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: Experiment of flipping a coin n times, 
##              and repeat the experiment k times.  
##		Compare probability of normalized binomial
##		to standard normal cdf
## DIAGNOSTICS: None
## CONFIGURATION AND ENVIRONMENT: None
## DEPENDENCIES: None
## INCOMPATIBILITIES:  None known
## PROVENANCE:  Created 
## BUGS AND LIMITATIONS:  None known
## FEATURES AND POTENTIAL IMPROVEMENTS:  None at this time
## Note: Profiling shows the majority of time (almost 99%) 
##       is spent on the line
## 	  array( 0+(runif(n*k) <= p), dim=c(n,k)) 
##	  with over 50% spent on the function runif
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 Fri Dec 14, 2012  5:55 AM
## KEYWORDS: Coin flips, binomial random variable,
## 	     DeMoivre-Laplace Central Limit Theorem


