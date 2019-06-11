p <- 0.5
n <- 500
k <- 1000
coinFlips <- array( 0+(runif(n*k) <= p), dim=c(n,k)) 
     # 0+ coerces Boolean to numeric
winLose = 2 * coinFlips - 1
# -1 for Tails, +1 for Heads
excessHeads <- colSums( winLose) 
# −n..n (every other integer) binomial rv sample 
# the second argument ‘‘2’’ means column−wise

s <- 20
prob <- sum( 0+(abs(excessHeads) > s) )/k
theoretical <- 2*(1-pnorm( (s+0.5)/sqrt(n), mean=0, sd=1))
cat(sprintf("Empirical probability: %f \n", prob ))
cat(sprintf("Excess Heads estimate: %f \n", theoretical))

## NAME: excessheads.R
##
## USAGE: within R, at interactive prompt
##        source("excessheads.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: Experiment of flipping a coin n times, 
##              and repeat the experiment k times.  
##		Compare probability of excess binomial
##		to standard normal cdf
## DIAGNOSTICS: None
## CONFIGURATION AND ENVIRONMENT: None
## DEPENDENCIES: None
## INCOMPATIBILITIES:  None known
## PROVENANCE:  Created Fri Dec 21, 2012  5:35 AM
## BUGS AND LIMITATIONS:  None known
## FEATURES AND POTENTIAL IMPROVEMENTS:  None at this time
## Note: Profiling shows the majority of time (almost 99%) 
##       is spent on the line
## 	  array( 0+(runif(n*k) <= p), dim=c(n,k)) 
##	  with over 50% spent on the function runif
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 Fri Dec 21, 2012  5:35 AM
## KEYWORDS: Coin flips, binomial random variable,
## 	     DeMoivre-Laplace Central Limit Theorem


