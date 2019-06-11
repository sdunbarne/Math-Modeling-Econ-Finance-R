set.seed(2015)
n <- 10000

sigma <- 1
K <- 1

## the true value
VP <- K * pnorm(-(log(1/K) - sigma^2/2)/sigma) - pnorm(-(log(1/K) + sigma^2/2)/sigma)

x <- rnorm(n)
y <- sapply(x, function(x) max(0, K - exp(sigma * x - sigma^2/2)))

mc100 <- t.test(y[1:100])  # first 100 simulations
mc1000 <- t.test(y[1:1000])  # first 1000 simulations
mcall <- t.test(y)  # all simulation results

type <- c("Blacksholes", "Monte Carlo 100", "Monte Carlo 1000", "Monte Carlo all")
putestimate <- c(VP, mc100$estimate, mc1000$estimate, mcall$estimate)
putconfintleft <- c(NA, mc100$conf.int[1], mc1000$conf.int[1], mcall$conf.int[1])
putconfintright <- c(NA, mc100$conf.int[2], mc1000$conf.int[2], mcall$conf.int[2])
d <- data.frame(type, putestimate, putconfintleft, putconfintright)

print(d)

##
## USAGE: within R, at interactive prompt
##        source("optionsimulation.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: The purpose of this script is to calculate the value of a
## simplified put option.   The security price is simplified by assuming
## the risk-free interest rate is 0, the starting security price is 1,
## the standard deviation is 1, the strike price is 1, and
## the time to expiration is 1.  First
## the script uses the Black Scholes formula for a put option to calculate the
## theoretical value for a put option.  Then the script uses Monte Carlo
## simulation to find an estimate and confidence intervals for the put option value.  
##              
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: R
## DEPENDENCIES:  none
## INCOMPATIBILITIES: none known
## PROVENANCE: created by sdunbar
## BUGS AND LIMITATIONS: none known
## FEATURES AND POTENTIAL IMPROVEMENTS:

## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 as of Tue Aug 18, 2015  8:27 AM
## KEYWORDS: Monte Carlo simulation, put option, Geometric Brownian Motion,


