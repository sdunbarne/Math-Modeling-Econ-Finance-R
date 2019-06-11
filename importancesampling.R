set.seed(2015)
n <- 10000

sigma <- 1
K <- 1

## the true value
VP <- K * pnorm(-(log(1/K) - sigma^2/2)/sigma) - pnorm(-(log(1/K) + sigma^2/2)/sigma)

x <- rexp(n, rate = 0.5)
tildeg <- function(x) (max(0, K - exp(-sigma^2/2 + sigma * sqrt(x))) + max(0, K - 
    exp(-sigma^2/2 - sigma * sqrt(x))))/sqrt(2 * pi * x)
y <- sapply(x, tildeg)

imp100 <- t.test(y[1:100])  # first 100 simulations
imp1000 <- t.test(y[1:1000])  # first 1000 simulations
impall <- t.test(y)  # all simulation results

type <- c("Blacksholes", "Importance 100", "Importance 1000", "Importance all")
putestimate <- c(VP, imp100$estimate, imp1000$estimate, impall$estimate)
putconfintleft <- c(NA, imp100$conf.int[1], imp1000$conf.int[1], impall$conf.int[1])
putconfintright <- c(NA, imp100$conf.int[2], imp1000$conf.int[2], impall$conf.int[2])
d <- data.frame(type, putestimate, putconfintleft, putconfintright)

print(d)

## NAME: importancesampling.R
##
## USAGE: within R, at interactive prompt
##        source("importancesampling.R")
## REQUIRED ARGUMENTS: none
## OPTIONS: none
## DESCRIPTION: The purpose of this script is to calculate the value of a
## simplified put option.   The security price is simplified by assuming
## the risk-free interest rate is 0, the starting security price is 1,
## the standard deviation is 1, the strike price is 1, and
## the time to expiration is 1.  First
## the script uses the Black Scholes formula for a put option to calculate the
## theoretical value for a put option.  Then the script uses Monte Carlo
## simulation with the importance sampling variance reduction method
## to find an estimate and confidence intervals for the put option value.  
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
## KEYWORDS: Monte Carlo simulation, importance sampling, put option, Geometric Brownian Motion,

