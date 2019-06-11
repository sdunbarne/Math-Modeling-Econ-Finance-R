set.seed(2015)
n <- 10000

S <- 1
sigma <- 1
r <- sigma^2/2
K <- 1
Tminust <- 1

## the value of the put option by Black-Scholes
d2 <- (log(S/K) + (r - sigma^2/2) * (Tminust))/(sigma * sqrt(Tminust))
d1 <- (log(S/K) + (r + sigma^2/2) * (Tminust))/(sigma * sqrt(Tminust))
VP <- K * exp(-r * Tminust) * pnorm(-d2) - S * pnorm(-d1)

x <- rlnorm(n)  #Note use of default meanlog=0, sdlog=1
y <- sapply(x, function(z) max(0, K - z))

mc100 <- t.test(exp(-r * Tminust) * y[1:100])  # first 100 simulations
mc1000 <- t.test(exp(-r * Tminust) * y[1:1000])  # first 1000 simulations
mcall <- t.test(exp(-r * Tminust) * y)  # all simulation results

type <- c("Blacksholes", "Monte Carlo 100", "Monte Carlo 1000", "Monte Carlo all")
putestimate <- c(VP, mc100$estimate, mc1000$estimate, mcall$estimate)
putconfintleft <- c(NA, mc100$conf.int[1], mc1000$conf.int[1], mcall$conf.int[1])
putconfintright <- c(NA, mc100$conf.int[2], mc1000$conf.int[2], mcall$conf.int[2])
d <- data.frame(type, putestimate, putconfintleft, putconfintright)

print(d)

x <- rnorm(n)
y1 <- sapply(x, function(z) max(0, K - exp(z)))
y2 <- sapply(-x, function(z) max(0, K - exp(z)))
y <- (y1 + y2)/2

anti100 <- t.test(exp(-r * Tminust) * y[1:100])  # first 100 simulations
anti1000 <- t.test(exp(-r * Tminust) * y[1:1000])  # first 1000 simulations
antiall <- t.test(exp(-r * Tminust) * y)  # all simulation results

type <- c("Blacksholes", "Antithetic 100", "Antithetic 1000", "Antithetic all")
putestimate <- c(VP, anti100$estimate, anti1000$estimate, antiall$estimate)
putconfintleft <- c(NA, anti100$conf.int[1], anti1000$conf.int[1], antiall$conf.int[1])
putconfintright <- c(NA, anti100$conf.int[2], anti1000$conf.int[2], antiall$conf.int[2])
d <- data.frame(type, putestimate, putconfintleft, putconfintright)

print(d)

xN <- rnorm(n)
yN <- sapply(xN, function(z) max(0, K - exp(z)))

mcN100 <- t.test(exp(-r * Tminust) * yN[1:100])  # first 100 simulations
mcN1000 <- t.test(exp(-r * Tminust) * yN[1:1000])  # first 1000 simulations
mcNall <- t.test(exp(-r * Tminust) * yN)  # all simulation results

type <- c("Blacksholes", "Monte Carlo 100", "Monte Carlo 1000", "Monte Carlo all")
putestimate <- c(VP, mcN100$estimate, mcN1000$estimate, mcNall$estimate)
putconfintleft <- c(NA, mcN100$conf.int[1], mcN1000$conf.int[1], mcNall$conf.int[1])
putconfintright <- c(NA, mcN100$conf.int[2], mcN1000$conf.int[2], mcNall$conf.int[2])
d <- data.frame(type, putestimate, putconfintleft, putconfintright)

print(d)

xE <- rexp(n, rate = 1/2)
yE <- sapply(xE, function(z) max(0, K - exp(-sqrt(z)))/(sqrt(2 * pi) * sqrt(z)))

mcE100 <- t.test(exp(-r * Tminust) * yE[1:100])  # first 100 simulations
mcE1000 <- t.test(exp(-r * Tminust) * yE[1:1000])  # first 1000 simulations
mcEall <- t.test(exp(-r * Tminust) * yE)  # all simulation results

type <- c("Blacksholes", "Monte Carlo 100", "Monte Carlo 1000", "Monte Carlo all")
putestimate <- c(VP, mcE100$estimate, mcE1000$estimate, mcEall$estimate)
putconfintleft <- c(NA, mcE100$conf.int[1], mcE1000$conf.int[1], mcEall$conf.int[1])
putconfintright <- c(NA, mcE100$conf.int[2], mcE1000$conf.int[2], mcEall$conf.int[2])
d <- data.frame(type, putestimate, putconfintleft, putconfintright)

print(d)

## NAME:  optionsimulation.R
## USAGE: within R, at interactive prompt
##        source("optionsimulation.R")
## REQUIRED ARGUMENTS: none
## OPTIONS: none
## DESCRIPTION: The purpose of this script is to calculate the value of a
## simplified put option.   The security price is simplified by assuming
## the risk-free interest rate is sigma^2/2, the starting security price is 1,
## the standard deviation is 1, the strike price is 1, and
## the time to expiration is 1.  First
## the script uses the Black Scholes formula for a put option to calculate the
## theoretical value for a put option.  Then the script uses Monte Carlo
## simulation to find an estimate and confidence intervals for the put option value.  
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


