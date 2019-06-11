## SPX,1614.96,*,SPX131019P01575000,,put,10/19/2013,07/01/2013,1575,44.2,43.5,45.3,20,1368,0.1827,-0.3793,0.2349,-27.5723,337.3409,SPX131019P01575000

## Date 1 Mo 3 Mo 6 Mo 1 Yr 2 Yr 3 Yr 5 Yr 7 Yr 10 Yr 20 Yr 30 Yr 07/01/13 0.01
## 0.04 0.09 0.15 0.34 0.65 1.39 1.93 2.50 3.19 3.48


n <- 10000

S <- 1614.96  # Standard and Poors 500 Index, on 07/01/2013
r <- 0.008  # implied risk free interest rate, between 3 year and 5 year T-bill rate
sigma <- 0.1827  # implied volatility
K <- 1575  # strike price
Tminust <- 110/365  # 07/01/2013 to 10/19/2013

## the true value from the Black Scholes Formula for put option
numerd1 <- log(S/K) + (r + sigma^2/2) * (Tminust)
numerd2 <- log(S/K) + (r - sigma^2/2) * (Tminust)
d1 <- numerd1/(sigma * sqrt(Tminust))
d2 <- numerd2/(sigma * sqrt(Tminust))
part1 <- S * (pnorm(d1) - 1)
part2 <- K * exp(-r * (Tminust)) * (pnorm(d2) - 1)
VP <- part1 - part2

x <- rnorm(n)
y1 <- sapply(x, function(x) max(0, K - S * exp((r - sigma^2/2) * Tminust + sigma * 
    x * sqrt(Tminust))))
y2 <- sapply(-x, function(x) max(0, K - S * exp((r - sigma^2/2) * Tminust + sigma * 
    x * sqrt(Tminust))))  # antithetic sample application
y <- (y1 + y2)/2

u <- rexp(n, rate = 0.5)
tildeg <- function(x) (max(0, K - S * exp((r - sigma^2/2) * Tminust - sigma * sqrt(Tminust) * 
    sqrt(x))) + max(0, K - S * exp((r - sigma^2/2) * Tminust + sigma * sqrt(Tminust) * 
    sqrt(x))))/sqrt(2 * pi * x)
u1 <- sapply(u, tildeg)  # importance sample application

mc100 <- t.test(exp(-r * Tminust) * y1[1:100])  # first 100 Monte Carlo sampling simulations
mc1000 <- t.test(exp(-r * Tminust) * y1[1:1000])  # first 1000 Monte Carlo sampling simulations
mcall <- t.test(exp(-r * Tminust) * y1)  # all Monte Carlo sampling simulation results

anti100 <- t.test(exp(-r * Tminust) * y[1:100])  # first 100 antithetic sampling simulations
anti1000 <- t.test(exp(-r * Tminust) * y[1:1000])  # first 1000 antithetic sampling simulations
antiall <- t.test(exp(-r * Tminust) * y)  # all antithetic sampling simulation results

imp100 <- t.test(exp(-r * Tminust) * u1[1:100])  # first 100 importance sampling simulations
imp1000 <- t.test(exp(-r * Tminust) * u1[1:1000])  # first 1000 importance sampling simulations
impall <- t.test(exp(-r * Tminust) * u1)  # all importance sampling simulation results

type <- c("Blacksholes", "Monte Carlo 100", "Monte Carlo 1000", "Monte Carlo all", 
    "Antithetic 100", "Antithetic 1000", "Antithetic all", "Importance 100", "Importance 1000", 
    "Importance all")
putestimate <- c(VP, mc100$estimate, mc1000$estimate, mcall$estimate, anti100$estimate, 
    anti1000$estimate, antiall$estimate, imp100$estimate, imp1000$estimate, impall$estimate)
putconfintleft <- c(NA, mc100$conf.int[1], mc1000$conf.int[1], mcall$conf.int[1], 
    anti100$conf.int[1], anti1000$conf.int[1], antiall$conf.int[1], imp100$conf.int[1], 
    imp1000$conf.int[1], impall$conf.int[1])
putconfintright <- c(NA, mc100$conf.int[2], mc1000$conf.int[2], mcall$conf.int[2], 
    anti100$conf.int[2], anti1000$conf.int[2], antiall$conf.int[2], imp100$conf.int[2], 
    imp1000$conf.int[2], impall$conf.int[2])
d <- data.frame(type, putestimate, putconfintleft, putconfintright)

print(d)

## NAME: SPXsimulation.R
##
## USAGE: within R, at interactive prompt
##        source("SPXsimulation.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: The purpose of this script is to calculate the value of a put option
## on the Standard & Poors 500 stock index (SPX).   First
## the script uses the Black Scholes formula for a put option to calculate the
## theoretical value for a put option.  Then the script uses Monte Carlo
## simulation based on the Geometric Brownian Motion with the SPX
## parameters to find an estimate for the put option value.  Finally, the script
## uses Monte Carlo simulation with antithetic sampling and importance sampling
## variance reduction methods to refine the estimates.  The ultimate purpose
## is to compare the various Monte Carlo methods with the put option value.
##              
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: R 
## DEPENDENCIES: none
## INCOMPATIBILITIES: none known
## PROVENANCE: created by sdunbar
## BUGS AND LIMITATIONS: none known
## FEATURES AND POTENTIAL IMPROVEMENTS:
## SPX data is from: https://www.historicaloptiondata.com/
## Interest rates are from http://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldYear&year=2013
## Profiling with Rprof() indicates that most of the script time is occupied in
## the applications of sapply() and an implicit lapply() probably also in conjunction
## with the definition of the tildeg function.
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 as of Wed Aug 26, 2015 10:26 AM
##          Version 1.1 as of Fri Jan 22, 2016  7:24 AM, added present
##                      value discounting to correct the calculations.
## KEYWORDS: Monte Carlo simulation, put option, Geometric Brownian Motion, variance reduction, antithetic sampling, importance sampling


