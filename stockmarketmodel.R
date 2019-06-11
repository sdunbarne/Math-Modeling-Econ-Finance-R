mu <- 0.1682262
sigma <- 0.1722922
T <- 5.75
# length of the interval [0, T] in time units of years
S0 <- 8242.38

N <- 1448
# number of end-points of the grid including T
Delta <- T/N
# time increment,

t <- seq(0, T, length = N + 1)
W <- c(0, cumsum(sqrt(Delta) * rnorm(N)))  # Wiener process, 
GBM <- S0 * exp(mu * t + sigma * W)

plot(t, GBM, type = "l", xaxt = "n", ylab = "Simulated Wilshire 5000 Index")
axis(1, at = c(0.75, 1.75, 2.75, 3.75, 4.75, 5.75), label = c("2010", "2011", "2012", 
    "2013", "2014", "2015"))


## NAME: stockmarketmodel.R
##
## USAGE: within R, at interactive prompt
##        source("stockmarketmodel.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: Create a Geometric Brownian Motion with the same
## parameters as the Wilshire 5000 Index over the period April 1, 2009 to
## December 31, 2014
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: none
## DEPENDENCIES: none
## INCOMPATIBILITIES: none known
## PROVENANCE: created by sdunbar
## BUGS AND LIMITATIONS: none known
## FEATURES AND POTENTIAL IMPROVEMENTS:
##   1. Would be good to include the actual Wilshire 5000 data over the period
##   April 1, 2009, to December 31, 2015, but that would require a way to
##   incorporate the 1449 dates and corresponding data values.
##   2.  Profiling shows almost all time is spent in plotting.
## AUTHOR:  Steve Dunbar
## VERSION: 1.0 as of Wed Sep  9, 2015  7:31 AM
## KEYWORDS: Wiener process, Brownian Motion, Geometric Brownian
## Motion, relative growth rate, Wilshire 5000
