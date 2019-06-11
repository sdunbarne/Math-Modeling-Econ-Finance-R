n <- 10000
z0 <- 21670                             #Willshire 5000 close on 2009-04-01
r  <- 9.273122  # mean of daily chainges in Wilshire 5000, 2009-04-01, 20014-12-31
sigma <- 144.7885 # stdev of daily chainges in Wilshire 5000, 2009-04-01, 2014-12-31
K <- 1

x <- rnorm(n)
y <- sapply(x, function(x) max(0, K-exp(sigma*x)))

## the true value
numerd1 <- log(S/K) + (r + sigma^2/2)*(T-time);
numerd2 <- log(S/K) + (r - sigma^2/2)*(T-time);
d1 <- numerd1/(sigma*sqrt(T-time));
d2 <- numerd2/(sigma*sqrt(T-time));
part1 <- S*(pnorm(d1))-1);
part2 <- K*exp(-r*(T-time))*(pnorm(d2)-1);
VP <- part1 - part2;

t.test(y[1:100])             # first 100 simulations

t.test(y[1:1000])            # first 1000 simulations

t.test(y)             # all simulation results

## NAME: 
##
## USAGE: within R, at interactive prompt
##        source(" .R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: 
## DESCRIPTION: The purpose of this script is to calculate the value of a put option
## on the Wishire 5000 stock index.  This is done using parameters calculated from
## historical data on the Wilshire 5000 index from 2009-04-01 to 2014-12-31.  First
## the script uses the Black Scholes formula for a put option to calculate the
## theoretical value for a put option.  Then the script uses Monte Carlo
## simulation based on the Geometric Brownian Motion with the Wilshire 5000
## parameters to find an estimate for the put option value.  Finally, the script
## Monte Carlo simulation with antithetic sampling and importance sampling
## variance reduction methods to refine the estiamtes.  The ultimate purpose
## is to compare the various methods with the actual put option value.
##              
## DIAGNOSTICS:
## CONFIGURATION AND ENVIRONMENT:
## DEPENDENCIES: 
## INCOMPATIBILITIES:
## PROVENANCE:
## BUGS AND LIMITATIONS:
## FEATURES AND POTENTIAL IMPROVEMENTS:

## AUTHOR:  Steve Dunbar
## VERSION: Version 
## KEYWORDS:

