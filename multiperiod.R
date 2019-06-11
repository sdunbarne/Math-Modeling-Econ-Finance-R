S <- 100
factorUp <- 1.05
factorDown <- 0.95
B <- 1
effR <- 1.02
deltati <- 1
K <- 100

f <- function(x, strike) {
    # European call option
    max(x - strike, 0)
}

riskNeutralMeas <- function(fUp, fDown, exprdt) {
    # risk neutral measure pi
    (exprdt - fDown)/(fUp - fDown)
}

piRNM <- riskNeutralMeas(factorUp, factorDown, effR)

v11 <- (1/effR) * (piRNM * f(S * factorUp * factorUp, K) + (1 - piRNM) * f(S * factorUp * 
    factorDown, K))
v10 <- (1/effR) * (piRNM * f(S * factorUp * factorDown, K) + (1 - piRNM) * f(S * 
    factorDown * factorDown, K))

value <- (1/effR) * (piRNM * v11 + (1 - piRNM) * v10)

cat("value:", value, "\n")

## NAME: multiperiod.m
##
## USAGE: within R, at interactive prompt
##        source("multiperiod.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: Set up and solve for the 
## value of European call option in a
## two period binomial model.
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: R
## DEPENDENCIES:  none
## INCOMPATIBILITIES: none known
## PROVENANCE: version 1.0 by sdunbar as of Fri Dec 18, 2015  7:03 AM
## BUGS AND LIMITATIONS: A limitation is that all parameters must be
## entered directly into the script.  Another limitation is that the
## derivative value function is only for a European call option.
## FEATURES AND POTENTIAL IMPROVEMENTS:A limitation is that all parameters must be
## entered directly into the script.  Another limitation is that the
## derivative value function is only for a European call option.
## AUTHOR:  Steve Dunbar 
## VERSION: Version 1.0 as of Fri Dec 18, 2015  7:05 AM
## KEYWORDS: two period binomial model, derivative security


