r <- -1                                 # growth/decay rate
sigma <- 0.5                            # relative standard deviation
b <- 3                                  # initial condition

M <- 100                                # number of steps for EM method to take
T <- 1                                  # maximum time
h <- T/M                                # time step
t <- seq(length=M+1, from=0, by= h)     # t is the vector [0 1h 2h 3h ... Nh]
X <- array(0, c(M+1))                   # place to store locations

N <- 30*(M+1)                           # number of steps for the Brownian Motion approx

p <- 0.5
S <- array(0, c(N+1))
rw <- cumsum( 2 *(runif(N) <= p) -1 )
S[2:(N+1)] <- rw

WcaretN <- function(z) {
    Delta <- T/N

    # add 1 since arrays are 1-based
    prior = floor(z/Delta) + 1
    subsequent = ceiling(z/Delta) + 1

    retval <- sqrt(Delta)*(S[prior] + ((z/Delta+1) - prior)*(S[subsequent] - S[prior]))
}

X[1] <- b
for (i in 1:M) {
   X[i+1] <- X[i]+r*X[i]*h+sigma*X[i]*(WcaretN(t[i]+h)-WcaretN(t[i]))
 }

plot(t,X,"l", xlim=c(0, T), ylim=c(X[1]-exp(abs(r)*T+1), X[1]+exp(abs(r)*T+1)))
title(main=paste("r = ", r, "sigma = ", sigma, "steps =", M))

## NAME: stochasticdes.R
##
## USAGE: within R, at interactive prompt
##        source("stochasticdes.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: Simulation of solution of linear stochastic
## differential equations using the Euler-Maruyama Method
##              
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: none
## DEPENDENCIES:  none
## INCOMPATIBILITIES: none known
## PROVENANCE: Created by S Dunbar Wed May 21, 2014  6:00 AM
## BUGS AND LIMITATIONS: none known
## FEATURES AND POTENTIAL IMPROVEMENTS:
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 as of Wed May 21, 2014  6:00 AM
## KEYWORDS: stochastic differential equations, Euler-Maruyama method, Wiener process

