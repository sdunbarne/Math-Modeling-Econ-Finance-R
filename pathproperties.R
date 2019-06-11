p <- 0.5
N <- 1000

T <- 1

S <- array(0, c(N+1))
rw <- cumsum( 2 * ( runif(N) <= p)-1 )
S[2:(N+1)] <- rw

WcaretN <- function(x) {
    Delta <- T/N

    # add 1 since arrays are 1-based
    prior = floor(x/Delta) + 1
    subsequent = ceiling(x/Delta) + 1

    retval <- sqrt(Delta)*(S[prior] + ((x/Delta+1) - prior)*(S[subsequent] - S[prior]))
}

h0 <- 1e-7
h1 <- 1e-2
m = 30
basepoint = 0.5

h <- seq(h0, h1, length=m)
x0 <- array(basepoint, c(m))

diffquotients <- abs(WcaretN( x0 + h) - WcaretN(x0) )/h

plot(h, diffquotients, type = "l", log = "y", xlab = expression(h),
	    ylab = expression(abs(W(x0+h) - W(x0))/h))
max(diffquotients, na.rm=TRUE)

## NAME: pathproperties.R
##
## USAGE: within R, at interactive prompt
##        source("pathproperties.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: 
##   Using a scaled random walk approximation of the Wiener process,
##   find the difference quotients on an array of differences based at
##   single point, then plot the absolute values of the difference quotients
##   to illustrate the non-existence of the derivative.
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: requires matplotlib
## DEPENDENCIES: requires matplotlib
## INCOMPATIBILITIES: None known
## PROVENANCE: Created by sdunbar, very loosely based on example 
##             ex1.07.R on page 20-21 in \emph{Simulation
##	       and Inference for Stochastic Differential
##	       Equations}, by Stefano Iacus, Springer, 2008
## BUGS AND LIMITATIONS: none known
## FEATURES AND POTENTIAL IMPROVEMENTS:
##   The difference quotients take advantage of the design of
##   WcaretN as vector-capable, that is, the input can be
##   a vector or array, and the output will be a vector of values of the 
##   approximation function at the corresponding points.
##
##   Because the difference quotients are computed using the scaled random
##   walk approximation of the Wiener process, the largest possible slope is
##   sqrt(T/N)/(T/N) = sqrt(N/T).  So the plotted difference quotients will 
##   max out once the increment is less than the scaled random walk step size.
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 as of Fri Jul  5, 2013  6:10 AM
##          Version 1.1 as of Fri Mar 11, 2016  7:39 AM
## KEYWORDS: random walk, Wiener process, Brownian motion, path properties,
##           derivative

