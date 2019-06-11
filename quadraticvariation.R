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

m1 <- N/5
partition1 <- seq(0,T,1/m1)
m2 <- N
partition2 <- seq(0,T,1/m2)
m3 <- 3*N
partition3 <- seq(0,T,1/m3)

qv1 <- sum( ( WcaretN( partition1[-1]) - WcaretN( partition1[-length(partition1)]))^2 )
qv2 <- sum( ( WcaretN( partition2[-1]) - WcaretN( partition2[-length(partition2)]))^2 )
qv3 <- sum( ( WcaretN( partition3[-1]) - WcaretN( partition3[-length(partition3)]))^2 )

cat(sprintf("Quadratic variation of approximation of  Wiener process paths with %d scaled random steps with %d partition intervals is: %f \n", N, m1, qv1 ))
cat(sprintf("Quadratic variation of approximation of  Wiener process paths with %d scaled random steps with %d partition intervals is: %f \n", N, m2, qv2 ))
cat(sprintf("Quadratic variation of approximation of  Wiener process paths with %d scaled random steps with %d partition intervals is: %f \n", N, m3, qv3 ))

## NAME: quadraticvariation.R
##
## USAGE: within R, at interactive prompt
##        source("quadraticvariation.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: 
##   Using a scaled random walk approximation of the Wiener process,
##   find the quadratic variation on 3 different partitions of [0,T],
##   the first a factor of N, the second N itself, the third a multiple of N.
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: none
## DEPENDENCIES: none
## INCOMPATIBILITIES: None known
## PROVENANCE: Created by sdunbar
## BUGS AND LIMITATIONS: none known
## FEATURES AND POTENTIAL IMPROVEMENTS:
##   The quadratic variation calculation takes advantage of the design of
##   WcaretN as vector-capable, that is, the input can be
##   a vector or array, and the output will be a vector of values of the 
##   approximation function at the corresponding points.
##
##   Because the quadratic variaiotn is computed using the scaled random
##   walk approximation of the Wiener process, the quadratic variation will be
##   1/k for a multiple k of N.
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 as of Wed Jul 31, 2013  5:38 AM
## KEYWORDS: random walk, Wiener process, Brownian motion, quadratic variation

