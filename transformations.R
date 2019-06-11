p <- 0.5
N <- 400

T <- 2
h <- 0.25
c <- 2.0

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

Wshift <- function(x) {
    retval <- WcaretN(x+h) - WcaretN(h)
}

Wscale <- function(x) {
    retval <- c*WcaretN(x/c^2)
}

curve(WcaretN, 0,1, n=400, col = "black")
curve(Wshift, 0,1, n=400, add = TRUE, col = "blue")
curve(Wscale, 0,1, n= 400, add = TRUE, col = "red")
 
## NAME: transformations.R
##
## USAGE: within R, at interactive prompt
##        source("transformations.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: Transformations of Wiener process using the
##              scaling approximation of random walk
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: none
## DEPENDENCIES:  none
## INCOMPATIBILITIES: none known
## PROVENANCE: Created by sdunbar, 
## BUGS AND LIMITATIONS: none known
## FEATURES AND POTENTIAL IMPROVEMENTS:
##  
##  The base approximation function assumes the inputs are in the interval [0,T]
##  The transformed functions assume the inputs are compatible with the base
##  approximation function.
##  Better input checking could catch errors before plotting.
##
##   The function WcaretN is vector-capable, that is, the input can be
##   a vector or array, and the output will be a vector of values of the 
##   approximation function at the corresponding points.
##   The transformed functions are correspondingly vactor-capable.
##
##   A feature of this N-step random walk scaling approximation program
##   is that it creates the approximation and the transformations as functions
##   on [0,T].  All
##   functions can be plotted with any number of points on the interval [0,T].
##   If the time grid on [0,T] is less than N points, then some of the
##   information in the N-step scaling approximation is ignored, and the
##   plotted function will be less representative of the approximation
##   than it could be.  If the time grid on [0,T] is is greater than N
##   points, then the plotted function will just represent the linear
##   interpolation between the step points at j*T/N and no new
##   information is represented.  
##
##  Depending on the internal plotting routines used by the language,
##  plotting the approximation and transformed functions
##  can result in
##  plot artifacts.  One simple artifact may be horizonal segments in the
##  points. If the plotting algorithms attempt to use adaptive point
##  selection to densely position a greater portion of a fixed number of
##  plotting points in a region of rapid variation, then other regions
##  will have fewer plotting points.  Those regions with fewer plotting
##  points will miss some of the information in that region.  Depending on
##  the language, the plotting routine may use smoothing or some other
##  nonlinear interpolation between plotting points which will result in
##  curved segments instead of a piecewise linear function.  If the
##  intention is to plot an transformed Brownian Motion, then there are
##  more direct and efficient ways to create and plot the \( N+1 \)
##  coordinate pairs \( (jT/N, \sqrt{T/N} S_{j}) \) defining the vertices of the
##  piecewise linear scaled random walk approximation with an appropriate amount of
##  information.  Here the intention is to first to demonstrate the
##  creation of the approximation function as a piecewise linear function,
##  then second to use the function to plot a graph.
##
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0  as of Wed May 15, 2013  5:55 AM
## KEYWORDS: random walk, Wiener process, Brownian motion, transformation

