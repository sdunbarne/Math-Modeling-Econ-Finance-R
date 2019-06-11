N <- 251
# number of end-points of the grid including T
T <- 2.51
# length of the interval [0, T] in time units
Delta <- T/N
# time increment
W <- numeric(N+1)
# initialization of the vector W approximating 
# Wiener process
t <- seq(0,T, length=N+1)
W <- c(18047.58, 18047.58+cumsum( sqrt(Delta) * 711.9648*rnorm(N)))
sim <- cbind(t,W)
write.table(sim, "djia_sim.dat", row.names=FALSE, col.names=FALSE)

## NAME: definition.R
##
## USAGE: within R, at interactive prompt
##        source("definition.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: Simulation of Wiener process using the
##              definition as independent increments having
##		normal distribution with variance sqrt(Delta)
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: none
## DEPENDENCIES:  none
## INCOMPATIBILITIES: none known
## PROVENANCE: Created by sdunbar, based on example 
##             ex1.06.R on page 19 in \emph{Simulation
##	       and Inference for Stochastic Differential
##	       Equations}, by Stefano Iacus, Springer, 2008
## BUGS AND LIMITATIONS: none known
## FEATURES AND POTENTIAL IMPROVEMENTS:  Note that the vertical axis limits
## are -1 and +1, so the probability is about 0.68 that the endpoint
## W(101) will be in the plot frame.  Plots may be truncated because of
## the choice of plot frame.  This is intentional to illustrate an
## aspect of the Wiener process.
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0  as of Fri Mar  1, 2013  5:43 AM
## KEYWORDS: Wiener process, Brownian motion

