mu <- 1
sigma <- 0.5
T <- 1
# length of the interval [0, T] in time units

trials <- 200
N <- 200
# number of end-points of the grid including T
Delta <- T/N
# time increment

t <- t( seq(0,T, length=N+1) * t(matrix(1, trials, N+1)) )
# Note use of the R matrix recycling rules, by columns, so transposes
W <- cbind(0, t( apply(sqrt(Delta) * matrix(rnorm(trials * N), trials, N), 1, cumsum)))
# Wiener process, Note the transpose after the apply, (side effect of
# apply is the result matches the length of individual calls to FUN,
# then the MARGIN dimension/s come next. So it's not so much
# "transposed" as that being a consequence of apply in 2D.)  Note
# use of recycling with cbind to start at 0

GBM <- exp( mu*t + sigma*W)

meanGBM <- colMeans(GBM)

meanGBM_rate <- lm(log(meanGBM) ~ seq(0,T, length=N+1))
predicted_mean_rate = mu + (1/2)*sigma^2

cat(sprintf("Observed meanGBM relative rate: %f \n", coefficients(meanGBM_rate)[2] ))
cat(sprintf("Predicted mean relative rate: %f \n", predicted_mean_rate))

## NAME: geometricbrownian.R
##
## USAGE: within R, at interactive prompt
##        source("geometricbrownian.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: create \verb+ trials + sample paths of Geometric
## Brownian Motion, sampled at \( N \) equally-spaced values on \(
##  [0,T]\).
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: none
## DEPENDENCIES: none
## INCOMPATIBILITIES: none known
## PROVENANCE: created by sdunbar
## BUGS AND LIMITATIONS: none known
## FEATURES AND POTENTIAL IMPROVEMENTS:
##  The function assumes the inputs and parameters are sensible.
##  Better input checking could catch errors before calculating.

## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 as of Thu Jul 10, 2014  6:24 AM
## KEYWORDS: Weiner process, Brownian Motion, Geometric Brownian
## Motion, relative growth rate
