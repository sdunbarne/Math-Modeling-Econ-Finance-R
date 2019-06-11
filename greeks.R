m <- 6
n <- 61
S0 <- 70
S1 <- 130
K <- 100
r <- 0.12
T <- 1
sigma <- 0.1

time <- seq(T, 0, length = m)
S <- seq(S0, S1, length = n)

numerd1 <- outer(((r + sigma^2/2) * (T - time)), log(S/K), "+")
d1 <- numerd1/(sigma * sqrt(T - time))
Delta <- pnorm(d1)

factor1 <- 1/(sqrt(2 * pi) * sigma * outer(sqrt(T - time), S, "*"))
factor2 <- exp(-d1^2/2)
Gamma <- factor1 * factor2

old.par <- par(mfrow = c(1, 2))
matplot(S, t(Delta), type = "l")
matplot(S, t(Gamma), type = "l")
par(old.par)

## NAME: greeks.R
##
## USAGE: within R, at interactive prompt
##        source("greeks.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION:  For given parameter values, the Black-Scholes-Merton
## call option "Greeks" Delta and Gamma are
## sampled at a specified m X 1 array of times and
## at a specified  1 X n array of security prices using vectorization
## and broadcasting.  The results can be plotted as functions of the
## security price.  This approach is taken to
## illustrate the use of vectorization and broadcasting for efficient
## evaluation of greeks for an array of solution values from
## a complicated formula.
##              
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: none
## DEPENDENCIES:  none
## INCOMPATIBILITIES: none known
## PROVENANCE: Created by sdunbar
## BUGS AND LIMITATIONS: The definition of the functions
## Delta, Gamma does no
## no error checking in the input variables.  This could be improved
## to do type checking and even value checking for appropriate values.
##
## The S variable is considered to be a row vector, of size 1 X n
## although R actually creates and uses it as a column vector of size n x 1.
## The t variable is considered to be a column vector, of size m X 1.
## The calculation of Delta and factor1  then uses broadcasting, also called
## binary singleton expansion, recycling, single-instruction multiple
## data or replication.
##
## The calculation is vectorized for an array of S values and an array
## of t values, but it is NOT vectorized for arrays in the parameters
## K, r, T, and sigma.
##
## The calculation relies on using the rules for calcuation and handling of
## infinity and NaN (Not a Number) which come from divisions by 0, taking
## logarithms of 0, and negative numbers and calculating the normal cdf at
## ininifty and negative infinity.  R will not plot a NaN which accounts for
## the gap at S = 0 in the graph line for t = 1.
##
## The color labeling of the plot lines is the default coloring.
## There doesn't seem to be an easy way to get the color labeling
## described in the text.
## FEATURES AND POTENTIAL IMPROVEMENTS:
## Add a plot key labeling the plot lines.
##
##
## To do the plotting. matplot plots the columns of one matrix against the 
## columns of another.  Delta and Gamma are 6 rows (one for each time element) by 61
## columns, and we wish to plot each row against S which is 1 column with 61 rows.
## Therefore, it is necessary to take the transpose of Delta and Gamma.
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0, as of Tue Dec 30, 2014  7:25 AM
## KEYWORDS: Black Scholes equation, Delta, Gamma, greeks


