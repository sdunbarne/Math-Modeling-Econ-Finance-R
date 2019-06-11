m <- 6;
n <- 61;
S0 <- 70;
S1 <- 130;
K <- 100;
r <- 0.12;
T <- 1.0;
sigma <- 0.10;

time <- seq(T,0, length=m);
S <- seq(S0,S1, length=n);

numerd1 <- outer( ( (r + sigma^2/2)*(T-time)), log(S/K), "+");
numerd2 <- outer( ( (r - sigma^2/2)*(T-time)), log(S/K), "+");

d1 <- numerd1/(sigma*sqrt(T-time));
d2 <- numerd2/(sigma*sqrt(T-time));
part1 <- t(t(pnorm(d1))*S);
part2 <- K*exp(-r*(T-time))*pnorm(d2);
VC <- part1 - part2;

matplot(S, t(VC), type = "l");

## NAME: solution.R
##
## USAGE: within R, at interactive prompt
##        source("solution.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION:  For given parameter values, the Black-Scholes-Merton
## solution formula is sampled at a specified m X 1 array of times and
## at a specified  1 X n array of security prices using vectorization
## and broadcasting.  The result can be plotted as functions of the
## security price as done in the text.  This approach is taken to
## illustrate the use of vectorization and broadcasting for efficient
## evaluation of an array of solution values from a complicated formula.
##              
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: none
## DEPENDENCIES:  none
## INCOMPATIBILITIES: none known
## PROVENANCE: Created by sdunbar
## BUGS AND LIMITATIONS: The definition of the function VC does only
## no error checking in the input variables.  This could be improved
## to do type checking and even value checking for appropriate values.
##
## The S variable is considered to be a row vector, of size 1 X n
## although R actually creates and uses it as a column vector of size n x 1.
## The t variable is considered to be a column vector, of size m X 1.
## The calculation of d1 and d2 then uses broadcasting, also called
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
## The calculation of the matrix part1 requires some analysis because it uses the
## recycling rule.  The matrix d1 is constructed as size 6 rows and 61 columns and
## pnorm() function applied to d1 preserves that size.  Then the transpose t() 
## is 61 rows by 6 columns.  The vector S has size 61 rows with 1 column.  Then
## Multiply t(pnorm(d1)) by S elementwise using the recylcing rule, going down the
## column.  This multiplication will recycle S 6 times, once for each column in
## t(pnorm(d1)).  Then take the transpose again to reshape the product to 6 by 61.
##
## The calculation of the matrix part2 requires some analysis because
## it uses the recycling rule.  The matrix d2 is constructed as size 6
## rows and 61 columns and pnorm() function applied to d2 preserves
## that size.  The vector K*exp(-r*(T-time)) has size 6 rows with 1 column.  Then
## Multiply K*exp(-r*(T-time)) with pnorm(d2) elementwise using the recylcing rule,
## going down the column.  This multiplication will recycle 61 times,
## once for each column in pnorm(d2). 
##
## To do the plotting. matplot plots the columns of one matrix against the 
## columns of another.  VC is 6 rows (one for each time element) by 61
## columns, and we wish to plot each row against S which is 1 column with 61 rows.
## Therefore, it is necessary to take the transpose of VC.
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0, as of Thu Oct 23, 2014  5:38 AM
## KEYWORDS: Black Scholes equation


