mu <- 0.1682262
sigma <- 0.1722922
T <- 5.75
# length of the interval [0, T] in time units of years
S0 <- 8242.38

N <- 1448
# number of end-points of the grid including T
Delta <- T/N
# time increment, 

t <- seq(0,T, length=N+1)
# Note use of the R matrix recycling rules, by columns, so transposes
W <- c(0, cumsum( sqrt(Delta) * rnorm(N)))
# Wiener process, Note the transpose after the apply, (side effect of
# apply is the result matches the length of individual calls to FUN,
# then the MARGIN dimension/s come next. So it's not so much
# "transposed" as that being a consequence of apply in 2D.)  Note
# use of recycling with cbind to start at 0

GBM <- S0*exp( mu*t + sigma*W)

wil5000Data <- read.csv("table.csv", stringsAsFactors=FALSE)
closingValue <- wil5000Data$Close
closingValue <- rev(closingValue)
Time <- as.Date(rev(wil5000Data$Date))
y = cbind(t, closingValue, GBM)

# matplot(Time, y, type="l")
matplot(Time, y, type ="l", lty = 1, xaxt="n", ylab="Wilshire 5000 Index")
axis.Date(1, Time)

write.table(y, "t_w5000_gbm.dat", row.names=FALSE, col.names=FALSE)

## NAME: 
##
## USAGE: within R, at interactive prompt
##        source(".R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: 
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: none
## DEPENDENCIES: none
## INCOMPATIBILITIES: none known
## PROVENANCE: created by sdunbar
## BUGS AND LIMITATIONS: none known
## FEATURES AND POTENTIAL IMPROVEMENTS:


## AUTHOR:  Steve Dunbar
## VERSION: 
## KEYWORDS: Wiener process, Brownian Motion, Geometric Brownian
## Motion, relative growth rate
