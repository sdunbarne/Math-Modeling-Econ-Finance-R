p <- 0.5
n <- 300
k <- 200

victory <- 10
# top boundary for random walk
ruin <- 0
# bottom boundary for random walk
interval <- victory - ruin + 1

winLose <- 2 * (array( 0+(runif(n*k*interval) <= p), dim=c(n,k,
interval))) - 1
# 0+ coerces Boolean to numeric
totals <- apply( winLose, 2:3, cumsum)
# the second argument ``2:3'' means column-wise in each panel
start <- outer( array(1, dim=c(n+1,k)), ruin:victory, "*")

paths <- array( 0 , dim=c(n+1, k, interval) )
paths[2:(n+1), 1:k, 1:interval] <- totals    
paths <- paths + start

hitVictory <- apply(paths, 2:3, (function(x)match(victory,x, nomatch=n+2)));
hitRuin    <- apply(paths, 2:3, (function(x)match(ruin,   x, nomatch=n+2)));
# the second argument ``2:3'' means column-wise in each panel
# If no ruin or victory on a walk, nomatch=n+2 sets the hitting
# time to be two more than the number of steps, one more than
# the column length.  Without the nomatch option, get NA which
# works poorly with the comparison hitRuin < hitVictory next.

duration <- pmin(hitVictory, hitRuin) - 1
# Subtract 1 since R arrays are 1-based, so duration is 1 less than index
is.na(duration) = duration > n
# Remove durations greater than length of trials
meanDuration = colMeans( duration, na.rm=TRUE)

startValues <- (ruin:victory);
durationFunction <- lm( meanDuration ~ poly(startValues,2,raw=TRUE) )
# lm is the R function for linear models, a more general view of
# least squares linear fitting for response ~ terms

plot(startValues, meanDuration, col = "blue");
lines(startValues, predict(durationFunction, data=startValues), col = "red")

cat(sprintf("Duration function is: %f  + %f x + %f x^2 \n", 
     coefficients(durationFunction)[1], coefficients(durationFunction)[2],
     coefficients(durationFunction)[3] ))

## NAME: duration.R 
##
## USAGE: within R, at interactive prompt
##        source("duration.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION:  Experiment of flipping a coin until ruin or victory
##              multiple times to measure experimental duration
##              
## DIAGNOSTICS: None
## CONFIGURATION AND ENVIRONMENT: None
## DEPENDENCIES: None
## INCOMPATIBILITIES: None known
## PROVENANCE: Created Thu Jul 19, 2012  6:20 AM
## BUGS AND LIMITATIONS: None known
## FEATURES AND POTENTIAL IMPROVEMENTS: None known at this timex

## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 Thu Jul 19, 2012  6:21 AM
##                  1.1.Thu Nov  8, 2012  5:46 AM adding output formatting
## KEYWORDS:Coinflips. binomial random variable, random walk, ruin
## probability, duration, absorbing Markov chain


