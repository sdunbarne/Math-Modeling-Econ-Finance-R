p <- 0.5
n <- 150
k <- 60

victory <- 10
# top boundary for random walk
ruin <- -10
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

probRuinBeforeVictory <- 
     apply( (hitRuin < hitVictory), 2, 
	 (function(x)length((which(x,arr.ind=FALSE)))) )/k

startValues <- (ruin:victory);
ruinFunction <- lm(probRuinBeforeVictory ~ startValues)
# lm is the R function for linear models, a more general view of
# least squares linear fitting for response ~ terms
cat(sprintf("Ruin function Intercept: %f \n", coefficients(ruinFunction)[1] ))
cat(sprintf("Ruin function Slope: %f \n", coefficients(ruinFunction)[2] ))

plot(startValues, probRuinBeforeVictory);
abline(ruinFunction)

## NAME: ruinprobs.R 
##
## USAGE: within R, at interactive prompt
##        source("ruinprobs.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION:  Experiment of flipping a coin until ruin or victory
##              multiple times to measure experimental probability
##              
## DIAGNOSTICS: None
## CONFIGURATION AND ENVIRONMENT: None
## DEPENDENCIES: None
## INCOMPATIBILITIES: None known
## PROVENANCE: Created Mon Apr 30, 2012  5:54 AM
## BUGS AND LIMITATIONS: None known
## FEATURES AND POTENTIAL IMPROVEMENTS: None known at this time

## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 Mon Apr 30, 2012  5:54 AM
## KEYWORDS:Coinflips. binomial random variable, random walk, ruin
## probability, absorbing Markov chain


