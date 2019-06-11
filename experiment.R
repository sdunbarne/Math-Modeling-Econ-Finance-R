    p <- 0.5
    n <- 100
    k <- 30
    coinFlips <- array( 0+(runif(n*k) <= p), dim=c(n,k)) 
         # 0+ coerces Boolean to numeric
    headsTotal <- colSums(coinFlips)  	# 0..n binomial rv sample, size k
    muHeads <- mean(headsTotal)         # Expected value is n/2
    sigmaSquaredHeads <- var(headsTotal)    # Theoretical value is np(1-p)
    cat(sprintf("Empirical Mean of Heads: %f \n", muHeads ))
    cat(sprintf("Empirical Variance of Heads: %f \n",  sigmaSquaredHeads ))

    winLose <- 2*coinFlips - 1          # -1 for Tails, 1 for Heads
    totals <- apply( winLose, 2, cumsum) 
    	 # -n..n (every other integer) binomial rv sample
         # the second argument ``2'' means column-wise 
    muWinLose <- mean( totals[n,])    # Expected value is 0
    sigmaSquaredWinLose <- var( totals[n,])  # Theoretical value is 4np(1-p)
    cat(sprintf("Empirical Mean of Wins minus Losses: %f \n", muWinLose ))
    cat(sprintf("Empirical Variance of Wins minus Losses: %f \n",  sigmaSquaredWinLose ))

## NAME: experiment.R
##
## USAGE: within R, at interactive prompt
##        source("experiment.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: Experiment of flipping a coin n times, 
##              and repeat the experiment k tiems.  
## DIAGNOSTICS: None
## CONFIGURATION AND ENVIRONMENT: None
## DEPENDENCIES: None
## INCOMPATIBILITIES:  None known
## PROVENANCE:  Created Fri Feb 10, 2012  6:05 AM by SRD
## BUGS AND LIMITATIONS:  None known
## FEATURES AND POTENTIAL IMPROVEMENTS:  None at this time

## AUTHOR:  Steve Dunbar
## VERSION: Version 1.2 Tue Oct 30, 2012  5:55 AM
##          fixed printing of muHeads, sigmaSquaredHead
##	    added formatted printing of results
## KEYWORDS: Coin flips, binomial random variable.

