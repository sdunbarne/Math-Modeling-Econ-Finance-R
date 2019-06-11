    victory <- 10  # top boundary for random walk
    ruin <- -10    # bottom boundary for random walk

    victoryOrRuin <- array(0, dim = c(k))

    hitVictory <- apply( (totals >= victory), 2, which)
    hitRuin <- apply( (totals <= ruin), 2, which)

    for (j in 1:k) {
       if ( length(hitVictory[[j]]) == 0 && length(hitRuin[[j]]) == 0 ) {
         # no victory, no ruin
         # do nothing
       }
       else if ( length(hitVictory[[j]]) > 0 && length(hitRuin[[j]]) == 0 ) {
         # victory, no ruin
         victoryOrRuin[j] <- min(hitVictory[[j]])
	 }
       else if ( length(hitVictory[[j]]) == 0 && length(hitRuin[[j]]) > 0 ) {
         # no victory, ruin
         victoryOrRuin[j] <- -min(hitRuin[[j]])
         }
       else # ( length(hitVictory[[j]]) > 0 && length(hitRuin[[j]]) > 0 )
         # victory and ruin
	 if ( min(hitVictory[[j]]) < min(hitRuin[[j]]) ) { # victory first
           victoryOrRuin[j] <- min(hitVictory[[j]])  # code hitting victory
	 }
	 else {  # ruin first
           victoryOrRuin[j] <- -min(hitRuin[[j]]) # code hitting ruin as negative
	 }
    }

victoryBeforeRuin <- sum(0+(victoryOrRuin > 0))  # count exits through top
ruinBeforeVictory <- sum(0+(victoryOrRuin < 0))  # count exits through bottom
noRuinOrVictory   <- sum(0+(victoryOrRuin == 0))

cat(sprintf("Victories: %i  Ruins: %i No Ruin or Victory: %i \n", 
    victoryBeforeRuin, ruinBeforeVictory, noRuinOrVictory))

avgTimeVictoryOrRuin <- mean( abs(victoryOrRuin) )
varTimeVictoryOrRuin <- var( abs(victoryOrRuin) )

cat(sprintf("Average Time to Victory or Ruin: %f \n", 
    avgTimeVictoryOrRuin))
cat(sprintf("Variance of Time to Victory or Ruin: %f \n",
    varTimeVictoryOrRuin))

hist( victoryOrRuin, nclass = 2*max(abs(victoryOrRuin))+1 ) # sample exit time distribution

## NAME: victoryorruin.R
##
## USAGE: within R, at interactive prompt
##        source("victoryorruin.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: After experiment, find, count and display the hitting
##              times for victory (exit through top boundary) or ruin 
##             (exit through bottom boundary) 
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: none
## DEPENDENCIES: Depends on having run experiment.R first
## INCOMPATIBILITIES: none known
## PROVENANCE: Created Fri Feb 10, 2012  6:11 AM
## BUGS AND LIMITATIONS: None known
## FEATURES AND POTENTIAL IMPROVEMENTS: None at this time

## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 Fri Feb 10, 2012  6:11 AM
## KEYWORDS: Hitting times, coin flips, random walk
