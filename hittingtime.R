T <- 10
a <- 1
time <- 2

p <- 0.5
n <- 10000
k <- 1000

Delta = T/n

winLose <- 2 * (array( 0+(runif(n*k) <= p), dim=c(n,k))) - 1
# 0+ coerces Boolean to numeric
totals <- apply( winLose, 2, cumsum)

paths <- array( 0 , dim=c(n+1, k) )
paths[2:(n+1), 1:k] <- sqrt(Delta)*totals    

hitIndex <- apply( 0+(paths <= a), 2, (function(x) match(0, x, nomatch=n+2)))
# If no hiting on a walk, nomatch=n+2 sets the hitting
# time to be two more than the number of steps, one more than
# the column length.  Without the nomatch option, get NA which
# works poorly with the comparison

hittingTime = Delta*(hitIndex-1)
## subtract 1 since vectors are 1-based

probHitlessTa <- sum( 0+(hittingTime <= time))/k
probMax = sum( 0+( apply(paths[1:((time/Delta)+1),], 2, max) >= a ) )/k
theoreticalProb = 2*pnorm(a/sqrt(time), lower=FALSE)

cat(sprintf("Empirical probability Wiener process paths hit %f before %f: %f \n", a, time, probHitlessTa ))
cat(sprintf("Empirical probability Wiener process paths greater than %f before %f: %f \n", a, time, probMax ))
cat(sprintf("Theoretical probability: %f \n", theoreticalProb ))

## NAME: hittingtime.R
##
## USAGE: within R, at interactive prompt
##        source("hittingtime.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: Experiment of approximating Wiener process over [0,T]
##              k times, finding the time of hitting a, then computing
##              the fraction out of k with hitting time less or equal to
##              t.  Also compute the maximum over [0,t], compute
##              fraction out of k with maximum greater or equal to a.
##
## DIAGNOSTICS: None
## CONFIGURATION AND ENVIRONMENT: None
## DEPENDENCIES: None
## INCOMPATIBILITIES: None known
## PROVENANCE: Created Thu May 30, 2013  6:04 AM
## BUGS AND LIMITATIONS: None known
## FEATURES AND POTENTIAL IMPROVEMENTS: 
## Need better formating for the output.
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 Thu May 30, 2013  6:04 AM
## KEYWORDS: Wiener process, Brownian Motion, hitting time,
##           distribtion of maximum
## PROFILE:
## $by.self
##                 self.time self.pct total.time total.pct
## "match"              0.58    23.77       0.58     23.77
## "apply"              0.36    14.75       1.44     59.02
## "runif"              0.28    11.48       0.28     11.48
## "+"                  0.26    10.66       0.26     10.66
## "array"              0.22     9.02       0.72     29.51
## "aperm.default"      0.18     7.38       0.18      7.38
## "<="                 0.16     6.56       0.16      6.56
## "eval"               0.14     5.74       2.44    100.00
## "*"                  0.14     5.74       0.14      5.74
## "-"                  0.06     2.46       0.06      2.46
## "unlist"             0.06     2.46       0.06      2.46
## 