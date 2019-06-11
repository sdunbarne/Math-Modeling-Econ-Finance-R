mydata <- read.csv("table.csv")
closingValue <- mydata$Close
## These two lines are particular to the data file and its format from finance.yahoo.com

closingValue <- rev(closingValue)
logClosingValue <- log(closingValue)
logChanges <- diff(logClosingValue)
zscoreChanges <- (logChanges - mean(logChanges))/sd(logChanges)

m <- ceiling(max( c(abs( max(zscoreChanges)) , abs( min(zscoreChanges))) ));

test <- hist(zscoreChanges, -m:m, plot=FALSE)

expectz <- diff( c( 0, pnorm( (-m+1):(m-1)), 1) )

ratios <- test$density/expectz
print(ratios)

## NAME: revisedtestingfattails.R
##  script to test financial data for the presence of fat tails
## USAGE: within R, at interactive prompt
##        source("testingfattails.R")
## REQUIRED ARGUMENTS: 
##   The script is applied to a datafile obtained from finance.yahoo.com
##   Here the datafile is called table.csv with a column called Close
## OPTIONS:
##   None
## DESCRIPTION: 
##   Takes the closing value of financial data from a file, then
##   puts the values in chronological order, takes the logarithm and finds the daily log-change.
##   The changes are normalized to a zscore by subtracting the mean and
##   dividing by the standard deviation.  Then the normalized scores are
##   summarized in with the histogram command.  The density summary from
##   histogram is then comapred to probabilities computed from the normal
##   cdf.
## DIAGNOSTICS: None
## CONFIGURATION AND ENVIRONMENT: Needs a financial data file in csv format
## DEPENDENCIES:  Needs a financial data file in csv format
## INCOMPATIBILITIES: None known
## PROVENANCE:  Steven R. Dunbar Thu Jun 28 15:25:27 CDT 2018
## BUGS AND LIMITATIONS: None known.
## FEATURES AND POTENTIAL IMPROVEMENTS:
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0, Fri Mar 20, 2015  5:46 AM
##          Version 1.1  Thu Jun 28 15:26:02 CDT 2018
## KEYWORDS: fat tails, normal data, histogram

