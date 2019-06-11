> mydata <- read.csv("table.csv")
> closingValue <- mydata$Close
> closingValue <- rev(closingValue)
> changes <- diff(closingValue)
> logclosingValue <- log(closingValue)
> changeslogclosing <- diff(logclosingValue)
> dim(changeslogclosing)
NULL
> NROW(logclosingValue)
[1] 1449
> NROW(changeslogclosing)
[1] 1448
> zscorelogChanges <- (changeslogclosing - mean(changeslogclosing))/sd(changeslogclsoing)                
Error in is.data.frame(x) : object 'changeslogclsoing' not found                                         
> zscorelogChanges <- (changeslogclosing - mean(changeslogclosing))/sd(changeslogclosing)                
> NROW(zscorelogChanges)
[1] 1448                                                                                                 
> m <- ceiling(max( c(abs( max(zscorelogChanges)) , abs( min(zscorelogChanges))) ));                     
> test <- hist(zscorelogChanges, -m:m, plot=FALSE)                                                       
> test                                                                                                   
$breaks                                                                                                  
 [1] -7 -6 -5 -4 -3 -2 -1  0  1  2  3  4  5  6  7                                                        
                                                                                                         
$counts                                                                                                  
 [1]   1   0   4   9  39 127 513 592 127  26   7   3   0   0

$density
 [1] 0.0006906077 0.0000000000 0.0027624309 0.0062154696 0.0269337017
 [6] 0.0877071823 0.3542817680 0.4088397790 0.0877071823 0.0179558011
[11] 0.0048342541 0.0020718232 0.0000000000 0.0000000000

$mids
 [1] -6.5 -5.5 -4.5 -3.5 -2.5 -1.5 -0.5  0.5  1.5  2.5  3.5  4.5  5.5  6.5

$xname
[1] "zscorelogChanges"

$equidist
[1] TRUE
> qqnorm(zscorelogChanges)
> qqline(zscorelogChanges)
 shapiro.test(zscorelogChanges)

        Shapiro-Wilk normality test

data:  zscorelogChanges
W = 0.94925, p-value < 2.2e-16

