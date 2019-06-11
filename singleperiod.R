S <- 50;
up <- .10;
down <- 0.03;
B <- 1;
r <- 0.06;
T <- 1;
K <- 50;

f <- function(x,strike) {
  max(x-strike,0);
}

m <- rbind( c(S*(1-down), B*exp(r*T)),  c(S*(1+up), B*exp(r*T)));
payoff <- c(f(S*(1-down), K), f(S*(1+up), K));

portfolio <- solve(m,payoff);
value <- portfolio %*% c(S,B);

cat("portfolio: phi=", portfolio[1], "psi=", portfolio[2],"\n");
cat("value = ", value, "\n");

## NAME: singleperiod.R
##
## USAGE: within R, at interactive prompt
##        source("singleperiod.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: Set up and solve for the replicating portfolio
## and the value of the corresponding derivative security in a
## single period binomial model.
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT:  R
## DEPENDENCIES: none
## INCOMPATIBILITIES: none known
## PROVENANCE: version 1.0 by sdunbar as of Thu Dec 10, 2015  8:58 AM
## BUGS AND LIMITATIONS: A limitation is that all parameters must be
## entered directly into the script.
## FEATURES AND POTENTIAL IMPROVEMENTS:  Make the script interactive so
## that parameters can be entered from a command line, perhaps as a function,
## or through a GUI.
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 as of Thu Dec 10, 2015  9:00 AM
## KEYWORDS:  single period biomial model, derivative security, replicating portfolio
