
f <- function(sigma, S, K, r, Tminust, C) {
  d1 <- (log(S/K) + ( (r + sigma^2/2)*(Tminust)))/(sigma*sqrt(Tminust));
  d2 <- (log(S/K) + ( (r - sigma^2/2)*(Tminust)))/(sigma*sqrt(Tminust));
  part1 <- pnorm(d1) * S;
  part2 <- K*exp(-r*(Tminust)) * pnorm(d2);
  VC <- part1 - part2;
  f <- VC - C;
  f
}

fprime <- function(sigma, S, K, r, Tminust, C) {
  d1 <- (log(S/K) + ( (r + sigma^2/2)*(Tminust)))/(sigma*sqrt(Tminust));
  fprime <- S*sqrt(Tminust)*dnorm(d1)*exp(-r*Tminust);
  fprime
}

S <- 21;
K <- 20;
Tminust <- 0.25;
r <- 0.10;
C <- 1.85;

sigmaNew <- 0.20;
epsilon <- 10^(-5);

repeat {
    sigmaOld <- sigmaNew
    sigmaNew <- sigmaOld - f(sigmaOld, S, K, r, Tminust, C)/fprime(sigmaOld, S, K, 
        r, Tminust)
    if (abs(sigmaNew - sigmaOld) < epsilon) {
        break
    }
}

cat(sprintf("%f\n", sigmaNew));

## NAME: impliedvolatility.R
##
## USAGE: within R at interactive prompt
##        source("impliedvolatility.R")
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: 
## For given numerical values for \( \sigma_0 \), the guess for
## the volatility; \( S \), the currect security price; \( K \),
## the strike price; \( r \), the risk-free interest rate; \( T -
## t \), the time to expiration; and \( C \), the current call
## option price, the script uses Newton's method to find the implied
## volatility with error tolerance \( \epsilon \).
##
## DIAGNOSTICS:  none
## CONFIGURATION AND ENVIRONMENT: none
## DEPENDENCIES: none
## INCOMPATIBILITIES: none known
## PROVENANCE:   Created by sdunbar
## BUGS AND LIMITATIONS:
## Uses the repeat{ ... if () { break } } construction to simulate an
## do ... until type loop.
## FEATURES AND POTENTIAL IMPROVEMENTS:
##
## Modify the scripts for implied volatility to be a function which
## takes the numerical values for \( \sigma_0 \), the guess for
## the volatility; \( S \), the currect security price; \( K \),
## the strike price; \( r \), the risk-free interest rate; \( T -
## t \), the time to expiration; and \( C \), the current call
## option price.
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0, as of Sat Dec 13, 2014  7:06 AM
## KEYWORDS: Black Scholes equation, implied volatility, Newton's method






