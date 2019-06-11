
f <- function(sigma, S, K, r, Tminust, C) {
  d1 <- (log(S/K) + ( (r + sigma^2/2)*(Tminust)))/(sigma*sqrt(Tminust));
  d2 <- (log(S/K) + ( (r - sigma^2/2)*(Tminust)))/(sigma*sqrt(Tminust));
  part1 <- pnorm(d1) * S;
  part2 <- K*exp(-r*(Tminust)) * pnorm(d2);
  VC <- part1 - part2;
  f <- VC - C;
  f
}

S <- 21;
K <- 20;
Tminust <- 0.25;
r <- 0.10;
C <- 1.85;

sigmaNew <- 0.20;
epsilon <- 10^(-5);
maxIterations <- 1000

## Function to find Implied Volatility using Bisection Method
iv <-
function(S, K, r, Tminust, C){
  sig <- sigmaNew
  sig.up <- 1
  sig.down <- 0.001
  count <- 0
  err <- f(sigmaNew, S, K, r, Tminust, C)
 
  ## repeat until error is sufficiently small or counter hits 1000
  while(abs(err) > epsilon && count<maxIterations){
    if(err < 0){
      sig.down <- sigmaNew
      sigmaNew <- (sig.up + sigmaNew)/2
    }else{
      sig.up <- sigmaNew
      sigmaNew <- (sig.down + sigmaNew)/2
    }
    err <- f(sigmaNew, S, K, r, Tminust, C)
    count <- count + 1
  }
 
  ## return NA if counter hit maxIterations
  if(count==maxIterations){
    return(NA)
  }else{
    return(sigmaNew)
  }
}

print( iv(S, K, r, Tminust, C))

