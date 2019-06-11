
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
iv <- 0.23;
C <- 1.85;

rNew <- 0.20;
epsilon <- 10^(-5);
maxIterations <- 1000

## Function to find the risk-free rate using Bisection Method
riskfree <-
function(S, K, Tminust, iv, C){
  r <- rNew
  r.up <- 1.0
  r.down <- 0.0
  count <- 0
  err <- f(iv, S, K, rNew, Tminust, C)
 
  ## repeat until error is sufficiently small or counter hits 1000
  while(abs(err) > epsilon && count<maxIterations){
    if(err < 0){
      r.down <- rNew
      rNew <- (r.up + rNew)/2
    }else{
      r.up <- rNew
      rNew <- (r.down + rNew)/2
    }
    err <- f(iv, S, K, rNew, Tminust, C)
    count <- count + 1
  }
 
  ## return NA if counter hit maxIterations
  if(count==maxIterations){
    return(NA)
  }else{
    return(rNew)
  }
}

print( riskfree(S, K, Tminust, iv, C))

