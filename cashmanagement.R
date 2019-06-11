n <- 10   # Top boundary, number of states 0..n is n+1
s <- 5   # Start and Reset state number 1 <= s <= n-1
p <- 1/2
steps <- 1000
rate <- 1.0
K <- 2   # Charge or cost to reset the Markov chain

diag.num <- outer(seq(1,n+1),seq(1,n+1), "-")
# diag.num is a matrix whose ith lower diagonal equals i, opposite for upper diagonal
T <- mat.or.vec(n+1,n+1)
# mat.or.vec creates an nr by nc zero matrix if nc is greater than 1
# Also remember that matrices in R are 1-based, so need n+1 states, 0..n
T[diag.num == -1] <- p
T[diag.num == 1] <- 1-p
T[1,2] <- 0; T[1,s+1] <- 1;
T[n+1,n] <- 0; T[n+1,s+1] <- 1;

# vector to hold the count of visits to each state during a cycle
count <- mat.or.vec(1, n+1)
# Initialize the number of cycles
numberCycles <- 0
# Initialize the total cost of the cashmanagement
totalCost <- 0
# Start in the state s
state = s+1

# Make steps through the markov chain
for (i in 1:steps)
{
    x = 0;
    u = runif(1, 0, 1);

    newState = state;
    for (j in 1:ncol(T))
    {
	x = x + T[state, j];
	if (x >= u)
	{
	    newState = j;
	    break;
	}
    }
    ## newState <- sample(1:ncol(T), 1, prob=T[state,])
    state = newState;
    count[state] <- count[state] + 1
    
    if (state == n+1 || state == 1) {
        numberCycles <- numberCycles + 1
        totalCost <- K + totalCost
    } else {
        totalCost <- rate*(state-1) + totalCost
    }	
}

avgCost <- totalCost/steps
theoreticalAvgCost <- ( K + (1/3)*(rate*s*(n^2 - s^2)) )/( s*(n-s) )

cat(sprintf("Average cost: \n "))
cat(avgCost)
cat("\n\n")
cat(sprintf("Theoretical average cost: \n "))
cat(theoreticalAvgCost)
cat("\n")

## NAME: cashmanagement.R
##
## USAGE: within R, at interactive prompt
##        source("cashmanagement.R")
##
## REQUIRED ARGUMENTS: none
##
## OPTIONS: none
## DESCRIPTION: Simulation of the cash management model as a Markov chain
## DIAGNOSTICS: None
## CONFIGURATION AND ENVIRONMENT: None
## DEPENDENCIES: None
## INCOMPATIBILITIES: None known
## PROVENANCE: Created
## The algorithm for simulating the Markov chain is adpated from 
## http://stackoverflow.com/questions/2754469/r-library-for-discrete-markov-chain-simulation 
## BUGS AND LIMITATIONS: None known
## FEATURES AND POTENTIAL IMPROVEMENTS:
## To generate a random starting state from 1 to n-1, it might be that sample(1:(n-1), 1) 
## is a bit easier to understand than ceiling((n-1) * runif(1, 0, 1)). 
## Also, for the innermost for-loop, you can simply use 
## newState <- sample(1:ncol(trans), 1, prob=trans[state,]). 
## That shows more clearly what's going on andyou won't even have to
## normalize the rows, either. 
## On the other hand, if other languages don't have a function comparable to sample, then
## the current formulation is easier to adapt and make the algorithms similar.
##
## An intersesting thing to do would be to vectorize the operations in this program
## to make the program faster (and therefore able to be larger).
## There would be two ways to vectorize:  one to vectorize the creation of the state chain
## and the other to vectorize chains with a vector of starting states s <- (1:n-1)
## Because of the state dependent nature of the Markov chains, neither of these seem to be
## easy or obvious how to do.  Because the programs seem to be fast enough now, vectorizing
## the program may make it more obscure and hard to understand without sufficient speed-up
## to justify the extra effort.

## AUTHOR:  Steve Dunbar
## VERSION:  1.1 Tue Nov 13, 2012  5:26 AM, added output formatting
## KEYWORDS: Markov chain, simulation, reflecting boundary


