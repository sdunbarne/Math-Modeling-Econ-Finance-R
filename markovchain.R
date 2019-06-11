n <- 10   # Top boundary, number of states 0..n is n+1
s <- 5   # Start and Reset state number 1 <= s <= n-1
p <- 1/2
steps <- 1000

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
# Initialize the length of the cycle
cycleLength <- 0
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
    cycleLength <- cycleLength + 1

    if (state == n+1 || state == 1) {
        numberCycles <- numberCycles + 1
        avgVisits <- count/numberCycles
        avgCycleLength <- i/numberCycles
    }
}

Wsk <- avgVisits
theoreticalWsk <- 2*(  s*(1-(0:n)/n) - pmax( s - (0:n),0)  ); 

cat(sprintf("Average number of visits to each state in a cycle: \n "))
cat(Wsk)
cat("\n\n")
cat(sprintf("Theoretical number of visits to each state in a cycle: \n "))
cat(theoreticalWsk)
cat("\n")

## NAME: markovchain.R
##
## USAGE: within R, at interactive prompt
##        source("markovchain.R")
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
## That shows more clearly what's going on andyou won't even have to normalize the rows, either. 
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
## VERSION: 1.1  Mon Nov 12, 2012  5:37 AM
## KEYWORDS: Markov chain, simulation, reflecting boundary


