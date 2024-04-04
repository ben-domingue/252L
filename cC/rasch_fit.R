##goal: to recreate something like Table 1 from Wu & Adams
##to do that, we need to do the following things:
##1. simulate data
##2. estimate rasch model for that simulated data
##3. compute fit statistics
##we're going to build some functions to do all of that below and then put things together in a simulation study.
##i'd work at this in layers.
##first understand the broad architecture (how do we use the different functions to put together the simulation study).
##second, what is going on in the guts of each function? 


# Simulating data --------------------------------------------------------------
## Here is the function we are going to use to simulate data for the Rasch model. As time permits, make sure you can figure out
## how this works and what each of the arguments 'does'. We've finally gotten far enough into the course where everything here
## should be something you're getting familiar with.
sim_rasch <- function(n.items, n.people, mean.item.diff = 0) {
  N <- n.items * n.people
  th <- matrix(rnorm(n.people), n.people, n.items, byrow = FALSE)
  diff <- matrix(rnorm(n.items, mean = mean.item.diff), n.people, n.items, byrow = TRUE)
  kern <- exp(th - diff)
  pr <- kern/(1 + kern)
  test <- matrix(runif(N), n.people, n.items)
  resp <- ifelse(pr > test, 1, 0)
  colnames(resp) <- paste("i.", 1:ncol(resp))
  resp <- resp[rowSums(resp) != 0 & rowSums(resp) != n.items, ] # can you figure out why this might be necessary?
  resp
}

resp <- sim_rasch(20, 100) # PAUSE AND LOOK AT WHAT resp LOOKS LIKE

# Estimating the Rasch model ----------------------------------------------
## Let's write a function to estimate the Rasch model with the 'resp' matrix
est_rasch <- function(resp) {
  library(mirt)
  mod <- mirt(resp, 1, itemtype = "Rasch") #first step of rasch estimation
  co <- coef(mod)
  co <- co[-length(co)]#why do i do this?
  pars <- do.call("rbind", co)
  theta <- fscores(mod, method = "ML", full.scores = TRUE)  ##note: this is where the ability scoring happens. we'll talk about the details of this component next week.
  nc <- ncol(theta)
  if (nc == 1) 
    theta <- as.numeric(theta) else theta <- theta[, ncol(resp) + 1]
  list(theta = theta, pars = pars[, 2])
}

est <- est_rasch(resp) # PAUSE AND LOOK AT est

# Estimated probabilities -----------------------------------------------------------
## To compute fit, we need to compute the estimated probability of a correct response 
## so that we can compute residuals (remember the formula?). Let's do that.
get_p <- function(est) {
  n1 <- length(est$theta)
  n2 <- length(est$pars)
  th <- matrix(est$theta, n1, n2, byrow = FALSE)
  ea <- matrix(est$pars, n1, n2, byrow = TRUE)
  kern <- exp(th + ea)
  kern/(1 + kern)
}

p <- get_p(est) # PAUSE AND LOOK AT p

# Compute item fit statistics ---------------------------------------------
## Now here is a function that will compute item fit statistics based on responses and estimated probabilities of correct
## responses these are known as 'outfit' fit statistics, developed by Ben Wright (the original source of Rasch model thinking in
## the US and my advisor's advisor's advisor [wright->wilson->briggs->domingue])

fit_stat <- function(resp, p) {
  q <- 1 - p
  z <- (resp - p)/sqrt(p * q)
  fit.u <- colMeans(z^2)
  fit.u
}

fit <- fit_stat(resp, p) # PAUSE AND LOOK AT fit

# Null distribution -------------------------------------------------------
## Let's take a first look at the null distribution of these fit statistics 
## (what does 'null' imply here?) what do you glean from this?
plot(density(fit))

## Now let's look at the critical values of this distribution
## for a fixed number of items (20) and varying numbers of people.
out <- list()
for (np in c(100, 200, 400, 600, 800, 1000)) { #IMPORTANT: this will take a few minutes. i would recommend starting the loop and then looking at it while it runs
  fit.list <- list()
  for (i in 1:25) {
    # why this loop?
    test <- 0
    while (test == 0) {
      resp <- sim_rasch(n.items = 20, n.people = np)
      rm <- rowMeans(resp)
      if (all(rm > 0) & all(rm < 1)) 
        test <- 1  #why am i doing this? it's complicated, so maybe ask me about it ;)
    }
    est <- est_rasch(resp)
    p <- get_p(est)
    fit <- fit_stat(resp, p)
    ## 
    fit.list[[i]] <- fit
  }
  fit <- unlist(fit.list)
  S <- mean(sapply(fit.list, sd))
  out[[as.character(np)]] <- c(np, quantile(fit, c(0.025, 0.975)), S)
}

do.call("rbind", out)


## So we've now worked out the critical values of the distribution of the outfit statistics under the null. Are you surprised by
## the behavior of these as a function of the number of people?

## Results from this analysis can be compared to Table 1 from Wu & Adams (specifically their column based on 20 items).
## They don't match exactly (but they should be fairly close). Any theories as to why our results differ?
## Take a look here and see if you agree with recommendations here:  http://www.rasch.org/rmt/rmt83b.htm

##Let's experiment with our empirical dataset from the first bit of code.
resp1 <- read.table("https://github.com/ben-domingue/252L/raw/master/data/emp-rasch.txt", header=FALSE)
resp<-resp1[rowSums(is.na(resp1))==0,] #getting rid of those with missing data
rs<-rowSums(resp)
resp<-resp[rs>0 & rs<ncol(resp),] #getting those with all or no incorrect responses
est <- est_rasch(resp)
p <- get_p(est)
fit <- fit_stat(resp, p)
sd(fit)
sqrt(2/nrow(resp))
#whatcha think?
