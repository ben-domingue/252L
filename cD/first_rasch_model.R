# Getting started ---------------------------------------------------------
## this will be our maiden voyage estimating IRT models the main goal is to get oriented with the key output 
## that we get from applications of these models to item response data
## this first set of code is just to get resp1, resp2, resp3, and resp4 from cB/item_analysis.R
## (you can probably mostly just run this set of code and ignore it)

resp1 <- read.table("https://github.com/ben-domingue/252L_winter2018/raw/master/data/emp-rasch.txt", header=FALSE)

ni <- ncol(resp1)
np <- nrow(resp1)
set.seed(12311) 
th <- rnorm(np)
th.mat <- matrix(th, np, ni, byrow = FALSE) 
a <- 1 
b <- rnorm(ni)
b.mat <- matrix(b, np, ni, byrow = TRUE)
inv_logit <- function(x) exp(x)/(1+exp(x))
pr <- inv_logit(a*th.mat+b.mat) 
test <- matrix(runif(ni*np),np,ni) 
resp2 <- ifelse(pr>test,1,0) 

resp3 <- read.table("https://github.com/ben-domingue/252L_winter2018/raw/master/data/emp-reading-3pl-gpcm.txt",header=TRUE)

ncat <- apply(resp3,2,function(x) length(table(x)))
resp4 <- resp3[,ncat==2]

# Analyzing one dataset ---------------------------------------------------
##i'll write code for a generic 'resp' dataset. you can experiment with the different ones 
## (e.g., change 'resp1' in below line to resp2 or whatever) as you see fit.

resp <- resp1 # choose which resp to use (i.e. can switch to resp <- resp2 for a different dataset)

## fit a model
library(mirt) # might have to run install.packages("mirt")
m1 <- mirt(resp1, 1, itemtype = "Rasch")
itemplot(m1, 1)  #what do we have here?
plot(m1, type = "trace") ## which is the easiest item? the most difficult item?

## get coefficients
coef(m1)

get_coef <- function(mod) {
  co <- coef(mod)
  co <- co[-length(co)]#why do i get rid of this last bit?
  do.call("rbind", co)
}

get_coef(m1)
## what do we have here? in particular, i would look over this closely vis-a-vis the relevant part of the mirt manual: Rasch Only one intercept estimated,
    ## and the latent variance of theta is freely estimated. If the data have more than two categories then a partial credit model is used instead (see 'gpcm'
    ## below).

    ## P(x = 1|theta, d) = \frac{1}{1 + exp(-(theta + d))}

    ## 2-4PL Depending on the model u may be equal to 1 and g may be equal to 0.

    ## P(x = 1|theta, psi) = g + \frac{(u - g)}{ 1 + exp(-(a_1 * theta_1 + a_2 * theta_2 + d))} question: what was the scale identification restriction used
    ## here?

## here is a fun way of looking at comparing the estimated icc to empirical data
itemfit(m1, empirical.plot = 3)

## question 1: can you compare this 'stuff' to the ctt item statistics we produced in the last analysis (item_analysis.R)?

## question 2: where are our theta estimates?

## question 3: what if you consider rasch models for resp2 or resp3? IMPORTANT: recall that one of the items had very little variation in the CTT discrimination
## coefficients. is that showing up here in any way?
