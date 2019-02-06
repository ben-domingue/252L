##Goal
##1. Make sense of how to simulate item response data
##2. See connection with logistic regression when theta is known.

################################################################
##first, we're going to simulate some data

##number of items and people. we'll start small just so that you can see everything, but you'll want to make this bigger downstream.
ni<-30
np<-2000
##now we're going to simulate data according to this model and examine some key properties
set.seed(12311)
##first let's describe the individual level part of the model
th<-rnorm(np)
th.mat<-matrix(th,np,ni,byrow=FALSE) #these are the true abilities. we don't observe them, which will be a real problem for us downstream. but we'll not worry about that today. 
##now the item level part. this is going to look like logistic regression, meaning we will have a slope and an intercept
a<-rep(1,ni)
a<-exp(rnorm(ni,sd=.3))
b<-rnorm(ni)
a.mat<-matrix(rep(a,np),np,ni,byrow=TRUE)
b.mat<-matrix(b,np,ni,byrow=TRUE) #these are the item difficulties

################################################################
##now we have to put this stuff together. what we want is a probability of a correct response for a person to an item
##we're goign to use what you may know from logistic regression
inv_logit<-function(x) exp(x)/(1+exp(x))
##now the probability of a correct response is:
pr<-inv_logit(a.mat*(th.mat+b.mat)) #note this is pairwise multiplication not matrix multiplication
##we can simulate data using that probability
##here is the kind of sneaky way i like to do it.
test<-matrix(runif(ni*np),np,ni)
resp<-ifelse(pr>test,1,0)

################################################################
##we're going to now consider a prototype of an item response model
##bump up the sample size to a reasonable level before you do this.
coefs<-list()
for (i in 1:ncol(resp)) {
    glm(resp[,i]~th,family="binomial")->mod
    coef(mod)->coefs[[i]]
}
do.call("rbind",coefs)->coefs
##what do we have here?

plot(b,coefs[,1]); abline(0,1) #how about this?

##ALWAYS REMEMBER: in practice you don't ever see th, a, or b. we are cheating here.

################################################################
##so let's estimate parameters & abilities abilities
library(mirt)
mod<-mirt(resp,1,itemtype="2PL")
coef(mod) ##see variation in the slopes? is there a lot or a little? how could we change this?
##now let's get theta values
theta<-fscores(mod,full.scores.SE=TRUE) ##what flavor scores are these?
par(mfrow=c(1,2))
plot(rowSums(resp),theta[,1],xlab="sum score",ylab="theta")
plot(theta,xlab="theta",ylab="se",ylim=c(0,.8))

