#################################################
##Likelihood exploration
#################################################

##Suppose we just observed x, a bunch of random numbers.
x<-rnorm(100)
##We first want to see what the distribution looks like. We can do this:
hist(x)

##Looks vaguely normalish, no? [Of course, you can see that I'm drawing variates from the normal distribution, so this isn't surprising. Pretend you didn't see that part!]
##So what if we wanted to estimate the mean and var of the normal distribution that may have generated this set of draws.

##How do we do this? The key statistical technique is to consider the likelihood.
##Let's start by writing a function that computes the likelihood for "x" in a normal with unknown mean and var (collectively, "pars").
likelihood<-function(pars,x) { #see the first eqn here, http://mathworld.wolfram.com/NormalDistribution.html
    tmp<-exp(-(x-pars[1])^2/(2*pars[2]))
    tmp/sqrt(2*pars[2]*pi)
}

##To completely evaluate this function, we would need to know x and pars. We only know x (this is the problem of estimation in general: the values in pars are not known to us!).
##With known x, we can think of the likelihood as the "probability" of observing the draws x from a normal distribution with parameters pars.
##That is, we are thinking of the likelihood as a function of pars (x is known).

##Let's think about what we get if the mean is unknown and the SD=1
out<-list()
for (m in seq(-1,1,length.out=100)) {
    like<-rep(NA,length(x))
    for (i in 1:length(x)) {
        like[i]<-likelihood(c(m,1),x[i])
    }
    c(c(m,prod(like)))->out[[as.character(m) ]]
}
plot(do.call("rbind",out),type="b") #this is a likelihood surface where we're seeing the likelihood as a function of the unknown mean
#Q. what do you notice? 

##From a computational perspective, working with the products of small numbers is very unstable. So we instead work with the sum of the logs.
##Why is this ok? First of all, log(xy)=log(x) + log(y)
##Second, log(f(x)) is a monotic transformation of f(x). So if we maximize log(f(x)) funtion, then we've also maximized f(x)
##Below is a function that will do this.
ll<-function(pars,x) {
    likelihood<-function(pars,x) {
        tmp<-exp(-(x-pars[1])^2/(2*pars[2]))
        tmp/sqrt(2*pars[2]*pi)
    }
    like<-rep(NA,length(x))
    for (i in 1:length(x)) {
        like[i]<-likelihood(pars,x[i])
    }
    -1*sum(log(like))
}
optim(par=c(-2,2),ll,x=x)$par #these are the mean and variance estimates produced by maximum likelihood.

#Q. How do our estimates vary in accuracy as a function of the sample size (change 100 to something much bigger and much smaller in the call to "rnorm" at the top)? What does this connect to in your understanding of estimation theory (think standard error)?
