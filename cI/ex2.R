#now the dgp will be wrong (guessing occurs)

source("https://raw.githubusercontent.com/ben-domingue/252L/master/cI/functions.R") ##you'll need to source this 

N<-2000
n<-40
##generate theta and pars
rnorm(N)->theta
cbind(1,rnorm(n,mean=-1.5),runif(n,min=.5,max=0.6))->pars #note that we have lots of item-side guessing here! i also made the items relatively hard. why?

sim_fun(theta=theta,pars=pars)->resp

Qfun<-function(resp) rowSums(resp)
Qfun(resp)->Q

#now let's blind ourselves to the true stuff we don't normally get.
rm("theta")
rm("pars") 

est_fun(resp,mod="Rasch")->est #the Rasch model is very wrong!!

Ql<-list()
for (i in 1:50) {
    rnorm(N,mean=est$hyp[1],sd=sqrt(est$hyp[2]))->th.sim
    sim_fun(theta=th.sim,pars=est$coef)->resp.sim
    Qfun(resp.sim)->Ql[[i]]
}

lapply(Ql,function(x) table(factor(x,levels=0:n))/length(x))->L
for (i in 1:length(L)) data.frame(ss=names(L[[i]]),freq=as.numeric(L[[i]]))->L[[i]]
data.frame(do.call("rbind",L))->L
as.numeric(as.character(L$ss))->L$ss
boxplot(freq~ss,L,xlab="sum score",ylab="density")
lines(density(Q),col="red")
##presumably we could also have detected poor fit via outfit statistics. however, this gives us specific information about the way in which fit is bad that may be valuable.

##but if we use the right model
est_fun(resp,mod="3PL")->est
Ql<-list()
for (i in 1:50) {
    rnorm(N,mean=est$hyp[1],sd=sqrt(est$hyp[2]))->th.sim
    sim_fun(theta=th.sim,pars=est$coef)->resp.sim
    Qfun(resp.sim)->Ql[[i]]
}
lapply(Ql,function(x) table(factor(x,levels=0:n))/length(x))->L
for (i in 1:length(L)) data.frame(ss=names(L[[i]]),freq=as.numeric(L[[i]]))->L[[i]]
data.frame(do.call("rbind",L))->L
as.numeric(as.character(L$ss))->L$ss
boxplot(freq~ss,L,xlab="sum score",ylab="density")
lines(density(Q),col="red")
##agreement is better, but you can still see it lacking a little. in particular, our empirical distribution is more low-ability than the simulated results.
##perhaps due to issues related to estimating guessing parameter?





