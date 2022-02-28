##here i am going to examine the sum score when the estimated dgp is correct.
##this is a case where my simulations should match my empirical results
source("https://raw.githubusercontent.com/ben-domingue/252L/master/cI/functions.R") ##you'll need to source this 

N<-5000
n<-60
##generate theta and pars
rnorm(N,sd=1)->theta
cbind(1,rnorm(n),0)->pars


sim_fun(theta=theta,pars=pars)->resp #we're now going to simulate data based on the rasch model
##now let's blind ourselves to the true stuff we don't normally get.
rm("theta")
rm("pars")
##so we're only going to be able to examine byproducts of the empirical data (i.e., the 'resp' matrix)
##super important that you catch what has happened here!!!

#this is going to be the generalized version of a fit statistic.
Qfun<-function(resp) rowSums(resp) 
##it's very simple in this case!
Qfun(resp)->Q

est_fun(resp,mod="Rasch")->est

Ql<-list()
for (i in 1:100) {
    rnorm(N,mean=est$hyp[1],sd=sqrt(est$hyp[2]))->th.sim
    sim_fun(theta=th.sim,pars=est$coef)->resp.sim
    Qfun(resp.sim)->Ql[[i]]
}

lapply(Ql,function(x) table(factor(x,levels=0:n))/length(x))->L
for (i in 1:length(L)) data.frame(ss=names(L[[i]]),freq=as.numeric(L[[i]]))->L[[i]]
data.frame(do.call("rbind",L))->L
as.numeric(as.character(L$ss))->L$ss
boxplot(freq~ss,L,xlab="sum score",ylab="density")
table(factor(Q,levels=0:n,ordered=TRUE))->tab
lines(0:n,as.numeric(tab)/length(Q),col="red",lwd=2)
##so things look quite nice! compare to figure here: https://journals.sagepub.com/doi/10.1177/0146621605285517

