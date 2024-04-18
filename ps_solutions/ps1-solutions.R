#see also: https://docs.google.com/document/d/1M038sD0I2jEj_93MKCYAkX08muAeTOt6GLSBg20Fn-0/edit?usp=sharing

######################################################################################
##1
##let's first look at the mean correlations
f<-function(x) {
    cor(x)->C
    C[upper.tri(C,diag=FALSE)]->C
    summary(C)
}
f(x1)
f(x2)
##so, item responses are more or less uncorrelated for x1. this isn't generally very reasonable for item response data. if items are sensitive to some underlying latent trait, they should be associated.

##note: many people looked at the distribution of sum scores. nothing wrong with an examination of your data from that perspective. however, i think you need to be wary here. in general, i don't think one should have too much intuition about what the distribution of sum scores should look like. in particular, one might note that
hist(rowSums(x1))
##looks very normal. this is true; but, in this case, it is not meaningful. x1 is just a matrix of coin flips. cells are all entirely independent of one another. the resulting normality of rowSums(x1) is just an example of the CLT (https://en.wikipedia.org/wiki/Central_limit_theorem): when we add up indepenent random variables, we get normal distributions.


######################################################################################
##2
##this one was pretty sneaky on my part. ;)
load("ps1-logreg.Rdata")
glm(y1~x,df,family="binomial")->m1
glm(y2~x,df,family="binomial")->m2
par(mfrow=c(1,2))
plot(density(residuals(m1,type="response")))
plot(density(residuals(m2,type="response"))) ##m2 was actually simulated to have a lower asymptote such that one never had less than pr(1)=0.2, hence the asymmetry.

##coefficients
load("ps1-logreg.Rdata")
glm(y1~x,df,family="binomial")->m1
exp(coef(m1)[2])
pr<-function(x) exp(x)/(1+exp(x))
pr(-2)/(1-pr(-2))


##now, in terms of fit, looking at AIC is frequently people's first instinct. i would encourage resistance here. it isn't that aic can never be useful, but i generally find applications of such information criteria (AIC, BIC, DIC) to be utterly unintuitive. on the other hand, working with something like residuals is relatively straightforward: we know where these came from! moreover, AIC is usually used when one is fitting a series of models to the same data. here, we are fitting the same model to two different datasets. AIC is not useful in such a context given that it is based on the log-likelihood, a value whose scale is entirely data-dependent.
##aside: i have been working hard on a fit index that i think is *more* useful. see here. https://osf.io/gu3ap/


######################################################################################
##3. 
##first, some people seemed to run into a computational problem (likelihood surfaces are flat lines). i suspect this is due to dealing with the likelihood rather than the log-likelihood.

##most of you noted that estimates got better with larger samples. this is true, but the observation was largely driven by single draws of larger samples i think. consider the following illustrtion:
get.est<-function(x) {
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
    optim(par=c(1,4),ll,x=x)$par #these are the mean and variance estimates produced by maximum likelihood.
}
par(mfrow=c(3,2))
for (Nsample in c(10,100,1000)) {
    est<-list()
    for (i in 1:50) { #we'll pick a bunch of samples of a given size and estimate mean/spread using each
        x<-rnorm(Nsample,mean=0,sd=5)
        est[[i]]<-get.est(x)
        print(i)
    }
    do.call("rbind",est)->est
    plot(density(est[,1]),xlim=c(-4,4),xlab="density of means",main=Nsample)
    plot(density(sqrt(est[,2])),xlim=c(0,10),xlab="density of SD",main=Nsample) #if you look closely, i am focusing on the variance of the normal in get.est instead of the sd. one always need be mindful of the specific parametrization they are working with (or that the software is implicitly using for that matter)
}
##each curve represents the density of estimates from the 50 draws. note that the spread is declining dramatically. this is larger samples leading to decreased SE in our estimates


######################################################################################
##4. response time
##second let's compute descriptives
pv<-colMeans(resp,na.rm=TRUE) #simple "p-values", which in psychometrics tends to just mean the mean number of points for an item
M<-by(df$rt,df$item,mean,na.rm=TRUE)
z<-merge(data.frame(item=names(pv),pv=pv),data.frame(item=names(M),rt=as.numeric(M)))
plot(z$rt,z$pv)
