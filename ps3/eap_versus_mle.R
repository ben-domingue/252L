get_pr<-function(th,a,b,c) {
    x<-a*(th-b)
    c+(1-c)/(1+exp(-x))
}
f1<-function(th,resp,a,b,c) { #used in straight MLE
    p<-get_pr(th,a,b,c)
    -sum(resp*log(p)+(1-resp)*log(1-p))
}
rmse<-function(x,y) sqrt(mean((x-y)^2))

ni<-10
b<-rnorm(ni) #difficulties
a<-exp(rnorm(ni,mean=0,sd=.25)) #discriminations
c<-runif(ni,min=0,max=0)

th<-rnorm(100)
est.mle<-est.eap<-numeric()

for (i in 1:length(th)) {
    ##we'll use th to simulate data
    p<-get_pr(th[i],a,b,c)
    resp<-rbinom(length(p),1,p)
    ##old MLE estimation based on optimization of f1
    est.mle[i]<-optim(0,f1,method='Brent',lower=-10,upper=10,a=a,b=b,c=c,resp=resp)$par
    ##EAP estimation!!
    x<-seq(-5,5,length.out=1000)
    n<-d<-numeric() #numerator and deminator here  ##https://www.rasch.org/rmt/rmt163i.htm
    for (ii in 1:length(x)) {
        x0<-x[ii]
        p<-get_pr(x0,a,b,c)
        p<-prod(p^resp*(1-p)^(1-resp))
        n[ii]<-x0*dnorm(x0)*p
        d[ii]<-dnorm(x0)*p
    }
    est.eap[i]<-sum(n)/sum(d)
}
df<-data.frame(th=th,est.mle=est.mle,est.eap=est.eap)
plot(df)
rmse(th,est.mle)
rmse(th,est.eap) ##what do you notice about the RMSEs?


