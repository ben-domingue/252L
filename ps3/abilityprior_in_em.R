simdata<-function(ni,th) {
    np<-length(th)
    th.mat<-matrix(th,np,ni,byrow=FALSE) 
    a<-exp(rnorm(ni,sd=.3))
    b<-rnorm(ni)
    a.mat<-matrix(rep(a,np),np,ni,byrow=TRUE)
    b.mat<-matrix(b,np,ni,byrow=TRUE) 
    inv_logit<-function(x) exp(x)/(1+exp(x))
    pr<-inv_logit(a.mat*(th.mat+b.mat))
    resp<-pr
    for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,resp[,i])
    resp<-data.frame(resp)
    names(resp)<-paste("item",1:ni,sep='')
    list(item=data.frame(a=a,b=b),resp=resp)
}

np<-1000
th<-runif(np,min=-3,max=3)

library(mirt)
L<-simdata(ni=25,th=th)
m<-mirt(L$resp,1,'2PL')
th.est<-fscores(m)[,1]
par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1))
hist(th)
hist(th.est)
plot(th,th.est); abline(0,1) #uhoh!


