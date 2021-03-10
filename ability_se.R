##not really finished


library(mirt)
Np<-1000
Ni<-50
th<-rnorm(Np)
a<-rep(1,Ni)
c<-runif(Ni,0,max=.3) #rep(0,Ni)
b<-rnorm(Ni)
a<-matrix(a,nrow=Np,ncol=Ni,byrow=TRUE)
b<-matrix(b,nrow=Np,ncol=Ni,byrow=TRUE)
c<-matrix(c,nrow=Np,ncol=Ni,byrow=TRUE)
th.mat<-matrix(th,nrow=Np,ncol=Ni,byrow=FALSE)
est<-list()
for (ii in 1:100) {
    p<-c+(1-c)/(1+exp(-1*a*(th.mat-b)))
    resp<-p
    for (i in 1:ncol(resp)) resp[,i]<-rbinom(Np,1,p[,i])
    m<-mirt(data.frame(resp),1,'3PL',parprior=list(c(paste("1-",Ni,sep=''),'beta',2,5)))
    est[[ii]]<-fscores(m)
}
est<-do.call("cbind",est)
se<-apply(est,1,sd)
plot(th,se)
