rho<-1
ni<-25
np<-5000
b.miss<- -3

library(MASS)
x<-mvrnorm(np,rep(0,2),matrix(c(1,rho,rho,1),2,2))
th.miss<-x[,1]
th<-x[,2]

b<-rnorm(ni)
k<-outer(th,b,'-')
p<-1/(1+exp(-k))
resp<-p
for (i in 1:ncol(p)) resp[,i]<-rbinom(nrow(p),1,p[,i])

b2<-rnorm(ni)
k<-outer(th.miss,b.miss+b2,'-')
p<-1/(1+exp(-k))
miss<-p
for (i in 1:ncol(p)) miss[,i]<-rbinom(nrow(p),1,p[,i])
resp.obs<-resp
for (i in 1:ncol(resp)) resp.obs[,i]<-ifelse(miss[,i]==1,resp[,i],NA)

library(mirt)
resp.obs<-data.frame(resp.obs)
names(resp.obs)<-paste("item",1:ni,sep='')
rm<-rowMeans(resp.obs,na.rm=TRUE)
test<-rm>0 & rm<1
resp.obs<-resp.obs[test,]
m<-mirt(resp.obs,1,'Rasch')
co<-coef(m,simplify=TRUE,IRTpars=TRUE)$items
th.est<-fscores(m,'ML')

rmse<-function(x,y) sqrt(mean((x-y)^2))
rmse(th[test],th.est[,1])
rmse(b,co[,2])
