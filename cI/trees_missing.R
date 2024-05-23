rho<-0
ni<-20
np<-1000
b.miss<- -2

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
x<-resp.obs ##we will analyze x

######################################
##prep data
L<-list()
for (i in 1:ncol(x)) {
    r1<-ifelse(is.na(x[,i]),0,1)
    r2<-x[,i]
    df1<-data.frame(id=1:nrow(x),item=paste('item',i),node='n1',resp=r1)
    df2<-data.frame(id=1:nrow(x),item=paste('item',i),node='n2',resp=r2)
    L[[i]]<-rbind(df1,df2)
}
df<-data.frame(do.call("rbind",L))

library(lme4)
m1 <- lmer(resp ~ 0 + item:node + (0 + node | id), 
            #family = binomial, #let's use linear probability model to save time
            data = df)

m1 #what do you notice about the correlation between person effects? is this reasonable? based on what? can you adjust somethign above to make this change?
fe<-fixef(m1)
fe #what do you make of variation in the noden1 estimates as compared to the noden2 estimates?
