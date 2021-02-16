b<- c(-1,0,1)
th<-rnorm(5000)
resp<-list()
for (i in 1:3) {
    p<-1/(1+exp(-(th-b[i])))
    resp[[i]]<-rbinom(length(p),1,p)
}
resp<-do.call("cbind",resp)
resp<-as.data.frame(resp)

library(mirt)
m<-mirt(resp,1,'Rasch')
fscores(m,full.scores=FALSE)

