##using one of the empirical datasets (look for the emp-... prefix)
##try to use one of these ideas to diagnose fit problems. 
source("https://raw.githubusercontent.com/ben-domingue/252L/master/cI/functions.R") ##you'll need to source this 

resp <- read.table("https://github.com/ben-domingue/252L/raw/master/data/emp-rasch.txt", header=FALSE)
resp[rowSums(is.na(resp))==0,]->resp
nrow(resp)->N
ncol(resp)->n

Qfun<-function(resp) rowSums(resp) #this is going to be the generalized version of a fit statistic
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
##so things look pretty decent!


Qfun<-function(resp) {
    rowMeans(resp)->rm
    apply(resp,2,function(x) cor(x,rm))
}
Qfun(resp)->Q
f<-function(resp) {
    Qfun(resp)->QQ
    est_fun(resp,mod="Rasch")->est.rasch
    est_fun(resp,mod="2PL")->est.2pl
    list(r=est.rasch,two=est.2pl)->L
    for (ii in 1:length(L)) {
        L[[ii]]->est
        Ql<-list()
        for (i in 1:50) {
            rnorm(N,mean=est$hyp[1],sd=sqrt(est$hyp[2]))->th.sim
            sim_fun(theta=th.sim,pars=est$coef)->resp.sim
            Qfun(resp.sim)->Ql[[i]]
        }
        do.call("rbind",Ql)->mat
        stack(data.frame(mat))->xv
        matrix(1:ncol(resp),nrow(mat),ncol(mat),byrow=TRUE)->tmp
        stack(data.frame(tmp))->yv
        data.frame(cor=xv[,1],item=yv[,1])->tmp
                                        #these were item-by-item plots that had some value but were more difficult to follow.
        #boxplot(cor~item,tmp,ylim=c(.2,.6))
        #lines(QQ,col="red")
        hist(tmp[,1],xlim=c(0,1),breaks=50,freq=FALSE,xlab="correlation",main=ifelse(ii==1,"Rasch","2PL"))
        lines(density(QQ),col="red",lwd=4)
    }
}
par(mfrow=c(2,1),mgp=c(2,1,0))
f(resp) 

##what kinds of conclusions would you make about fitting models to this data?
