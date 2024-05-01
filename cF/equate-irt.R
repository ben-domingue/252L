##the goal here is to do some irt-based equating
##note: i'm going to occasionally equate things that *should not* be equated. if you're following closely, you should be able to see this coming.

read.table("https://raw.githubusercontent.com/ben-domingue/252L/master/data/rasch.txt")->resp
##we're now going to form a bunch of groups to be equated. make sure you understand what each one is!!! some of them are randomly equivalent. others, not at all!
##first i'm going to do some ordering with rows and columns, don't worry about this bit.
sample(1:nrow(resp),replace=FALSE)->index
resp[index,]->resp 
sample(1:ncol(resp),replace=FALSE)->index
resp[,index]->resp 

pairs<-list( #let's create a variety of different groups
    re.people=list(rowSums(resp[1:500,]),rowSums(resp[501:1000,])), #here we're just taking the first and second half of the data. randomly equivalent due to our reshuffling above. 
    re.items=list(rowSums(resp[,1:20]),rowSums(resp[,21:50])) #now we're not taking groups of respondents but groups of items! basically creating something like a quasi-parallel form
    )
##now let's take systematically varying splits of the data
resp[,order(colSums(resp))]->resp2
pairs$diff.items <- list(rowSums(resp2[,1:15]),rowSums(resp2[,16:50])) #again we are grouping by items, but in a very specific way. the first list element is all hard items, the second element all easy items. so, very different test forms
resp[order(rowSums(resp)),]->resp2
pairs$diff.people <- list(rowSums(resp2[1:500,]),rowSums(resp2[501:1000,])) #now we're creating two clearly non-equivalent groups. first half is low-achieving people, second half is high-achieving.

#############################################################################################################################################
#############################################################################################################################################
##stocking lord
##recall that this is the irt-based approach. we're going to minimize differences between test characteristic curves (TCCs) based on common items

tcc<-function( #this will compute the TCC based on the item parameters
              pars #needs to be a Nx3 matrix. N=1 is ok, in which case it will return icc.
              ) { 
    pv<-function(theta,a=1,b,c=0) {
        kern<-exp(a*(theta+b))
        kern<-kern/(1+kern)
        p<-c+(1-c)*kern
    }
    Vectorize(pv,"theta")->pv
    theta<-seq(-4,4,length.out=2500)
    p.tot<-rep(0,length(theta))
    for (i in 1:nrow(pars)) {
        pv(theta,a=pars[i,1],b=pars[i,2],c=pars[i,3])->p.tmp
        p.tot+p.tmp->p.tot
    }
    cbind(theta,p.tot)
}

##first we need data with common items!
resp[1:500,1:30]->r1
resp[501:1000,21:50]->r2


##start of stocking-lord demo
library(mirt)
N<-ncol(r1)
s<-paste("F=1-",N,"
        PRIOR = (1-",N,", a1, lnorm, 0.2, 0.2),(1-",N,", g, norm, -1.5, .2)",sep="")
mirt.model(s)->model
mirt(r1,model,itemtype=rep("3PL",N),method="EM",technical=list(NCYCLES=5000))->m1
fscores(m1,response.pattern=r1,method="EAP")->z
data.frame(z)$F->s1
N<-ncol(r2)
s<-paste("F=1-",N,"
        PRIOR = (1-",N,", a1, lnorm, 0.2, 0.2),(1-",N,", g, norm, -1.5, .2)",sep="")
mirt.model(s)->model
mirt(r2,model,itemtype=rep("3PL",N),method="EM",technical=list(NCYCLES=5000))->m2
fscores(m2,response.pattern=r2,method="EAP")->z
data.frame(z)$F->s2
co<-function(mod) {
    length(coef(mod))->n
    coef(mod)[-n]->tmp
    do.call("rbind",tmp)
}
co(m1)->m1
co(m2)->m2

##let's focus on parameters for common items. what do you note?
m1[21:30,]
m2[1:10,]
par(mfrow=c(2,2),mar=c(3,3,1,1),mgp=c(2,1,0))
plot(m1[21:30,1],m2[1:10,1],pch=19,xlab="test1",ylab="test2",main="discrimination"); abline(0,1)
plot(m1[21:30,2],m2[1:10,2],pch=19,xlab="test1",ylab="test2",main="difficulty"); abline(0,1)
plot(m1[21:30,3],m2[1:10,3],pch=19,xlab="test1",ylab="test2",main="guessting"); abline(0,1)
plot(density(rowSums(r1)),lwd=2,xlim=c(0,35),main="density of sum scores")
lines(density(rowSums(r2)),lwd=2,col='red')

## now let's look at the TCC
tcc(m1)->tcc1
tcc(m2)->tcc2
layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
par(mar=c(3,3,1,1),mgp=c(2,1,0))
plot(tcc1,type="l",col="black",ylim=c(0,30),lwd=3,main="all items")
for (i in seq(-4,4,by=1)) abline(v=i,lwd=.4)
for (j in seq(0,100,by=5)) abline(h=j,lwd=.4)
lines(tcc2,col="red",lwd=3)
plot(tcc(m1[21:30,]),type="l",col="black",ylim=c(0,10),lwd=3,main="common items")
tcc(m2[1:10,])->tcc2.sub
lines(tcc2.sub,col="red",lwd=3)
for (i in seq(-4,4,by=1)) abline(v=i,lwd=.4)
for (j in seq(0,100,by=5)) abline(h=j,lwd=.4)
plot(density(s1),lwd=2,xlim=c(-3,3),main="theta")
lines(density(s2),lwd=2,col='red')
##top left, TCC for all items in each group (black is first group)
##top right, TCC on common items
##unequated distributions for sum scores


##now let's actually do the stocking-lord trick
obj_fun<-function(AB,pars1,pars2) { #see https://www.measuredprogress.org/documents/10157/19213/StockingLord.pdf
    AB[1]->A
    AB[2]->B
    tcc(pars1)->p1
    pars2[,1]/A->pars2[,1]
    A*pars2[,2]+B->pars2[,2]
    tcc(pars2)->p2
    dnorm(p1[,1])->qu.pts #quadrature!
    (p1[,2]-p2[,2])^2 -> diff
    sum(qu.pts*diff)
}
#obj_fun(c(1,0),m1[21:30,],m2[1:10,])
optim(c(1,0),obj_fun,method="BFGS",pars1=m1[21:30,],pars2=m2[1:10,])->opt.out
opt.out$par[1]->A
opt.out$par[2]->B
m2[,1]/A->m2[,1]
A*m2[,2]+B->m2[,2]
A
B

layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
par(mar=c(3,3,1,1),mgp=c(2,1,0))
plot(tcc1,type="l",col="black",ylim=c(0,30),lwd=3)
lines(tcc2,col="red",lwd=3)
lines(tcc(m2),col="red",lty=2,lwd=3)
for (i in seq(-4,4,by=1)) abline(v=i,lwd=.4)
for (j in seq(0,100,by=5)) abline(h=j,lwd=.4)
plot(tcc(m1[21:30,]),type="l",col="black",ylim=c(0,10),lwd=3)
lines(tcc2.sub,col="red",lwd=3)
lines(tcc(m2[1:10,]),col="red",lty=2,lwd=3)
legend("topleft",lty=c(1,2),c("original","transformed"))
for (i in seq(-4,4,by=1)) abline(v=i,lwd=.4)
for (j in seq(0,100,by=5)) abline(h=j,lwd=.4)
plot(density(s1),lwd=2,xlim=c(-3,3),main="theta")
lines(density(A*s2-B),lwd=2,col='red')
##red curves are now TCC before/after equating (solid/dashed)

##the first time we did this we used kind of trivial data. let's look at something more devious
##load this datset:
resp[order(rowSums(resp)),]->resp2
resp2[,order(-colSums(resp2))]->resp2
resp2[1:900,1:30]->r1
resp2[101:1000,21:50]->r2
##then find this line: ##start of stocking-lord demo
##and redo things    
