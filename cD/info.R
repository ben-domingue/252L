##GOAL: to develop intuition for maximum likelihood surface for theta and the test information function.
##note-the code here might be too complex. i would recommend that you run it "block" by "block" and focus on the pictorial output. 

ll<-function(theta,resp,beta,rasch=TRUE,a=rep(1,length(theta)),c=rep(0,length(theta))) {
    if (rasch) {
        kern<-exp(theta-beta)
        p<-kern/(1+kern)
    } else {#more pl
        kern<-exp(a*(theta-beta))
        kern<-kern/(1+kern)
        p<-c+(1-c)*kern
    }
    #
    q<- 1-p
    log(p^resp*q^(1-resp))->terms
    return(sum(terms))
}
Vectorize(ll,vectorize.args="theta")->ll
info<-function(theta,beta,a=rep(1,length(theta)),c=rep(0,length(theta))) {
    kern<-exp(a*(theta-beta))
    kern<-kern/(1+kern)
    p<-c+(1-c)*kern
    q<- 1-p
    sum(a^2*(q/p)*((p-c)/(1-c))^2)
}
Vectorize(info,vectorize.args="theta")->info


theta<-seq(-4,4,length.out=1000)
build.all<-function(resp.l) {
    resp.l->r1->r0
    for (i in 1:length(resp.l)) {
        c(resp.l[[i]],1)->r1[[i]]
        c(resp.l[[i]],0)->r0[[i]]
    }
    c(r1,r0)->resp.l
    resp.l
}

pf<-function(theta,resp.l,y.l,beta) {
    range(unlist(y.l))->yl
    plot(NULL,xlim=range(theta),ylim=yl,bty="n",xlab="",yaxt="n",ylab="")
    for (i in 1:length(beta)) abline(v=beta,col="blue",lwd=.5)
    for (i in 1:length(y.l)) {
        y.l[[i]]->yv
        lines(theta,yv,lwd=1.3)
        mtext(las=2,side=4,at=y.l[[i]][length(yv)],paste(resp.l[[i]],collapse=""),cex=.7)
        mtext(las=2,side=2,at=y.l[[i]][1],paste(resp.l[[i]],collapse=""),cex=.7)
        max(yv)->M
        which.max(yv)->index
        segments(theta[index],yl[1],theta[index],M,lty=2,lwd=.4)
        points(theta[index],M,pch=19,col="red")
    }
    yl
}


pf.i<-function(theta,i.l,M) {
    for (i in 1:length(i.l)) {
        i.l[[i]]->yv
        max(yv)->M2
        (M[2]-M[1])/M2->scale
        M[1]+yv*scale->yv
        lines(theta,yv,lwd=2.5,col="red")
        #lines(theta,1/yv,lwd=2,col="red",lty=2)
    }
}


par(mar=c(2,4,1,2),mgp=c(2,1,0),mfrow=c(4,4))

#1
##1 item
resp.l<-list(1,0)
beta<-0
i.l<-y.l<-list()
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=beta)->y.l[[i]]
    info(theta=theta,beta=beta)->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)


#2
#1 item
resp.l<-list(1,0)
i.l<-y.l<-list()
beta<-0
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=beta,rasch=FALSE,a=2)->y.l[[i]]
    info(theta=theta,beta=beta,a=2)->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)

#3
#1 item
resp.l<-list(1,0)
i.l<-y.l<-list()
beta<-0
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=beta,rasch=FALSE,a=.5)->y.l[[i]]
    info(theta,beta=beta,a=.5)->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)

#4
#1 item
resp.l<-list(1,0)
i.l<-y.l<-list()
beta<-0
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=beta,rasch=FALSE,c=.25)->y.l[[i]]
    info(theta,beta=beta,c=.25)->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)


#5
#1 item
resp.l<-list(1,0)
i.l<-y.l<-list()
beta<-0
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=beta,rasch=FALSE,c=.5)->y.l[[i]]
    info(theta,beta=beta,c=.5)->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)


#6
#2 item
i.l<-y.l<-list()
build.all(resp.l)->resp.l
beta<-c(-1,1)
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=beta)->y.l[[i]]
    info(theta,beta=beta)->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)

#7
#2 item, different disc
i.l<-y.l<-list()
beta<-c(-1,1)
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=beta,a=c(1,2),c=rep(0,2),rasch=FALSE)->y.l[[i]]
    info(theta,beta=beta,a=c(1,2),c=rep(0,2))->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)

#8
#3 item
i.l<-y.l<-list()
build.all(resp.l)->resp.l
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=c(-1,0,1))->y.l[[i]]
    info(theta,beta=c(-1,0,1))->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)

#9
#3 item, not symmetric
i.l<-y.l<-list()
c(-1,0,2)->beta
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=beta)->y.l[[i]]
    info(theta,beta=beta)->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)

#10
#3 item, not rasch
i.l<-y.l<-list()
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=c(-1,0,1),rasch=FALSE,a=c(1,1,2),c=rep(0,3))->y.l[[i]]
    info(theta,beta=c(-1,0,1),a=c(1,1,2),c=rep(0,3))->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)

#11
#3 item, not rasch
i.l<-y.l<-list()
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=c(-1,0,1),rasch=FALSE,a=c(1,1,.5),c=rep(0,3))->y.l[[i]]
    info(theta,beta=c(-1,0,1),a=c(1,1,.5),c=rep(0,3))->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)

#12
#3 item, not rasch
i.l<-y.l<-list()
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=beta,rasch=FALSE,a=c(1,1,2),c=rep(0.2,3))->y.l[[i]]
    info(theta,beta=beta,a=c(1,1,2),c=rep(0.2,3))->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)

#13
#3 item, not rasch
i.l<-y.l<-list()
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=beta,rasch=FALSE,a=c(1,1,1),c=c(0,0,.3))->y.l[[i]]
    info(theta,beta=beta,a=c(1,1,1),c=c(0,0,.3))->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)

#14
#3 item, not rasch
i.l<-y.l<-list()
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=beta,rasch=FALSE,a=c(1,1,1),c=c(0,.15,.3))->y.l[[i]]
    info(theta,beta=beta,a=c(1,1,1),c=c(0,.15,.3))->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)

#15
#4 item
i.l<-y.l<-list()
beta<-c(-1,0,1,2)
build.all(resp.l)->resp.l
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=beta)->y.l[[i]]
    info(theta,beta=beta)->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)

#16
#5 item, not rasch but no guessing
build.all(resp.l)->resp.l
i.l<-y.l<-list()
c(-1,0,0.5,1,2)->beta
for (i in 1:length(resp.l)) {
    ll(theta,resp=resp.l[[i]],beta=beta,rasch=FALSE,a=c(0.5,1,1,2,0.75),c=rep(0,5))->y.l[[i]]
    info(theta,beta=beta,a=c(0.5,1,1,2,0.75),c=rep(0,5))->i.l[[i]]
}
pf(theta,resp.l,y.l,beta=beta)->M
pf.i(theta,i.l,M)

