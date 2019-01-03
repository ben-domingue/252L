##coins

ll<-function(p,x) { 
    like<-function(x,p) p^x*(1-p)^(1-x) #this is the likelihood for a single coin toss (the result is in x) where the probability that x=1 is p
    like<-Vectorize(like,"x")
    sum(log(like(x,p)))
}

true.p<-.7

par(mfrow=c(5,1),mar=c(3,3,1,1),mgp=c(2,1,0))
ii.seq<-c(1,10,100,1000,10000)
for (ii in ii.seq) {
    plot(NULL,xlim=c(0,1),ylim=c(-1e5,0),xlab="",ylab="ll")
    p<-seq(0,1,length.out=100)
    x<-rbinom(ii,1,true.p)
    out<-numeric()
    for (i in 1:length(p)) out[i]<-ll(p[i],x)
    mtext(side=1,line=2,paste(ii,"tosses;mean",mean(x)),cex=.8)
    op<-optim(.5,function(p,x) -1*ll(p,x),x=x,method="Brent",lower=0,upper=1)$par
    lines(p,out)
    segments(op,-1e5,op,ll(op,x))
    abline(v=true.p,col="red",lty=2)
}

##q1-what do you make of the flatness of the likelihood surface? what do you think this corresponds to?
##q2-how sensitive is "flatness" to the value of true.p?
https://en.wikipedia.org/wiki/Fisher_information
