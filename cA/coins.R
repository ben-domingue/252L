##coins

ll<-function(p,x) { 
    like<-function(x,p) p^x*(1-p)^(1-x) #this is the likelihood for a single coin toss (the result is in x) where the probability that x=1 is p
    like<-Vectorize(like,"x") #this takes a function of a scalar and converts it to a function of a vector (where each element of the vector will be operated on as in the original function)
    sum(log(like(x,p)))
}

true.p<-.7 

##the goal is to see how the likelihood behaves as a function of the data and different choices for the estimator of true.p (which, if we just saw the coin tosses, we wouldn't know)
par(mfrow=c(3,2),mar=c(3,3,1,1),mgp=c(2,1,0),oma=rep(1,4)) #tuning graphical parameters, don't worry about this
ii.seq<-c(1,10,100,1000,5000,10000) #this will be the number of tosses of the coin with weight true.p
for (ii in ii.seq) {
    p<-seq(0,1,length.out=100) #this is the grid of 'p' (ie possible values of 'true.p') that we'll consider
    x<-rbinom(ii,1,true.p) #this generated the data by taking ii draws from the appropriate binomial distribution. the binomial distribution tells us about the probability of having k successes from n independent experiments each of which has a probability p of success. in this degenerate case of only a single trial, the binomial collapses into a bernoulli distribution (which is what governs the random behavior of coins).  we're passing three arguments: ii (the number of draws from the binomial distribution), 1 (the number of independent experiments, so n=1 here), and the probability of success. 
    out<-numeric()
    for (i in 1:length(p)) out[i]<-ll(p[i],x) #this computes the likelihood for our "p" grid of values using the data
    ##now let's see where the ll is maximal
    ml.p<-optim(.5,function(p,x) -1*ll(p,x),x=x,method="Brent",lower=0,upper=1)$par
    ##graphing results, don't worry too much about this.
    plot(NULL,xlim=c(0,1),ylim=c(-1e4,0),xlab="",ylab="ll")
    mtext(side=1,line=2,paste(ii,"tosses;mean",mean(x)),cex=.8)
    abline(v=true.p,col="red",lty=2)
    lines(p,out)
    segments(ml.p,-1e5,ml.p,ll(ml.p,x)) 
}

##questions that you can explore via adjustments to true.p
##q1 how does difference between true.p (the vertical dashed red line) and the estime, ml.p (the solid vertical black line segment), change as a function of the # of coin flips (ii)
##q2-what do you make of the flatness of the likelihood surface? what do you think this corresponds to?
##https://en.wikipedia.org/wiki/Fisher_information, "Thus, the Fisher information may be seen as the curvature of the support curve (the graph of the log-likelihood). Near the maximum likelihood estimate, low Fisher information therefore indicates that the maximum appears "blunt", that is, the maximum is shallow and there are many nearby values with a similar log-likelihood. Conversely, high Fisher information indicates that the maximum is sharp.
##q3-how sensitive is "flatness" of black curve to the value of true.p? how can you make the curve flatter or less flat (for a constant number of tosses)? what does this imply? 


