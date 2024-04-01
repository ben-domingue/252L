##a helper function we'll use along the way.
##kr20 is a function that computes the kr20 version of cronbach's alpha
kr20<-function(resp) {
    k<-ncol(resp)
    p<-colMeans(resp,na.rm=TRUE)
    q<-1-p
    o<-rowSums(resp)
    (k/(k-1))*(1-sum(p*q)/var(o))
}

###################################################################################################################
##the goal here is to get 'under the hood' of ctt and illustrate some potential shortcomings of ctt
##we'll do that in two ways, the first mainly as a jumping off point for the second.
##first, we'll flip coins.

##a single person flips an even coin (p=0.5) N.items times. that is their string of item responses
##then the next person does the same.
##we generate a large bank of item responses for many people htis way
##what kind of reliability will this item response data have?
##before running the below, generate a hypothesis about what you think will happen.

par(mfrow=c(2,3),mgp=c(2,1,0),mar=c(3,3,2,1))
for (N.items in seq(20,50,by=15)) { 
    for (N.ppl in c(100,1000)) {
        alph<-numeric()
        for (i in 1:25) {
            resp<-matrix(rbinom(N.items*N.ppl,size=1,pr=.5),N.ppl,N.items)
            alph[i]<-kr20(resp)
        }
        boxplot(alph,main=paste(N.items,N.ppl),ylim=c(-.20,1))
        abline(h=mean(alph))
    }
}
mtext(side=1,line=.5,"from coin flips")
##q. what happened? did this meet your expectations?


###################################################################################################################
##now, we were kind of cheating with the coins. we didn't really impose any structure on the thing
##thus, this isn't really demonstrating something too weird about ctt, just that coin flips aren't reliable measures!

##let's now give ourselves a stiffer challenge. let's generate item response data that has
##A. a pre-specified reliability (in terms of the percentage of observed variation that is due to true score variation)
##B. and yet weird inter-workings such that reliability *estimates* fall apart completely. 
##Please distinguish between A and B. The data will be simulated such that the true reliability is what it should be. It's just that estimates of reliability from KR20 require more (without these additional requirements being specified in the CTT model). It's precisely this bit of wiggle room that we're going to exploit.
##C. [i don't really emphasize this in the below, but you can also (roughly) specify the distribution of item p-values. see the 'pr' argument and my comments about it below.]

##below i'm going to do in a number of different ways just that using this function.
sim_ctt<-function(N.items, #N.items needs to be even
                  N.ppl,
                  s2.true, #variance of true scores
                  s2.error, #variance of error scores
                  check=FALSE #this will produce a figure that you can use to check that things are working ok
                  ) { 
    ##desired reliability
    reliability<-(s2.true)/(s2.error+s2.true)
    ##sample (unobserved) true scores & errors
    T<-round(rnorm(N.ppl,mean=round(N.items/2),sd=sqrt(s2.true)))
    E<-round(rnorm(N.ppl,mean=0,sd=sqrt(s2.error)))
    O<-T+E #the CTT model in all its glory
    O<-ifelse(O<0,0,O) #kind of cheating here, don't want scores below 0
    O<-ifelse(O>N.items,N.items,O) #or above N.items
    ##note that we already know how many items a person got right (O)
    ##CTT doesn't specify how individual item responses are generated <evil grin>
    ##so we have carte blanche in terms of coming up with item responses. here's how i'm going to do it. 
    pr<-runif(N.items,min=.25,max=.85)
    pr.interval<-c(0,cumsum(pr))/sum(pr)
    resp<-list()
    for (i in 1:N.ppl) { #for each person
        resp.i<-rep(0,N.items) #start them with all 0s
        if (O[i]>0) { #only do the below for people that got at least 1 item right
            for (j in 1:O[i]) { #now for each of the observed correct responses (for those who got at least 1 item right) i'll pick an item to mark correct
                init.val<-1
                while (init.val==1) {
                    ##what comes next is key
                    ##i first randomly pick an item to get the correct response.
                    ##so as to match the imposed distributino of p-values (see 'pr'), i weight the items by the "pr.interval" argument that is random here but could be pre-specied by the user
                    index<-cut(runif(1),pr.interval,labels=FALSE)
                    ##but i want to make sure that i have picked a new item for this person to get right, so the next line checks that (and doesn't break out of the loop until it is a new item)
                    init.val<-resp.i[index]
                    ##what i've done that is a problem for CTT is that i picked an item in a way that imposes no structure.
                    ##i picked responses so that p-value distributions for items match up, but people with relatively few responses are just as likely to get hard items right as people with large numbers of responses. CTT doesn't demand that this happen, just kind of assumes it will.
                }
                resp.i[index]<-1 #assign a correct response to the 'index' item
            }
        }
        resp[[i]]<-resp.i
    }
    resp<-do.call("rbind",resp)
    ##note that we are getting both the p-values and the true score/observed score relationships right!
    if (check) {
        par(mfrow=c(2,1))
        plot(T,rowSums(resp),xlab="true score",ylab="observed scores"); abline(0,1)
        plot(pr,colMeans(resp),xlab="pr",ylab="observed p-values"); abline(0,1)
    }
    obs.rel<-cor(T,O)^2 #we can use this to make sure that the true and observed score variances are right.
    print(c(reliability,obs.rel)) ##for real time checking, these are the pre-specified reliability and what we get from correlation our true and observed scores
    kr<-kr20(resp)
    c(obs.rel,kr) #the function returns true reliability (obs.rel) and kr20 estimate (kr)
}

##to illustrate the basic idea here, we'll generate 10 sets of item response data.
##for each iteration, we'll use the same parameters, these are below.
N.items<-50
N.ppl<-500
s2.true<-10
s2.error<-3
alph<-list()
alph[[1]]<-sim_ctt(N.items=N.items,N.ppl=N.ppl,s2.true=s2.true,s2.error=s2.error,check=TRUE) #note, the check option is goign to produce a figure that allows us to check the marginals to make sure things are looking ok

for (i in 2:10) {
    alph[[i]]<-sim_ctt(N.items=N.items,N.ppl=N.ppl,s2.true=s2.true,s2.error=s2.error,check=TRUE) #note, the check option is goign to produce a figure that allows us to check the marginals to make sure things are looking ok
}
alph<-do.call("rbind",alph)
plot(alph,xlim=c(0,1),ylim=c(-.2,1),pch=19,xlab="true correlation",ylab="kr20"); abline(0,1)
abline(v=s2.true/(s2.true+s2.error),col="red")
##what you can see here is that the item response datsets have the right property when it comes to the observed correlation between O and T (these correlations are the dots; the red line is the pre-specified reliability)
##but, kr20 is producing reliability estimates that are *way* low

par(mfrow=c(3,3),mgp=c(2,1,0),mar=c(3,3,2,1))
##now let's look at reliabilities for many different datasets generated by the above wherein we vary the true and error variances
N.ppl<-500
N.items<-50
for (s2.true in N.items*c(.1,.5,.9)) {
    for (s2.error in N.items*c(.1,.25,.5)) {
        alph<-list()
        for (i in 1:10) {
            alph[[i]]<-sim_ctt(N.items=N.items,N.ppl=N.ppl,s2.true=s2.true,s2.error=s2.error,check=FALSE)
        }
        plot(do.call("rbind",alph),xlim=c(-1,1),ylim=c(-1,1),pch=19,xlab="true correlation",ylab="kr20")
        mtext(side=3,line=0,paste("s2.true",round(s2.true,2),"; s2.error",round(s2.error,2)),cex=.7)
        abline(0,1)
        abline(h=(s2.true)/(s2.error+s2.true),col="red",lty=2)
        abline(v=(s2.true)/(s2.error+s2.true),col="red",lty=2)
    }
}
##q. how does the kr20 estimate of reliability behave as a function of the true and error variances?
##q. what does this make you think of the CTT model? 
