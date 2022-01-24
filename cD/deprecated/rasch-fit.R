## ##############################################################################################
## ##Here is the function we are going to use to simulate data for the Rasch model.
## ##As time permits, make sure you can figure out how this works and what each of the arguments "does". We've finally gotten far enough into the course where everything here should be something you're getting familiar with.

## sim_rasch<-function(n.items,n.people,mean.item.diff=0) {
##     n.items*n.people->N
##     th<-matrix(rnorm(n.people),n.people,n.items,byrow=FALSE)
##     diff<-matrix(rnorm(n.items,mean=mean.item.diff),n.people,n.items,byrow=TRUE)
##     kern<- exp(th - diff)
##     pr<-kern/(1+kern)
##     test<-matrix(runif(N),n.people,n.items)
##     x2<-ifelse(pr>test,1,0)
##     colnames(x2)<-paste("i.",1:ncol(x2))
##     x2
## }
## resp<-sim_rasch(20,100)

## ##############################################################################################
## ##Let's write a function to estimate the Rasch model with the "resp" matrix
## est_rasch<-function(resp) {
##     library(mirt)
##     mirt(resp,1,itemtype="Rasch")->mod
##     coef(mod)->co
##     co[-length(co)]->co #why do i do this?
##     do.call("rbind",co)->pars
##     theta<-fscores(mod,method="ML",full.scores=TRUE) ##note: this is where the ability scoring happens. we'll talk about the details of this component next week.
##     ncol(theta)->nc
##     if (nc==1) as.numeric(theta)->theta else theta[,ncol(resp)+1]->theta
##     list(theta=theta,pars=pars[,2])
## }
## est<-est_rasch(resp)

## ##############################################################################################
## ##To compute fit, we need to compute the estimated probability of a correct response so that we can compute residuals (remember the formula?). Let's do that.
## get_p<-function(est) {
##     length(est$theta)->n1
##     length(est$pars)->n2
##     th<-matrix(est$theta,n1,n2,byrow=FALSE)
##     ea<-matrix(est$pars,n1,n2,byrow=TRUE)
##     exp(th+ea)->kern
##     kern/(1+kern)
## }
## p<-get_p(est)

## ##############################################################################################
## ##Now here is a function that will compute item fit statistics based on responses and estimated probabilities of correct responses
## fit_stat<-function(resp,p) { #these are known as "outfit" fit statistics, developed by Ben Wright (the original source of Rasch model thinking in the US and my advisor's advisor's advisor [wright->wilson->briggs->domingue])
##     1-p->q
##     z<-(resp-p)/sqrt(p*q)
##     fit.u<-colMeans(z^2)
##     fit.u
## }
## fit<-fit_stat(resp,p)

## ##############################################################################################
## ##Let's take a first look at the null distribution of these fit statistics (what does "null" imply here?)
## ##what do you glean from this? 
## plot(density(fit))

## ##############################################################################################
## ##Now let's look at the critical values of this distribution for a fixed number of items (20) and varying numbers of people.
## out<-list()
## for (np in c(100,200,400,600,800,1000)) {
##     fit.list<-list()
##     for (i in 1:25) { #why this loop?
##         test<-0
##         while (test==0) {
##             resp<-sim_rasch(n.items=20,n.people=np)
##             rm<-rowMeans(resp)
##             if (all(rm>0) & all(rm<1)) test<-1 #why am i doing this? it's complicated, so maybe ask me about it ;)
##         }
##         est<-est_rasch(resp)
##         p<-get_p(est)
##         fit<-fit_stat(resp,p)
##         ##
##         fit.list[[i]]<-fit
##     }
##     unlist(fit.list)->fit
##     mean(sapply(fit.list,sd))->S
##     c(np,quantile(fit,c(.025,.975)),S)->out[[as.character(np)]]
## }
## do.call("rbind",out)
## ##congrat btw! you've just completed your first psychometric simulation study ;)

## ##So we've now worked out the critical values of the distribution of the outfit statistics under the null.
## ##Are you surprised by the behavior of these as a function of the number of people?

## ##Results from this analysis can be compared to Table 1 from Wu & Adams
## ##They don't match exactly. Any theories as to why our results differ?

## ##Take a look here
## ##http://www.rasch.org/rmt/rmt83b.htm
## ##What do you make of their recommendation that t-statistics between 0.8 and 1.2 are ok? 
