library(mirt)
Science
## Description
## This data set comes from the Consumer Protection and Perceptions of Science and Technology
## section of the 1992 Euro-Barometer Survey (Karlheinz and Melich, 1992) based on a sample from
## Great Britain. The questions asked are given below:
## Format
## All of the below items were measured on a four-group scale with response categories "strongly
## disagree", "disagree to some extent", "agree to some extent" and "strongly agree":

## Comfort: Science and technology are making our lives healthier, easier and more comfortable.
## Environment: Scientific and technological research cannot play an important role in protecting the
## environment and repairing it.
## Work: The application of science and new technology will make work more interesting.
## Future: Thanks to science and technology, there will be more opportunities for the future generations.
## Benefit The benefits of science are greater than any harmful effect it may have.

##q: what do higher numbers mean here?
##q: what is this scale "measuring"?

apply(Science,2,table) #or, if you prefer, round(apply(Science,2,table) / nrow(Science), 2)
#q. what are the salient features of this data?
#q. what score categories concern you?

library(psych)
alpha(Science) 

##here is the mirt description of what we will estimate
## graded The graded model consists of sequential 2PL models, and
##      here k is the predicted category.
##      P(x = k | theta, psi) = P(x >= k | theta, phi) - P(x >= k + 1 | theta, phi)
mod <- mirt(Science, 1,itemtype="graded") 

mod
coef(mod,IRTpars=TRUE) 
##we will see these coefficients reappear momentarily when we look at trace lines.
##but, at this point, you should be able to draw them! ;)

plot(mod, type = 'trace')
##hm. let's check
extr <- extract.item(mod,1)
Theta <- matrix(seq(-6,6, length.out=2000))
pr <- probtrace(extr, Theta) #min() of first item
ii<-which.min(abs(pr[,1]-.5))
Theta[ii] #you should be able to match this value to something for the $Comfort item
ii<-which.min(abs(pr[,1]+pr[,2]-.5))
Theta[ii] #this one should match as well
ii<-which.min(abs(pr[,1]+pr[,2]+pr[,3]-.5))
Theta[ii] #and this one

##other plots mirt will produce for you :)
plot(mod,type="info")
plot(mod,type="rxx")
plot(mod,type="infotrace")
plot(mod,type="SE")
plot(mod,type="score")

##Let's now consider other types of models
mod.grm<-mod
mod.pcm <- mirt(Science, 1,itemtype="Rasch") #this will estimate the PCM
mod.srm <- mirt(Science, 1,itemtype="Tutz")

##Let's now compare the CRFs for the three models for item n
f<-function(mod,n) { #see line 41
    extr <- extract.item(mod,n)
    Theta <- matrix(seq(-6,6, length.out=2000))
    pr <- probtrace(extr, Theta) #min() of first item
    list(Theta,pr)
}
mods<-list(grm=mod.grm,pcm=mod.pcm,srm=mod.srm)
n<-1
out<-list()
for (i in 1:length(mods)) out[[i]]<-f(mods[[i]],n)

par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3,3,1,1))
for (i in 1:4) {
    plot(NULL,xlim=c(-5,5),ylim=0:1,xlab='theta',ylab='Pr')
    for (j in 1:length(out)) {
        z<-out[[j]]
        lines(z[[1]],z[[2]][,i])
    }
}
