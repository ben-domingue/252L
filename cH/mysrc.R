###############################################################################################
library(mirt)
library("lme4")
data("VerbAgg")
VerbAgg$r2<-ifelse(VerbAgg$r2=="Y",1,0)
VerbAgg

###############################################################################################
## A first example: A Rasch model with fixed item effects and random person effects
#m1.true<-glmer(r2 ~ 0 + item + (1|id), data=VerbAgg, family="binomial") ##takes too long!
m1<-lmer(r2 ~ 0 + item + (1|id), data=VerbAgg)
summary(m1) #what do we make of the fixed effect estimates? what are these effectively?
hist(ranef(m1)$id[,1]) #and what are these?

###############################################################################################
##mirt version
#rearrange data
L<-list()
for (i in unique(VerbAgg$item)) {
    z<-VerbAgg[VerbAgg$item==i,]
    L[[i]]<-z$r2[order(z$id)]
}
resp<-do.call("cbind",L)
m1.mirt<-mirt(resp,1,'Rasch')
gender<-z$Gender #saving for later use
table(gender) #what do you think about this?

###############################################################################################
##let's compare mirt and lmer estimates
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
##compare item parameters
co<-coef(m1.mirt)[-length(coef(m1.mirt))]
co<-do.call("rbind",co)
easy.pars<-cbind(fixef(m1),co[,2])
plot(easy.pars,xlab="lmer parameters",ylab="mirt parameter",pch=19)
##compare person parameters
person.pars<-cbind(ranef(m1)$id[,1],fscores(m1.mirt)[,1])
plot(jitter(person.pars[,1]),jitter(person.pars[,2]),xlab="lmer abilities",ylab="mirt abilities",cex=.5) #note the differences in the scales! what's going on?
##impressive given that the lmer-based estimates are coming from the linear probability model!!

##STOP

###############################################################################################
##Now let's begin to look at some more questions complicated 

##Is endorsement that you "want" to do something more likely than saying you'd actually "do" it?
VerbAgg$subitem<-paste(VerbAgg$btype,substr(VerbAgg$item,1,2))
m2<-lmer(r2 ~ 0+mode+subitem+ (1|id), data=VerbAgg)
summary(m2) ##what do you think?

##STOP

##Are males more likely to be verbally aggressive?
m3<-lmer(r2 ~ 0+item+ Gender+(1|id), data=VerbAgg)
summary(m3)
##FWIW, the IRT-ish way of doing this is captured via the multipleGroup function in mirt
m<-multipleGroup(resp,1,group=gender,invariance=c("slopes","intercepts","free_mean","free_var"))
coef(m)$F$GroupPars
coef(m)$M$GroupPars

##STOP

##Are males differentially more likely to say that they will "do" (as opposed to the gender difference for "want")?
m4<-lmer(r2 ~ 0+mode*Gender+subitem+ (1|id), data=VerbAgg)
summary(m4)
