library(mirt)
library("lme4")
data("VerbAgg")
VerbAgg$r2<-ifelse(VerbAgg$r2=="Y",1,0)
## A first example: A Rasch model with fixed item effects and random person effects
#m1.true<-glmer(r2 ~ 0 + item + (1|id), data=VerbAgg, family="binomial") ##takes too long!
m1<-lmer(r2 ~ 0 + item + (1|id), data=VerbAgg)
##mirt version
#rearrange data
L<-list()
for (i in unique(VerbAgg$item)) {
    z<-VerbAgg[VerbAgg$item==i,]
    L[[i]]<-z$r2[order(z$id)]
}
resp<-do.call("cbind",L)
m1.mirt<-mirt(resp,1,'Rasch')

##compare item parameters
co<-coef(m1.mirt)[-length(coef(m1.mirt))]
co<-do.call("rbind",co)
easy.pars<-cbind(fixef(m1),co[,2])
plot(easy.pars)

##compare person parameters
person.pars<-cbind(ranef(m1)$id[,1],fscores(m1.mirt)[,1])
plot(person.pars)



VerbAgg$subitem<-paste(VerbAgg$btype,substr(VerbAgg$item,1,2))
m2<-lmer(r2 ~ 0+mode+subitem+ (1|id), data=VerbAgg)

m3<-lmer(r2 ~ 0+item+ Gender+(1|id), data=VerbAgg)

m4<-lmer(r2 ~ 0+mode*Gender+subitem+ (1|id), data=VerbAgg)
