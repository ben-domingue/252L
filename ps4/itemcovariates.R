dataset <- redivis::user("datapages")$dataset("item_response_warehouse",version='v5.0')
nm <- c("frac20")
df <- dataset$table(nm)$to_data_frame()
library(lme4)

##A
##lme4 style model
m1<-lmer(resp~0+(1|id)+item,df)
resp<-irw::long2resp(df)
resp$id<-NULL
m1.irt<-mirt::mirt(resp,1,'Rasch')

fe<-fixef(m1)
names(fe)<-gsub("itemitem","item",names(fe))
co<-mirt::coef(m1.irt,simplify=TRUE)$items[,2]
z<-merge(fe,co,by=0)
plot(z[,-1]) #very stylish!
abline(0,1) #but they depend on different identification assumptions

##B
##now let's focus on the Q matrix components
nms<-paste("Qmatrix__",1:8,sep='')
fm<-paste("resp~0+(1|id)+",paste(nms,collapse="+"))
m2<-lmer(as.formula(fm),df)

##C
##now a CDM approach
library(GDINA)
m3 <- GDINA(frac20$dat,frac20$Q,model="DINA") #you'll need to install GDINA
coef(m3) #these show the probability of a correct response for a given pattern of skills. you can see the A part of DINA in the fact that the probabilities are constant for patterns that are not all 1s
z<-personparm(m3) #here we have hard calls for whether a person has each of the 8 skills
##hard to know how to contrast theta estimates and skills but one question we can ask is about the relationship between skill acquisition and ability. 
##specifically let's look at ability estimates and average number of skills
th<-ranef(m1)[[1]][,1]
plot(th,rowSums(z)) 

##now let's take another perspective: what if we modeled having a specific skill as a function of theta
xl<-range(th)
plot(NULL,xlim=xl,ylim=0:1,xlab='theta',ylab='Pr(skill=1)')
for (i in 1:ncol(z)) {
    m<-glm(z[,i]~th,family='binomial')
    xx<-seq(xl[1],xl[2],length.out=1000)
    co<-coef(m)
    yy<-1/(1+exp(-(co[1]+co[2]*xx)))
    lines(xx,yy)
}
##What do you make of this figure?

##D
##fit
AIC(m1)
AIC(m2)
AIC(m3)
