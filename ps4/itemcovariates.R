dataset <- redivis::user("datapages")$dataset("item_response_warehouse",version='v5.0')
nm <- c("frac20")
df <- dataset$table(nm)$to_data_frame()
library(lme4)

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

##now let's focus on the Q matrix components
nms<-paste("Qmatrix__",1:8,sep='')
fm<-paste("resp~0+(1|id)+",paste(nms,collapse="+"))
m2<-lmer(as.formula(fm),df)

##now a CDM approach
library(GDINA)
m3 <- GDINA(frac20$dat,frac20$Q,model="DINA") #you'll need to install GDINA
coef(m3) #these show the probability of a correct response for a given pattern of skills. you can see the A part of DINA in the fact that the probabilities are constant for patterns that are not all 1s
z<-personparm(m3) #here we have hard calls for whether a person has each of the 8 skills
##hard to know how to contrast theta estimates and skills so here are two ad hoc approaches
##first let's look at ability estimates and average number of skills
th<-ranef(m1)[[1]][,1]
plot(th,rowSums(z)) 
##second let's look at ability estimates for each skill
th.avg<-numeric()
for (i in 1:ncol(z)) th.avg[i]<-mean(th[z[,i]==1],na.rm=TRUE)
th.avg

##fit
AIC(m1)
AIC(m2)
AIC(m3)
