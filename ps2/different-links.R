sim_data<-function(b, #item difficulties
                   np=1000,
                   link=function(x) exp(x)/(1+exp(x))
                   )
{
    ni<-length(b)
    th<-rnorm(np)
    th.mat<-matrix(th,np,ni,byrow=FALSE) 
    a<-1
    b.mat<-matrix(b,np,ni,byrow=TRUE) #these are the item difficulties
    ##now the probability of a correct response is:
    pr<-link(a*th.mat+b.mat) ##look very carefully here at how b.mat is being included. is this what you expect? if not, take a look at ?mirt (specifically the form used for the Rash itemtype).
    test<-matrix(runif(ni*np),np,ni)
    resp<-ifelse(pr>test,1,0)
    colnames(resp)<-paste("i",1:ncol(resp))
    resp
}




##first, the default
b<-rnorm(50)
resp<-sim_data(b=b) 
library(mirt)
m<-mirt(resp,1,itemtype="Rasch")
get_coef<-function(mod) {
    coef(mod)->co
    co[-length(co)]->co
    do.call("rbind",co)
}
plot(b,get_coef(m)[,2]); abline(0,1)

##now, the normal
b<-rnorm(50)
resp<-sim_data(b=b,link=pnorm) 
m<-mirt(resp,1,itemtype="Rasch")
plot(b,get_coef(m)[,2]); abline(0,1) 
abline(0,1.7,col="red") ##what is this?

##something with heavy tails
b<-rnorm(50)
resp<-sim_data(b=b,link=function(x) pt(x,df=50)) 
m<-mirt(resp,1,itemtype="Rasch")
plot(b,get_coef(m)[,2]); abline(0,1) 

##skew
library(sn)
x<-seq(-3,3,length.out=100)
plot(x,dsn(x,alpha=3),type="l") #note the skew
plot(x,psn(x,alpha=3),type="l") #corresponding icc
b<-rnorm(50)
resp<-sim_data(b=b,link=function(x) dsn(x,alpha=3)) 
m<-mirt(resp,1,itemtype="Rasch")
plot(b,get_coef(m)[,2]); abline(0,1) #hm, what is going on here? can we make sense of why parameter estimation is quite bad when b is large
