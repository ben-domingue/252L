sim_data<-function( #this will simulate item response data using whatever link you send it
                   b, #item difficulties
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
    k<-a*th.mat+b.mat ##look very carefully here at how b.mat is being included. is this what you expect? if not, take a look at ?mirt (specifically the form used for the Rash itemtype).
    pr<-apply(k,2,link)
    test<-matrix(runif(ni*np),np,ni)
    resp<-ifelse(pr>test,1,0)
    colnames(resp)<-paste("i",1:ncol(resp))
    resp
}




##first, the default
b<-rnorm(50)
resp<-sim_data(b=b) #note that this will rely on the default logistic link 
library(mirt)
m<-mirt(resp,1,itemtype="Rasch") #if you check the mirt documentation, itemtype="Rasch" ends up meaning that we are assuming the correct model for estimation
get_coef<-function(mod) {
    coef(mod)->co
    co[-length(co)]->co
    do.call("rbind",co)
}
plot(b,get_coef(m)[,2],xlab="true diff",ylab="est diff"); abline(0,1) #now we are going to compare true and estimated item parameters

##now, the normal cdf
b<-rnorm(50)
resp<-sim_data(b=b,link=pnorm) #note we are sending a different link. what is this sigmoid?
m<-mirt(resp,1,itemtype="Rasch")
plot(b,get_coef(m)[,2],xlab="true diff",ylab="est diff"); abline(0,1) 
abline(0,1.7,col="red") ##what is this? [https://journals.sagepub.com/doi/10.3102/10769986019003293]

##something with heavy tails
b<-rnorm(50)
resp<-sim_data(b=b,link=function(x) pt(x,df=50)) #an even differenter link!
m<-mirt(resp,1,itemtype="Rasch")
plot(b,get_coef(m)[,2],xlab="true diff",ylab="est diff"); abline(0,1) 

##skew
library(sn)
x<-seq(-3,3,length.out=100)
plot(x,dsn(x,alpha=3),type="l") #note the skew
plot(x,psn(x,alpha=3),type="l") #corresponding icc
b<-rnorm(50)
resp<-sim_data(b=b,link=function(x) psn(x,alpha=3)) #now we've really gone overboard ;)
m<-mirt(resp,1,itemtype="Rasch")
plot(b,get_coef(m)[,2],xlab="true diff",ylab="est diff"); abline(0,1) #looking rough!


