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
    pr<-link(a*th.mat+b.mat) ##look very carefully here at how b.mat is being included. is this what you expect? if not, take a look at ?mirt (specifically the form used for the Rash itemtype).
    test<-matrix(runif(ni*np),np,ni)
    resp<-ifelse(pr>test,1,0)
    colnames(resp)<-paste("i",1:ncol(resp))
    resp
}

b<-rnorm(50)
link<-function(x) .25+(1-.25)*(exp(x)/(1+exp(x)))
resp<-sim_data(b=b,link=link) #note that this will rely on the default logistic link 
library(mirt)
m<-mirt(resp,1,itemtype="Rasch") #if you check the mirt documentation, itemtype="Rasch" ends up meaning that we are assuming the correct model for estimation
get_coef<-function(mod) {
    coef(mod)->co
    co[-length(co)]->co
    do.call("rbind",co)
}
plot(-1*b,-1*get_coef(m)[,2],xlab="true difficulties",ylab="estimated difficulties",pch=19); abline(0,1) #now we are going to compare true and estimated item parameters
