sim<-function(ni=30,np=2000) {
    ##This will simulate response data in a long format to emphasize that this is the 'likelihood' view of the response data
    x<-expand.grid(id=1:np,item=1:ni)
    th<-rnorm(np)
    x<-merge(x,data.frame(id=1:np,th=th))
    a<-exp(rnorm(ni,sd=.3))
    b<-rnorm(ni)
    x<-merge(x,data.frame(item=1:ni,a=a,b=b))
    k<-x$th-x$b
    x$p<-1/(1+exp(-x$a*k))
    x$resp<-rbinom(length(x$p),1,x$p)
    x
}

x<-sim()
##Now we'll induce missinness 
x$resp.full<-x$resp
mmp<-.3 #this will be the Max Missing Probability.
p2<-mmp-x$p*mmp
miss<-rbinom(nrow(x),1,p2)
x$resp<-ifelse(miss==1,NA,x$resp.full)

##now we're ready for analysis
resp<-irw::long2resp(x)
id<-resp$id #might need this! 
resp$id<-NULL
library(mirt)
m<-mirt(resp,1,'2PL')

##Now you need to implement your analysis
