sim<-function(ni=30,np=2000) {
    ##This will simulate response data in a long format to emphasize that this is the 'likelihood' view of the response data
    x<-expand.grid(id=1:np,item=1:ni)
    th<-rnorm(np)
    x<-merge(x,data.frame(id=1:np,th=th))
    a<-exp(rnorm(ni,sd=.3))
    b<-rnorm(ni)
    x<-merge(x,data.frame(item=1:ni,a=a,b=b))
    k<-x$th-x$b
    p<-1/(1+exp(-x$a*k))
    x$resp<-rbinom(length(p),1,p)
    x
}

x<-sim(ni=10)
##Now we'll induce purely random missingness 
x$resp.full<-x$resp
p<-.4
miss<-rbinom(nrow(x),1,p)
x$resp<-ifelse(miss==1,NA,x$resp.full)

##now we're ready for analysis
resp<-irw::long2resp(x)
id<-resp$id #might need this! 
resp$id<-NULL
library(mirt)
m<-mirt(resp,1,'2PL')

##Your task
##1. decide what you are going to analyze (options: abilities. item parameters.)
##2. you can modify sim() to also return th/a/b (or take those as inputs)
##3. you can also compare to the estimates we'd get from analysis of resp.full
##4. might want to turn p up to something larger to make it more obvious what is going on at the outset

x$resp<-x$resp.full
resp<-irw::long2resp(x)
resp$id<-NULL
library(mirt)
m.full<-mirt(resp,1,'2PL')
##
z<-x[,c("id","th")]
z<-z[!duplicated(z$id),]
th<-fscores(m)
z<-merge(z,data.frame(id=id,th.obs=th[,1]))
th<-fscores(m.full)
z<-merge(z,data.frame(id=id,th.full=th[,1]))
cor(z[,-1])

