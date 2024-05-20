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

x<-sim(ni=10,np=10000)
##Now we'll induce missinness 
x$resp.full<-x$resp
mmp<-.7 #this will be the Max Missing Probability.
p2<-mmp-x$p*mmp
miss<-rbinom(nrow(x),1,p2)
x$resp<-ifelse(miss==1,NA,x$resp.full)

##now we're ready for analysis
resp<-irw::long2resp(x)
id<-resp$id #might need this! 
resp$id<-NULL
library(mirt)
m<-mirt(resp,1,'2PL')

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

z<-z[order(z$th),]
xv<-z$th
yv<-abs(z$th.obs-z$th)
mm<-loess(yv~xv)
plot(mm$x,mm$fitted,type='l',col='red')
##
yv<-abs(z$th.full-z$th)
mm<-loess(yv~xv)
lines(mm$x,mm$fitted,type='l')
