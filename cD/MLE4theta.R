##Let's say we have 3pl items with known abilities

get_pr<-function(th,a,b,c) {
    x<-a*(th-b)
    c+(1-c)/(1+exp(-x))
}
f<-function(th,resp,a,b,c) {
    p<-get_pr(th,a,b,c)
    -sum(resp*log(p)+(1-resp)*log(1-p))
}
rmse<-function(x,y) sqrt(mean((x-y)^2))

ni<-10
b<-rnorm(ni) #difficulties
a<-exp(rnorm(ni,mean=0,sd=.25)) #discriminations
c<-runif(ni,min=0,max=0)

th<-rnorm(100)
est<-numeric()

for (i in 1:length(th)) {
    ##we'll use th to simulate data
    p<-get_pr(th[i],a,b,c)
    resp<-rbinom(length(p),1,p)
    ##but now we won't use th (just resp) for estimation
    est[i]<-optim(0,f,method='Brent',lower=-10,upper=10,a=a,b=b,c=c,resp=resp)$par
}
plot(th,est)
rmse(th,est)

##what factors in the simulation can you use to change the quality (via rmse) of the estimates?
    
