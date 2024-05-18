like<-function(theta,resp,beta) {
    kern<-theta-beta
    pv<-1/(1+exp(-1*kern))
    prod(pv^resp*(1-pv)^(1-resp))
}

##1. Suppose that we have a test scaled with the Rasch model whose first 3 items have known difficulties -1, 0, and 1.5. An examinee with ability theta got the first item right, the second item right, and the third item wrong. Can you write the likelihood of observing this sequence of item responses as a function of theta? 

resp<-c(1,1,0)
beta<-c(-1,0,1.5)
f<-function(theta) like(theta,resp,beta)


##2. Can you plot this as a function of theta? 
Vectorize(like,"theta")->f
th<-seq(-4,4,length.out=1000)
plot(th,f(th,resp=resp,beta=beta),type="l")

##3. If theta=0.5, what is the likelihood of that response sequence? 
like(0.5,resp=resp,beta=beta)

##4. If theta=0.5, what is the most likely response sequence given the known item difficulties? 
like(0.5,resp=c(0,0,0),beta=beta)
like(0.5,resp=c(1,0,0),beta=beta)
like(0.5,resp=c(0,1,0),beta=beta)
like(0.5,resp=c(0,0,1),beta=beta)
like(0.5,resp=c(1,1,0),beta=beta) #winner!
like(0.5,resp=c(1,0,1),beta=beta)
like(0.5,resp=c(0,1,1),beta=beta)
like(0.5,resp=c(1,1,1),beta=beta)

##5. At what value of theta does a response sequence of 1-1-0 (that is: they got the first and second items right and the third item wrong) become more likely than a response sequence of 1-0-0?
plot(NULL,xlim=c(-4,4),ylim=c(0,.5))
th<-seq(-4,4,length.out=1000)
plot(th,f(th,resp=resp,beta=beta),type="l")
lines(th,f(th,resp=c(1,1,0),beta=beta),col="red")
lines(th,f(th,resp=c(1,0,0),beta=beta),col="black")
##looks to be about 0, which is sufficient for our purposes here. 

##6. Returning to questions 1 and 2, can you plot the “test information” as a function of theta 
info<-function(theta,beta) { #note that this DOES NOT depend upon the responses
    kern<-theta-beta
    p<-1/(1+exp(-1*kern))
    q<-1 - p
                                        #p.prime<- exp(-1*kern)/(1+exp(-1*kern))^2 #(see Eqn 2-6 in Lord).
                                        #sum(p.prime^2/(q*p))
    sum(p*q)
}
Vectorize(info,"theta")->f
th<-seq(-4,4,length.out=1000)
plot(th,f(th,beta=beta),type="l")

##7. Where is the function in #6 maximized? What do you think this implies? 
optim(0,fn=function(x,beta) -1*f(x,beta),method="Brent",beta=beta,lower=-5,upper=5)


