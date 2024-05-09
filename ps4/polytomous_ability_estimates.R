##make probabilities
getprob<-function(model,th,b,a) {
    pcm<-function(th,b,a) {
        K<-length(b)+1
        psi<-list()
        psi[[1]]<-rep(1,length(th))
        for (k in 1:(K-1)) {
            kern<-k*th-sum(b[1:k])
            psi[[k+1]]<-exp(a*kern)
        }
        psi<-do.call("cbind",psi)
        den<-rowSums(psi)
        p<-psi/den
    }
    grm<-function(th,b,a) { 
        invlogit<-function(z) 1/(1+exp(-z))
        K<-length(b)+1
        pr<-list()
        for (i in 1:(K-1)) pr[[i]]<-invlogit(a*(th-b[i]))
        pr<-do.call("cbind",pr)
        pr<-cbind(1,pr,0)
        p<-list()
        for (i in 1:K) p[[i]]<-pr[,i]-pr[,i+1]
        p<-do.call("cbind",p)
        p
    }
    srm<-function(th,b,a) {
        invlogit<-function(z) 1/(1+exp(-z))
        K<-length(b)+1
        pr<-list()
        for (i in 1:(K-1)) pr[[i]]<-invlogit(a*(th-b[i]))
        pr<-do.call("cbind",pr)
        p<-list()
        for (i in 1:K) {
            if (i==1) tmp<-1-pr[,1]
            if (i==K) tmp<-apply(pr,1,prod)
            if (i>1 & i<K) tmp<-apply(pr[,1:(i-1),drop=FALSE],1,prod)*(1-pr[,i])
            p[[i]]<-tmp
        }
        p<-do.call("cbind",p)
        p
    }
    p<-do.call(model,args=list(th=th,b=b,a=a)) 
    p
}

##Functions to simulate data from models
simresp<-function(model,th,b,a) {
    p<-getprob(model,th=th,b=b,a=a)
    resp<-numeric()
    for (i in 1:length(th)) resp[i]<-which(rmultinom(1,1,p[i,])[,1]>0)-1
    resp
}

##Functions to estimate models
##estimate grm
estfun<-function(model,resp,th) {
    f<-function(pars,resp,th,model) {
        a<-pars[1]
        b<-pars[-1]
        p<-getprob(model,th=th,b=b,a=a)
        z<-mapply(function(i,j) p[i,j],1:nrow(p),resp+1)
        -1*sum(log(z))
    }
    K<-length(unique(resp[!is.na(resp)]))
    inits<-c(1,seq(-1,1,length.out=K-1))
    optim(inits,fn=f,resp=resp,th=th,model=model)$par
}

set.seed(103010)
b0<- -1
b1<-0
a<-1
N<-5000
th<-sort(rnorm(N))
b<-c(b0,b1)
resp<-simresp('pcm',th,b,a) #this is just a vector of responses for one item
estfun('pcm',resp,th) #here are a and then b parameters. those estimates are pertty accurate! 

##Now let's take that data and those abilities and estimate the GRM!
estfun('grm',resp,th) 
