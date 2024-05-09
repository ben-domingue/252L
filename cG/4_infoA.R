##we're going to simulate data under a few different models and then look at test information when we estimate different models. so in a given panel of the resulting plot, the generating model is the one noted at the bottom and the curves represent different recovery models.

sim.gpcm<-function(N, #people
                   n, #items
                   k, #categories
                   a=NULL
                   ) {
    theta<-rnorm(N)
    sort(theta)->theta
    resp<-list()
    if (is.null(a)) rep(1,n)->a
    for (i in 1:n) {
        m<--1
        while (m<.1) {
            rnorm(k-1,sd=.5)->cat.thr
            min(diff(cat.thr))->m
        }
        sort(cat.thr)->cat.thr
        psi<-list()
        psi[[1]]<-rep(1,length(theta))
        for (j in 1:(k-1)) {
            j*theta-sum(cat.thr[1:j]) -> kern
            exp(a[i]*kern)->psi[[j+1]]
        }
        do.call("cbind",psi)->psi
        rowSums(psi)->den
        psi/den->pr
        resp.tmp<-numeric()
        for (ii in 1:length(theta)) {
            rmultinom(1,1,pr[ii,])->out
            which(out[,1]>0)-1->resp.tmp[ii]
        }
        resp.tmp->resp[[i]]
    }
    do.call("cbind",resp)->resp
    print(mean(cor(cbind(theta,resp))[-1,1]))
    1:ncol(resp)->colnames(resp)
    resp
}

sim.grm<-function(N, #people
                  n, #items
                  k, #categories
                  a=NULL
                  ) {
    theta<-rnorm(N)
    sort(theta)->theta
    resp<-list()
    if (is.null(a)) rep(1,n)->a
    for (i in 1:n) {
        m<--1
        while (m<.1) {
            rnorm(k-1,sd=1)->cat.thr
            min(diff(cat.thr))->m
        }
        sort(cat.thr)->cat.thr
        pr<-list()
        for (j in 1:length(cat.thr)) {
            exp(a[i]*(theta-cat.thr[j]))->kern
            kern/(1+kern)->pr[[j]]
        }
        c(list(rep(1,length(theta))),pr,list(rep(0,length(theta))))->pr
        do.call("cbind",pr)->pr
        tmp<-list()
        for (j in 1:(ncol(pr)-1)) pr[,j]-pr[,j+1]->tmp[[j]]
        do.call("cbind",tmp)->pr
        resp.tmp<-numeric()
        for (ii in 1:length(theta)) {
            rmultinom(1,1,pr[ii,])->out
            which(out[,1]>0)-1->resp.tmp[ii]
        }
        resp.tmp->resp[[i]]
    }
    do.call("cbind",resp)->resp
    print(mean(cor(cbind(theta,resp))[-1,1]))
    1:ncol(resp)->colnames(resp)
    resp
}



est.plot<-function(resp) {
    library(mirt)
    mod<-list()
    mod$pcm <- mirt(resp, itemtype="Rasch",1)
    mod$grm<- mirt(resp, 1,itemtype="graded") 
    mod$gpcm <- mirt(resp, itemtype="gpcm",1)
    resp->resp01
    for (i in 1:ncol(resp01)) ifelse(resp01[,i]>0,1,0)->resp01[,i]
    mod$`2pl.lo` <- mirt(resp01, itemtype="2PL",1)
    resp->resp01
    for (i in 1:ncol(resp01)) ifelse(resp01[,i]>1,1,0)->resp01[,i]
    mod$`2pl.hi` <- mirt(resp01, itemtype="2PL",1)
                                        #
    ## fun<-function(mod,i,th) {
    ##     extract.item(mod,i)->extr
    ##     iteminfo(extr,th)
    ## }
    lapply(mod,coef,IRTpars=TRUE,simplify=TRUE)->co
                                        #
    cols<-c("black","red","green","blue","blue")
    lty<-c(1,1,1,2,3)
    th<-matrix(seq(-4,4,length.out=1000))
    lapply(mod,testinfo,Theta=th)->out
    plot(NULL,xlim=range(th),ylim=c(0,10),xlab="",ylab="")
    for (j in 1:length(out)) {
        lines(th,out[[j]],col=cols[j],lty=lty[j])
        co[[j]][[1]]->mat
        if (j>3) mat[,1:2]->mat
        (ncol(mat)-1):1->vals
        #mtext(side=3,at=mat[i,-1],vals[1:(ncol(mat)-1)],col=cols[j],line=0+.5*j,cex=.7)
        legend("topright",c("pcm","grm","gpcm","2pl.lo","2pl.hi"),lty=lty,col=cols,lwd=1,bty="n")
    }
    NULL
}

par(mfcol=c(2,2),mar=c(3,3,4,1),mgp=c(2,1,0),oma=c(1,1,2,1))
                                        #
sim.gpcm(N=500,n=5,k=5)->resp
est.plot(resp)
mtext(side=1,line=2,"pcm")
                                        #
sim.gpcm(N=500,n=5,k=5,a=sort(runif(10,min=.75,max=1.3)))->resp
est.plot(resp)
mtext(side=1,line=2,"gpcm")
                                        #
sim.grm(N=5000,n=5,k=5,a=rep(1,10))->resp
est.plot(resp)
mtext(side=1,line=2,"grm, a=1")
                                        #
sim.grm(N=5000,n=5,k=5,a=sort(2*runif(10,min=.75,max=1.3)))->resp
est.plot(resp)
mtext(side=1,line=2,"grm")
