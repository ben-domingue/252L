##now items may/may not have common discriminations.
##discriminations lead to different patternings in resulting data, so we'll look at something different

source("https://raw.githubusercontent.com/ben-domingue/252L/master/cI/functions.R") ##you'll need to source this 

N<-10000
n<-40
##generate theta and pars
rnorm(N)->theta
##first with rasch model
cbind(1,rnorm(n),0)->pars
sim_fun(theta=theta,pars=pars)->resp.rasch
#then with 2pl
cbind(exp(rnorm(n,sd=.5)),rnorm(n),0)->pars
sim_fun(theta=theta,pars=pars)->resp.2pl


Qfun<-function(resp) {
    rowMeans(resp)->rm
    apply(resp,2,function(x) cor(x,rm))
}
Q<-list()
Qfun(resp.rasch)->Q$r
Qfun(resp.2pl)->Q$two

#now let's blind ourselves to the true stuff we don't normally get.
rm("theta")
rm("pars") 

f<-function(resp) {
    Qfun(resp)->QQ
    est_fun(resp,mod="Rasch")->est.rasch
    est_fun(resp,mod="2PL")->est.2pl
    list(r=est.rasch,two=est.2pl)->L
    for (ii in 1:length(L)) {
        L[[ii]]->est
        Ql<-list()
        for (i in 1:50) {
            rnorm(N,mean=est$hyp[1],sd=sqrt(est$hyp[2]))->th.sim
            sim_fun(theta=th.sim,pars=est$coef)->resp.sim
            Qfun(resp.sim)->Ql[[i]]
        }
        do.call("rbind",Ql)->mat
        stack(data.frame(mat))->xv
        matrix(1:ncol(resp),nrow(mat),ncol(mat),byrow=TRUE)->tmp
        stack(data.frame(tmp))->yv
        data.frame(cor=xv[,1],item=yv[,1])->tmp
        #these were item-by-item plots that had some value but were more difficult to follow.
        #boxplot(cor~item,tmp,ylim=c(.2,.6))
        #lines(QQ,col="red")
        hist(tmp[,1],xlim=c(0,1),breaks=50,freq=FALSE,xlab="correlation",main=ifelse(ii==1,"Analysis=Rasch","Analysis=2PL"))
        lines(density(QQ),col="red",lwd=4)
    }
}
##we're going to look at how observed correlations between item-total look compared to simulated correlations
par(mfrow=c(2,2),mgp=c(2,1,0))
f(resp.rasch) #here we have a match between dgp and assumed model on left
legend("topright",bty='n',"Generative=Rasch")
f(resp.2pl) #here the match is on right.
legend("topright",bty='n',"2PL")

##note that the 2PL can capture the rasch features!
