##This is a function that will embed local dependencies in item responses (in particular, it's a kind of 'hot hand' effect)
sim_ld<-function(N=1000,
                 n=10,
                 per=.1 #this will be a parameter such that your performance on the last item influences your performance on this item
                 ) {
    th<-rnorm(N)
    b<-rnorm(n)
    getp<-function(th,b) 1/(1+exp(b-th))
    resp<-matrix(NA,N,n)
    delta<-numeric()
    for (i in 1:N) {
        person.par<-runif(1,-1*per,per)
        for (j in 1:n) {
            getp(th[i],b[j])->p
            if (j>1 && resp[i,j-1]==1 & person.par>0) p+person.par*(1-p)->p #so if you got the last item right, then your chance on this one is improved by person.par times 1-p
            if (j>1 && resp[i,j-1]==0 & person.par<0) p+person.par*p->p #so if you got the last item wrong, then your pr on this one is decrease by person.par*p (person.par is negative here)
            rbinom(1,1,p)->y
            y->resp[i,j]
        }
        person.par->delta[i]
    }
    1:ncol(resp)->colnames(resp)
    list(true=list(b=b,th=th,delta=delta),resp=resp)
}
sim_ld()->resp

##the game will be the same as last time: what damage does this violation of the assumptions do to our understanding post-estimation.
##but, before you start, ask yourself where you expect to see problems! 
##big question: how big of a problem does this seem to you? 
