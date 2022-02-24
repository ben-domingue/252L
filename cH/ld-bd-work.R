compare<-function(resp,itemtype="Rasch") {
    ##this will create a plot where we look at recovery of our ability estimates as a function of the "person.per" parameter
    library(mirt)
    mirt(resp$resp,1,itemtype=itemtype)->mod
    coef(mod)->co
    co[-length(co)]->co
    do.call("rbind",co)->co
    ##
    fscores(mod)->est
    data.frame(true=resp$true$th,del=resp$true$delta,est=est[,1])->df
    df[order(df$del),]->df
    plot(df$del,df$est-df$true)
    df$del->xv
    df$est-df$true->yv
    loess(yv~xv)->m
    lines(xv,m$fitted,lwd=3,col="red"); abline(h=0)
}
compare(resp)

sim_ld(per=0.025,n=25)->resp
compare(resp)


sim_ld(per=.3,n=25)->resp
compare(resp)

sim_ld(per=.7,n=25)->resp
compare(resp)
##REMINDER: we would never see 'person.par' in general. 
