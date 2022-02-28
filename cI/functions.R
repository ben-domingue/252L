est_fun<-function(resp,mod,person=FALSE) {
    ##this will estimate the relevant Rasch model
    ##just a wrapper for call to mirt
    library(mirt)
    if (mod=="Rasch") {
        mirt(resp,1,itemtype=rep("Rasch",ncol(resp)),method="EM",technical=list(NCYCLES=5000))->mirt.mod
    }
    if (mod=="2PL") {
        s<-paste("F=1-",ncol(resp),"
        PRIOR = (1-",ncol(resp),", a1, lnorm, 0.2, 0.2)",sep="") #note i'm imposing priors on parameters which may be poorly behaved.
        mirt.model(s)->model
        mirt(resp,model,itemtype=rep("2PL",ncol(resp)),method="EM",technical=list(NCYCLES=10000))->mirt.mod
    }
    if (mod=="3PL") {
        s<-paste("F=1-",ncol(resp),"
            PRIOR = (1-",ncol(resp),", a1, lnorm, 0.2, 0.2),(1-",ncol(resp),", g, norm, -1.5, 1)",sep="") #-1.5
                                        #PRIOR = (1-",ncol(resp),", a1, lnorm, 0.2, 0.2),(1-",ncol(resp),", g, norm, 0, 1)",sep="")
        mirt.model(s)->model
        mirt(resp,model,itemtype=rep("3PL",ncol(resp)),method="EM",technical=list(NCYCLES=10000))->mirt.mod
    }
    coef(mirt.mod)->co
    co[-length(co)]->co
    do.call("rbind",co)->co
    coef(mirt.mod)[length(coef(mirt.mod))]->hyp
    if (person) {
        fscores(mirt.mod,response.pattern=resp,method="WLE")->th
        th[,ncol(resp)+1]->theta
    } else NULL->theta
    list(coef=co[,1:3],hyp=hyp[[1]],theta=theta)
}

sim_fun<-function(theta,pars) {
    ##this will simulate item response data based on the relevant user inputs (abilities and item parameters)
    ##note that parts is a matrix of item parameters (so, even for the 1pl, we use the 3pl formulation)
    length(theta)->n.people
    nrow(pars)->n.items
    #
    matrix(theta,n.people,n.items,byrow=FALSE)->th
    matrix(pars[,1],n.people,n.items,byrow=TRUE)->a
    matrix(pars[,2],n.people,n.items,byrow=TRUE)->b
    matrix(pars[,3],n.people,n.items,byrow=TRUE)->c
    exp(a*(theta+b))->kern
    kern/(1+kern)->kern
    c+(1-c)*kern -> pv
    matrix(runif(length(theta)*nrow(pars)),n.people,n.items)->test
    ifelse(pv>test,1,0)->resp
    1:ncol(resp)->colnames(resp)
    resp
}
