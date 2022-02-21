set.seed(12101)
compare<-function(resp,itemtype="2PL") {
    ##let's first estimate a **unidimensional** model. this is wrong, of course.
    library(mirt)
    mirt(resp$resp,1,itemtype=itemtype)->mod
    coef(mod)->co
    co[-length(co)]->co
    do.call("rbind",co)->co
    ##now we create two plots
    ##first plot, looking at easiness: absolute bias in easiness estimates as a function of item position
    ##second plot, looking at discrimination: density of discrimination coefs separately for unidim items (black) and multidim items (red)
                                        #
    nrow(co)->n
    plot(abs(co[,2]-resp$true$b),col=c(rep("black",n/2),rep("red",n/2)),pch=19,ylab=c("abs(est easy - true easy)"))
    1:n->xv
    abs(co[,2]-resp$true$b)->yv
    loess(yv~xv)->m
    lines(xv,m$fitted)
                                        #
    plot(density(co[1:n/2,1]),col="black",xlab="density of discrimination",xlim=c(0,2))
    lines(density(co[(n/2+1):n,1]),col="red")
}

##How does this depend on choice of r?
par(mfrow=c(3,2),mgp=c(2,1,0),mar=c(3,3,1,1))
sim_md(r=0,n=30)->resp
compare(resp)
sim_md(r=1/3,n=30)->resp
compare(resp)
sim_md(r=2/3,n=30)->resp
compare(resp)
##Question: how much of this would you be able to know how to do if you didn't know the specific form of misfit here?


##How does this depend on choice of item response model used for parameter recovery (not data generation, be sure to keep the distinction between these two front and center)?
##i will focus on the rasch and 2pl [note that i am not simulating any guessing]
par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3,3,1,1))
sim_md(N=2000,n=50,A=2)->resp
compare(resp,itemtype="Rasch")
sim_md(N=2000,n=50,A=2)->resp
compare(resp,itemtype="2PL")


par(mfrow=c(4,2),mgp=c(2,1,0),mar=c(3,3,1,1))
sim_md(N=2000,r=0,n=50,A=2)->resp
compare(resp,itemtype="Rasch")
sim_md(N=2000,r=1,n=50,A=2)->resp
compare(resp,itemtype="Rasch")
sim_md(N=2000,n=50,A=2)->resp
compare(resp,itemtype="2PL")

## ##For fun let's now estimate the right model
sim_md(N=5000,r=0)->resp

mirt(resp$resp,2,"2PL")->mod
coef(mod)->co
co[-length(co)]->co
do.call("rbind",co)->co
plot(co[,1],pch=19,ylim=c(-2,2))
points(co[,2],pch=19,col="red") #subtle things are afoot here. if you know anything about factor analysis, the reason this isn't quite what you expect is related to the idea of "rotation"



