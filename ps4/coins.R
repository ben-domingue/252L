## require("stats4");

## ## sample data from Do and Batzoglou
## ds<-data.frame(heads=c(5,9,8,4,7),n=c(10,10,10,10,10),
##     coin=c("B","A","A","B","A"),weight_A=1:5*0)

## ## "baby likelihood" for a single observation
## llf <- function(heads, n, theta) {
##   comb <- function(n, x) { #nCr function
##     return(factorial(n) / (factorial(x) * factorial(n-x)))
##   }
##   if (theta<0 || theta >1) { # probabilities should be in [0,1]
##     return(-Inf);
##   }
##   z<-comb(n,heads)* theta^heads * (1-theta)^(n-heads);
##   return (log(z))
## }

## ## the "E-M" likelihood function
## em <- function(theta_A,theta_B) {
##   # expectation step: given current parameters, what is the likelihood
##   # an observation is the result of tossing coin A (vs coin B)?
##   ds$weight_A <<- by(ds, 1:nrow(ds), function(row) {
##         llf_A <- llf(row$heads,row$n, theta_A);
##         llf_B <- llf(row$heads,row$n, theta_B);

##     return(exp(llf_A)/(exp(llf_A)+exp(llf_B)));
##   })

##   # maximisation step: given params and weights, calculate likelihood of the sample
##   return(- sum(by(ds, 1:nrow(ds), function(row) {
##     llf_A <- llf(row$heads,row$n, theta_A);
##     llf_B <- llf(row$heads,row$n, theta_B);

##     return(row$weight_A*llf_A + (1-row$weight_A)*llf_B);
##   })))
## }

## est<-mle(em,start = list(theta_A=0.6,theta_B=0.5), nobs=NROW(ds))




####################################################

EM<-function(dat,init=c(.5,.5),N=10) {
    Estep<-function(n,N,p1,p2) {
        p1^n*(1-p1)^(N-n)->z1
        p2^n*(1-p2)^(N-n)->z2
        z1/(z1+z2)->pr1
        z2/(z1+z2)->pr2
        c(pr1*n,pr1*(N-n),pr2*n,pr2*(N-n))
    }
    Vectorize(Estep,vectorize.args="n")->Estep
    Mstep<-function(mat) {
        rowSums(mat)->rs
        c(rs[1]/(rs[1]+rs[2]),rs[3]/(rs[3]+rs[4]))
    }
    ##
    init -> new.est
    c(10,10)->old.est
    counter<-1
    ##
    while (max(abs(old.est-new.est))>.001) {
        new.est->old.est
        Estep(dat,p1=new.est[1],p2=new.est[2],N=N)->mat
        Mstep(mat)->new.est
        counter<-counter+1
    }
    new.est
}
EM(dat=c(5,9,8,4,7),init=c(.52,.5)) #http://www.nature.com/nbt/journal/v26/n8/full/nbt1406.html

N.flips.per.run<-3
N.runs<-10
sim<-list()
for (N.runs in c(2,5,10,15)) for (N.flips.per.run in c(2,4,10,25)) {
    out<-list()
    for (i in 1:300) {
        runif(2)->p
        sort(p)->p
        dat<-numeric()
        for (ii in 1:N.runs) {
            sample(1:2,1)->index
            rbinom(1,size=N.flips.per.run,prob=p[index])->dat[ii]
        }
        EM(dat,init=c(.4,.6),N=N.flips.per.run)->est
        c(p,est)->out[[i]]
    }
    do.call("rbind",out)->mat
    c(N.runs,N.flips.per.run,mean((mat[,1]-mat[,3])^2)+mean((mat[,2]-mat[,4])^2))->sim[[paste(N.runs,N.flips.per.run)]]
}
do.call("rbind",sim)->sim

plot(sim[,3],xaxt="n",xlab="",ylab="mse")
strsplit(rownames(sim)," ")->txt
do.call("rbind",txt)->txt
mtext(side=1,line=0.2,at=1:nrow(sim),txt[,1])
mtext(side=1,line=1,at=1:nrow(sim),txt[,2])

    

    
