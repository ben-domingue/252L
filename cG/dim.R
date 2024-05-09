##This is a function that will generate item response data based on a combination of unidimensional and  multidimensional models.
sim_md<-function(N=1000, #number of people
                 n=10, #number of items (we will simulate 2n items)
                 r=.5, #correlation of MD abilities
                 A=1 #the max loading on the second dimension, all loadings on first dimension are 1
                 ) {
    library(MASS)
    th<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,r,r,1),2,2,byrow=TRUE))
                                        #make first n items unidimensional
    b1<-rnorm(n)
    kern<-outer(th[,1],b1,"+") #this just creates a matrix consisting where element i,j is th[i,1]+b1[j]
    kern<-exp(kern)
    pv<-kern/(1+kern)
    runif(N*n)->test
    ifelse(pv>test,1,0)->resp1
                                        #now second n items are multidimensional (if A>0) with increasing loadings on the second dimension
    a<-seq(0,A,length.out=n) 
    b2<-rnorm(n)
    mat<-list()
    for (i in 1:N) th[i,1]+a*th[i,2]+b2->mat[[i]] #this is where the multidimensional magic happens
    do.call("rbind",mat)->kern
    kern<-exp(kern)
    pv<-kern/(1+kern)
    runif(N*n)->test
    ifelse(pv>test,1,0)->resp2
    resp<-cbind(resp1,resp2) #so first half of items will be unidimensional and second half multidimensional
    1:ncol(resp)->colnames(resp)
    list(true=list(a=a,b=c(b1,b2),th=th),resp=resp)
}
sim_md()->resp
##note: this 'resp' matrix does not contain information about where it 'came from'.
##in particular, there is nothing about it that tells you it came from a multidimensional model! you have to figure that part out.

##Using this function, examine parameter recovery for the item parameters. my work is in dim-bd-work.R if you would like a guide.
##How does this depend on choice of r?
##How does this depend on choice of item response model?
##i would suggest focusing on the rasch and 2pl [confirm that i am not simulating any guessing]

##On fit statistics:
##They may or may not be pick up the ways in which things are going wrong here.
##If you have some insight into the ways the true DGP differs from the model-assumed DGP, you might have insight into fit statistics designed to detect such deviations.
