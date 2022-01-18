#We're going to explore a few issues here.
#1. how does cronbach's alpha compare to reliability coefficients based on different split halves?
#2. how do adjusted split-half correlations compare to cronbach's alpha

#####################################################################
##some helper functions
kr20<-function(resp) { #should square this with the definition we saw in class
    k<-ncol(resp)
    p<-colMeans(resp,na.rm=TRUE)
    q<-1-p
    o<-rowSums(resp)
    (k/(k-1))*(1-sum(p*q)/var(o))
}
##this is a rewrite of the above for the general case, but we don't need here.
## cronbach_alpha<-function(resp) {
##     k<-ncol(resp)
##     v.i<-apply(resp,2,var)
##     o<-rowSums(resp)
##     (k/(k-1))*(1-sum(v.i)/var(o))
## }

par_test<-function(resp) {
    #this function just creates two split-halves of a test and computes:
    #1. the raw correlation of the two subtests.
    #2. the spearman-brown adjusted correlation.
    N<-ncol(resp)
    if (N %% 2>0) stop("please give me a test with even number of items")
    ##next several lines will split the test into two randomly chosen halves
    t1<-sample(N,N/2)
    t2<-(1:N)[!(1:N %in% t1)]
    t1<-resp[,t1]
    t2<-resp[,t2]
    ##now we'll process those halves
    r<-cor(rowSums(t1),rowSums(t2),use='p')
    c(r,2*r/(1+r))
}


#####################################################################
#simulated data
set.seed(512373)
read.table("https://raw.githubusercontent.com/ben-domingue/252L/master/data/rasch.txt")->resp
rel<-list()
for (i in 1:1000) par_test(resp)->rel[[i]] #what just happened here?
do.call("rbind",rel)->rel
plot(density(rel[,1]),col="red",xlim=c(0.5,1),xlab="correlation of parallel forms")
lines(density(rel[,2]),col="black") #what is difference between black and red curves?
kr20(resp)->alph
abline(v=alph) #vertical black line at estimate of kr20/cronbach's alpha
##q. what about portion of the distribution in black below the horizontal line? what do you make of these? [focus on assumptions that undergird everything]

##what if you considered empirical data? (from towards_irt.R)
resp<-read.table("https://github.com/ben-domingue/252L/raw/master/data/emp-rasch.txt",header=FALSE)
resp<-resp[rowSums(is.na(resp))==0,]
rel<-list()
for (i in 1:100) par_test(resp)->rel[[i]] #what just happened here?
do.call("rbind",rel)->rel
plot(density(rel[,1]),col="red",xlim=c(0.5,1))
lines(density(rel[,2]),col="black") #what is difference between black and red curves?
kr20(resp)->alph
abline(v=alph) #vertical black line at estimate of kr20/cronbach's alpha

##q. how does this make you feel about cronbach's alpha?

##########################
##if you have extra time and want to further explore:
##for simulated data (e.g., code block starting at line 23 of item_analysis.R), ask yourself the following questions [and feel free to track down answers!]
##q. how do these values change as you change ni and np in sample?
##q. what if you change the generating distribution of th?
##q. what if you change the value of "a" in line 31?
