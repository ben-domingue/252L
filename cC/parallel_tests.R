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
    colSums(t1)->cs1
    colSums(t2)->cs2
    c(r,2*r/(1+r),abs(mean(rowSums(t1)-rowSums(t2)))) #last value is absolute mean difference in sum scores across the split halves. why might we care about this?
}


#####################################################################
#simulated data
read.table("https://raw.githubusercontent.com/ben-domingue/252L/master/data/rasch.txt")->resp


set.seed(512373)
rel<-list()
for (i in 1:1000) par_test(resp)->rel[[i]] #what just happened here?
do.call("rbind",rel)->rel
plot(density(rel[,1]),col="red",xlim=c(0.5,1))
lines(density(rel[,2]),col="black") #what is difference between black and red curves?
kr20(resp)->alph
abline(v=alph)

##first, why is alpha not acting as a lower bound?
plot(rel[,3],rel[,2],pch=19,cex=.8) #q. what theory am i testing here?
abline(h=alph) 
cor(rel[,3],rel[,2]) 
##fitting loess curves, don't worry too much about below chunk
loess(rel[,2]~rel[,3])->m
cbind(rel[,3],m$fitted)->tmp
tmp[order(tmp[,1]),]->tmp
lines(tmp,col="red",lwd=2)


##what if you considered empirical data? (from towards_irt.R)
resp<-read.table("https://github.com/ben-domingue/252L/raw/master/data/emp-rasch.txt",header=FALSE)
resp<-resp[rowSums(is.na(resp))==0,]
    
##for simulation (e.g., code block starting at line 21 of item_analysis.R), ask yourself the following questions [and feel free to track down answers!]
##how do these values change as you change ni and np in sample?
##what if you change the generating distribution of th?
##what if you change the value of a
