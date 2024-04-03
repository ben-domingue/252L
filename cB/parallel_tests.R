#We're going to explore a few issues here.
#1. how does cronbach's alpha compare to reliability coefficients based on different split halves?
#2. how do adjusted split-half correlations compare to cronbach's alpha

#####################################################################
##some helper functions
kr20<-function(resp) { #should square this with the definition we saw in class
    k<-ncol(resp)
    p<-colMeans(resp,na.rm=TRUE)
    q<-1-p
    o<-rowSums(resp,na.rm=TRUE)
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
set.seed(512373)
dataset <- redivis::user("datapages")$dataset("item_response_warehouse",version='v5.0')
df <- dataset$table("chess_lnirt")$to_data_frame() ##https://redivis.com/datasets/as2e-cv7jb41fd/tables/35se-d5dmd2xn9
resp<-irw::long2resp(df)
resp$id<-NULL

rel<-list()
for (i in 1:1000) par_test(resp)->rel[[i]] #what just happened here?
do.call("rbind",rel)->rel
plot(density(rel[,1]),col="red",xlim=c(0.5,1),xlab="correlation of parallel forms")
lines(density(rel[,2]),col="black") #what is difference between black and red curves?
kr20(resp)->alph
abline(v=alph) #vertical black line at estimate of kr20/cronbach's alpha


##q. how does this make you feel about cronbach's alpha?
