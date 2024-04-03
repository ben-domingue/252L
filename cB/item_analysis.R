##GOAL: what do CTT item fit statistics look like in item responses datasets with qualitatively different features?

##CTT item analysis
##this function will compute CTT item statistics for a generic item response matrix. it leverages what we had from correlations.R and adds calculation of p-values to that.
item_analysis<-function(resp) { #'resp' is just a generic item response matrix, rows are people, columns are items
    pv<-colMeans(resp,na.rm=TRUE) #simple "p-values", which in psychometrics tends to just mean the mean number of points for an item
    r.xt<-numeric() #initializing a vector
    rowSums(resp,na.rm=TRUE)->ss #these are the sum scores/observed scores
    for (i in 1:ncol(resp)) {
        cor(ss,resp[,i],use='p')->r.xt[i] #this is the correlations between the i-th item (resp[,i]) and the total score (ss)
    }
    cbind(pv,r.xt) #returning a matrix consisting of the p-values and the item/total correlations
}


##we're going to work with lists. lists are really useful!! this just initializes this one so that we can use it later. read more @ http://www.r-tutor.com/r-introduction/list
respL<-list() 
dataset <- redivis::user("datapages")$dataset("item_response_warehouse",version='v5.0')
dataset_names <- c("4thgrade_math_sirt", "chess_lnirt", "dd_rotation")
##https://redivis.com/datasets/as2e-cv7jb41fd/tables/769d-bmjr8xzbq
##https://redivis.com/datasets/as2e-cv7jb41fd/tables/35se-d5dmd2xn9
##https://redivis.com/datasets/as2e-cv7jb41fd/tables/jbqa-8k5dtkw7j
for (nm in dataset_names) respL[[nm]] <- dataset$table(nm)$to_data_frame()
respL<-lapply(respL,irw::long2resp)
for (i in 1:length(respL)) respL[[i]]$id<-NULL

##let's now add a simulated dataset
##code to simulate item response data. just run this block for a moment, don't feel the need to look at it in detail
#############################################################
##feel free to ignore between the above line and the similar below line
ni<-25
np<-1000
set.seed(12311) #this is to ensure that we all draw the same random numbers downstream
th<-rnorm(np)
th.mat<-matrix(th,np,ni,byrow=FALSE) #these are the true abilities. we don't observe them, which will be a real problem for us downstream. but we'll not worry about that today. 
a<-1 ##for reasons we'll get into in the coming weeks, let's start with a slope equal to 1
b<-rnorm(ni)
b.mat<-matrix(b,np,ni,byrow=TRUE) #these are the item difficulties, also something we don't typically observe with empirical data
inv_logit<-function(x) exp(x)/(1+exp(x))
pr<-inv_logit(a*th.mat+b.mat) ##the probability of a correct response
##watch closely now
test<-matrix(runif(ni*np),np,ni) #what do we have here?
respL$sim<-ifelse(pr>test,1,0) #what bit of magic was this? it's a nice way of using the uniform distribution to generate bernoulli random variables
#############################################################

out<-lapply(respL,item_analysis)

##let's plot the ctt values to see what we can see
par(mfrow=c(4,2),mgp=c(2,1,0),mar=c(3,3,1,1))
pf<-function(nm,out) {
    x<-out[[nm]]
    hist(x[,1],xlab="p-values",main='',xlim=c(0,max(1,max(x[,1]))))
    legend("topleft",bty='n',legend=nm)
    plot(density(x[,2]),xlim=c(0,1),xlab="item-total correlations",main="")
}
lapply(names(out),pf,out) #lapply is a dear friend of mine!
##q. what do you note about the ctt statistics for the different datasets
##q. how could you change the distributions for the simulated data?

##time permitting:
##q. can you rewrite item_analysis() so that it computes the correlations between an item after removing that item from the sum score?
##q. can you use the biserial correlation coefficient instead of the raw correlations [note that this wouldn't work for resp3]. 



biserial_cor<-function(x,y) { #https://www.statisticshowto.com/point-biserial-correlation/
    y1<-mean(y[x==1])
    y0<-mean(y[x==0])
    p<-mean(x)
    q<-1-p
    s<-sd(y)
    val<-qnorm(p)
    yy<-dnorm(val)
    ##
    (y1-y0)*(p*q/yy)/s
}
