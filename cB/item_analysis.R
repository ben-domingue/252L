##GOAL: what do CTT item fit statistics look like in item responses datasets with qualitatively different features?

out<-list() #lists are really useful!! this just initializes this one so that we can use it later. read more @ http://www.r-tutor.com/r-introduction/list

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

##let's start with an empirical dataset
resp1<-read.table("https://github.com/ben-domingue/252L/raw/master/data/emp-rasch.txt",header=FALSE)
out[[1]]<-item_analysis(resp1) #i'm passing the resp1 object to the item_analysis function. remind yourself what the item_analysis function will return and check your intuition against the next line
out[[1]] #q. why the double brackets? [hint: it has to do with the fact that "out" is a list]
##q. what do we have here? what do you think?

##code to simulate item response data. just run this block for a moment, don't feel the need to look at it in detail
#############################################################
##feel free to ignore between the above line and the similar below line
ni<-ncol(resp1)
np<-nrow(resp1)
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
resp2<-ifelse(pr>test,1,0) #what bit of magic was this?
#############################################################
out[[2]]<-item_analysis(resp2)

##now a new empirical dataset
resp3<-read.table("https://github.com/ben-domingue/252L/raw/master/data/emp-reading-3pl-gpcm.txt",header=TRUE)
head(resp3) ##q. what do you think about the wisdom of applying item_analysis to resp3? does resp3 have any distinguishing features vis-a-vis resp1 or resp2?
out[[3]]<-item_analysis(resp3) 

##q. what if you cut the non-dichotomous items out of this data?
apply(resp3,2,function(x) length(table(x)))->ncat
ncat #what is this?
resp3[,ncat==2]->resp4
out[[4]]<-item_analysis(resp4)

##let's plot the ctt values to see what we can see
par(mfrow=c(4,2),mgp=c(2,1,0),mar=c(3,3,1,1))
pf<-function(x) {
    hist(x[,1],xlab="p-values",main='',xlim=c(0,max(1,max(x[,1]))))
    plot(density(x[,2]),xlim=c(0,1),xlab="item-total correlations",main="")
}
lapply(out,pf) #lapply is a dear friend of mine!
##q. what do you note about the ctt statistics for the different datasets
##q. how does the simulated data compare to our first empirical dataset?

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
