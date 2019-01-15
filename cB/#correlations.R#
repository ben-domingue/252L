##let's start with an empirical dataset
resp<-read.table("https://github.com/ben-domingue/252L/raw/master/data/emp-rasch.txt",header=FALSE)
resp<-resp[rowSums(is.na(resp))==0,] #taking just those rows with no NAs

get.coors<-function(resp) {
    r.xt<-numeric() #initializing a numeric vector
    ss<-rowSums(resp) #these are the sum scores/observed scores of CTT
    for (i in 1:ncol(resp)) {
        r.xt[i]<-cor(ss,resp[,i]) #for any i, what is this?
    }


    for (i in 1:ncol(resp)) {
        ss<-rowSums(resp[,-i]) #these are the sum scores/observed scores of CTT
        r.xt[i]<-cor(ss,resp[,i]) #for any i, what is this?
    }


    r.xt
}
##q. why might we be interested in the correlation (r.xt) between an item response and the total score?

plot.fun<-function(resp) { #don't worry too much about the details here in the first pass.
    pv<-colMeans(resp,na.rm=TRUE)
    r.xt<-get.coors(resp)
    plot(pv,r.xt,pch=19)
    ##everything below here just goes into the red line
    loess(r.xt~pv)->m
    cbind(pv,fitted(m))->tmp
    lines(tmp[order(tmp[,1]),],col="red")
    NULL
}
plot.fun(resp)
##q. what do you notice about the relationship between item p-value and item-total correlation?
##q. could this be driven by the fact that the item is included in the calculation of the sum score (you can check this empirically if you like)

##q. could this be a weird feature of this empircal dataset? below is code to simulate an item response dataset. does it lead to the same kind of conclusion?
set.seed(12311)
th<-matrix(rnorm(50000),1000,50,byrow=FALSE)
diff<-matrix<-matrix(rnorm(10),1000,50,byrow=TRUE)
kern<- exp(th - diff)
pr<-kern/(1+kern)
test<-matrix(runif(1000),1000,50)
x2<-ifelse(pr>test,1,0)

plot.fun(x2)
