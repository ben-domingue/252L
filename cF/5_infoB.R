set.seed(12311)
read.table("https://raw.githubusercontent.com/ben-domingue/252L/master/data/emp-reading-3pl-gpcm.txt",header=TRUE)->resp ##might take a while, could also download directly and then read.table locally
resp[rowSums(is.na(resp))==0,]->resp
resp[1:5000,]->resp
#first just the constructed response items
grep("^cr",names(resp))->index
resp[,index]->resp

apply(resp,2,table)

library(mirt)
mod<-list()
mod$pcm <- mirt(resp, itemtype="Rasch",1)
mod$grm<- mirt(resp, 1,itemtype="graded") 
mod$gpcm <- mirt(resp, itemtype="gpcm",1)
resp->resp01
for (i in 1:ncol(resp01)) ifelse(resp01[,i]>0,1,0)->resp01[,i]
mod$`2pl.lo` <- mirt(resp01, itemtype="2PL",1)
resp->resp01
for (i in 1:ncol(resp01)) ifelse(resp01[,i]>1,1,0)->resp01[,i]
mod$`2pl.hi` <- mirt(resp01, itemtype="2PL",1)

fun<-function(mod,i,th) {
    extract.item(mod,i)->extr
    iteminfo(extr,th)
}
cols<-c("black","red","green","blue","blue")
lty<-c(1,1,1,2,3)
par(mfrow=c(3,5),mar=c(3.3,3.3,1,1),mgp=c(2,1,0))
th<-matrix(seq(-4,4,length.out=1000))
for (i in 1:ncol(resp)) {
    lapply(mod,fun,i=i,th=th)->out
    max(unlist(out))->M
    plot(NULL,xlim=range(th),ylim=c(0,M),xlab="",ylab="")
    for (j in 1:length(out)) lines(th,out[[j]],col=cols[j],lty=lty[j],lwd=2)
}
legend("topright",c("pcm","grm","gpcm","2pl.lo","2pl.hi"),lty=lty,col=cols,lwd=1,bty="n")
##test info
lapply(mod,testinfo,Theta=th)->out
max(unlist(out))->M
plot(NULL,xlim=range(th),ylim=c(0,M),xlab="",ylab="")
for (j in 1:length(out)) lines(th,out[[j]],col=cols[j],lty=lty[j],lwd=2)

##note: i'm not asserting that the model which provides maximal info is the one we should use. only comparing the differences in info that we get from application of different models.
