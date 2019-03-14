resp->hold

##i used this code to generate a picture for class
hold->resp
mod <- mirt(resp, itemtype="gpcmIRT",1)
th<-seq(-4,4,length.out=1000)
testinfo(mod,th)->ti
par(mgp=c(2,1,0))
plot(th,ti,type="l",lwd=2,ylab="test information",xlab="theta")
##
for (i in 1:ncol(resp)) ifelse(resp[,i]>2,2,resp[,i])->resp[,i]
mod <- mirt(resp, itemtype="gpcmIRT",1)
testinfo(mod,th)->ti
lines(th,ti,col="gray",lwd=2,lty=2)
##
hold->resp
for (i in 1:ncol(resp)) ifelse(resp[,i]>1,1,0)->resp[,i]
mod <- mirt(resp, itemtype="2PL",1)
testinfo(mod,th)->ti
lines(th,ti,col="red",lwd=2,lty=2)
##
hold->resp
for (i in 1:ncol(resp)) ifelse(resp[,i]>0,1,0)->resp[,i]
mod <- mirt(resp, itemtype="2PL",1)
testinfo(mod,th)->ti
lines(th,ti,col="blue",lwd=2,lty=2)
##
hold->resp
for (i in 1:ncol(resp)) ifelse(resp[,i]==max(resp[,i]),1,0)->resp[,i]
mod <- mirt(resp, itemtype="2PL",1)
testinfo(mod,th)->ti
lines(th,ti,col="green",lwd=2,lty=2)
##
legend("topright",bty="n",c("original","34->1",">1 ->1",">0 ->1","hi -> 1"),fill=c("black","gray","red","blue","green"))
