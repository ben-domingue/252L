resp<-read.table("https://raw.githubusercontent.com/ben-domingue/252L/master/data/nde_math_white_space.txt")
library(mirt)

##quality control
resp<-resp[,-22]
rs<-rowSums(resp)
resp<-resp[rs>0 & rs<ncol(resp),]

##rasch
m1<-mirt(resp,1,itemtype="Rasch")
th1<-fscores(m1,method="ML",full.scores.SE=TRUE)
th1e<-fscores(m1,method="EAP",full.scores.SE=TRUE)

plot(density(th1[,1]),col='red',lwd=2,ylim=c(0,.4))
lines(density(th1e[,1]),col='blue',lwd=2)
legend("topright",bty='n',fill=c("red","blue"),c("MLE","EAP"))

outfit <- function(resp,m,th) {
    co <- coef(m)
    co <- co[-length(co)]#why do i do this?
    pars <- do.call("rbind", co)
    n1 <- length(th)
    n2 <- length(pars)
    th <- matrix(th, n1, n2, byrow = FALSE)
    ea <- matrix(pars, n1, n2, byrow = TRUE)
    kern <- exp(th + ea)
    p<-kern/(1 + kern)
    q <- 1 - p
    z <- (resp - p)/sqrt(p * q)
    fit.u <- colMeans(z^2)
    fit.u
}
fit <- outfit(resp, m1,th1[,1])
sd(fit)
sqrt(2/nrow(resp))

##3pl
m2<-mirt(resp,1,itemtype="2PL",technical=list(NCYCLES=5000))
m3<-mirt(resp,1,itemtype="3PL",technical=list(NCYCLES=5000))
th2<-fscores(m2,method="ML",full.scores.SE=TRUE)
th3<-fscores(m3,method="ML",full.scores.SE=TRUE)
th3e<-fscores(m3,method="EAP",full.scores.SE=TRUE)

plot(density(th2[,1]),col='red',lwd=2,ylim=c(0,.4))
lines(density(th3[,1]),col='blue',lwd=2)
legend("topright",bty='n',fill=c("red","blue"),c("2PL","3PL"))

co<-coef(m3)[-length(coef(m3))]
co<-do.call("rbind",co)
summary(co[,3]) #mean 0.16
ii1<-which.max(co[,3])
ii2<-which.min(co[,3])
th<-seq(-4,4,length.out=1000)
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
for (ii in c(ii1,ii2)) {
    fity<-expected.item(extract.item(m3,ii),th)
    plot(th,fity,type='l',lwd=2,ylim=c(0,1))
    rs<-rowSums(resp)
    df<-data.frame(rs=rs,th=th3[,1],y=resp[,ii])
    df<-df[is.finite(df$th),]
    for (i in unique(df$rs)) {
        z<-df[df$rs==i,]
        xv<-mean(z$th)
        yv<-mean(z$y)
        cx<-.3+nrow(z)/15
        points(xv,yv,pch=19,col='red',cex=cx)
    }
    legend("bottomright",as.character(ii))
}

    
##rasch versus 3pl
par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
plot(th1[,1],th3[,1],pch=19,cex=.5,col='gray',xlab='Rasch',ylab='3pl',xlim=c(-4,4),ylim=c(-4,4))
legend("topleft",bty='n',title='MLE',legend='')
plot(th1e[,1],th3e[,1],pch=19,cex=.5,col='gray',xlab='Rasch',ylab='3pl',xlim=c(-4,4),ylim=c(-4,4))
legend("topleft",bty='n',title='EAP',legend='')
##
plot(th1,col='blue',pch=19,cex=.5,xlim=c(-4,4),ylim=c(0,1.5),xlab='theta',ylab='SE')
points(th3,col='red',pch=19,cex=.5)
legend("topright",bty='n',fill=c("blue","red"),c("Rasch","3PL"))
plot(th1e,col='blue',pch=19,cex=.5,xlim=c(-4,4),ylim=c(0,1.5),xlab='theta',ylab='SE')
points(th3e,col='red',pch=19,cex=.5)
legend("topright",bty='n',fill=c("blue","red"),c("Rasch","3PL"))
