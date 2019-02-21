set.seed(12311)
read.table("https://raw.githubusercontent.com/ben-domingue/252L_winter2018/master/data/emp-reading-3pl-gpcm.txt",header=TRUE)->resp
resp[rowSums(is.na(resp))==0,]->resp
resp[1:5000,]->resp
#just the constructed response items
grep("^cr",names(resp))->index
resp[,index]->resp

##Let's look at different dichotomizations for just a single item, #2
library(mirt)
mod <- mirt(resp, itemtype="gpcmIRT",1)
extr.2 <- extract.item(mod, 2)
Theta <- matrix(seq(-4,4, by = .1))
info.2 <- iteminfo(extr.2, Theta)
plot(Theta,info.2,type="l")
for (i in 1:3) {
    resp->tmp
    ifelse(tmp[,2]>=i,1,0)->tmp[,2]
    mod <- mirt(tmp, itemtype="gpcmIRT",1)
    extr.2 <- extract.item(mod, 2)
    info.tmp <- iteminfo(extr.2, Theta)
    lines(Theta,info.tmp,)
}

##two general questions coming up. it might be easier to make a simplifying assumption (e.g., things are just coded 012):
for (i in 1:ncol(resp)) ifelse(resp[,i]>2,2,resp[,i])->resp[,i]
##1. what is the effect of dichotomizing low (e.g., responses of 1 and 2 become 1) versus high (e.g., responses of 0 and 1 become 0) on the central tendency and spread of information curves.
##2. how do standard errors for theta compare when we estimate the GPCM on the data versus low and high dichotimizations?
