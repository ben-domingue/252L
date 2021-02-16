library(mirt)

mod.pcm <- mirt(Science, 1,itemtype=rep("gpcm",ncol(Science))) 
mod.pcm #q: what do we have here?
coef(mod.pcm) #q: please stop and try to interpret these!
q

coef(mod.pcm,IRTpars=TRUE) #q: is this better?
plot(mod.pcm, type = 'trace')

runif(nrow(Science))->test
ifelse(Science$Comfort==2 & test<.5,1,Science$Comfort)->Science$Comfort #what am i doing here? 
apply(Science,2,table)
mod.pcm.fake <- mirt(Science, 1,itemtype=rep("Rasch",ncol(Science))) 
coef(mod.pcm,IRTpars=TRUE) 
coef(mod.pcm.fake,IRTpars=TRUE) #q: can you make sense of this?
plot(mod.pcm.fake, type = 'trace')



#you can now do something comparable with testing data.
set.seed(12311)
read.table("https://raw.githubusercontent.com/ben-domingue/252L_winter2018/master/data/emp-reading-3pl-gpcm.txt",header=TRUE)->resp
resp[rowSums(is.na(resp))==0,]->resp
resp[1:5000,]->resp
#first just the constructed response items
grep("^cr",names(resp))->index
resp[,index]->resp.cr
apply(resp,2,table)
#
mod <- mirt(resp.cr, itemtype="Rasch",1)

plot(mod,type="trace")

plot(mod,type="info")
plot(mod,type="rxx")
plot(mod,type="infotrace")
plot(mod,type="SE")
plot(mod,type="score")



                                        #a full mixed format test
set.seed(12311)
read.table("https://raw.githubusercontent.com/ben-domingue/252L_winter2018/master/data/emp-reading-3pl-gpcm.txt",header=TRUE)->resp
resp[rowSums(is.na(resp))==0,]->resp
resp[1:5000,]->resp



#first just the constructed response items
apply(resp,2,function(x) length(unique(x)))->tab
#
mod <- mirt(resp, itemtype=ifelse(tab==2,"3PL","gpcmIRT"),1)
mod <- mirt(resp, itemtype=ifelse(tab==2,"2PL","gpcmIRT"),1)

                                        #now, how would you find reversals
plot(mod,type="trace")
