library(mirt)
Science
## Description
## This data set comes from the Consumer Protection and Perceptions of Science and Technology
## section of the 1992 Euro-Barometer Survey (Karlheinz and Melich, 1992) based on a sample from
## Great Britain. The questions asked are given below:
## Format
## All of the below items were measured on a four-group scale with response categories "strongly
## disagree", "disagree to some extent", "agree to some extent" and "strongly agree":

## Comfort: Science and technology are making our lives healthier, easier and more comfortable.
## Environment: Scientific and technological research cannot play an important role in protecting the
## environment and repairing it.
## Work: The application of science and new technology will make work more interesting.
## Future: Thanks to science and technology, there will be more opportunities for the future generations.
## Benefit The benefits of science are greater than any harmful effect it may have.

##q: what do higher numbers mean here?
##q: what is this scale "measuring"?

apply(Science,2,table) #or, if you prefer, round(apply(Science,2,table) / nrow(Science), 2)
#q. what are the salient features of this data?
#q. what score categories concern you?




library(psych)
alpha(Science) 


##here is the mirt description of what we will estimate
## graded The graded model consists of sequential 2PL models, and
##      here k is the predicted category.
##      P(x = k | theta, psi) = P(x >= k | theta, phi) - P(x >= k + 1 | theta, phi)
mod <- mirt(Science, 1,itemtype="graded") 


mod
coef(mod,IRTpars=TRUE) 
##we will see these coefficients reappear momentarily when we look at trace lines.
##but, at this point, you should be able to draw them! ;)

plot(mod, type = 'trace')
##hm. let's check
extr <- extract.item(mod,1)
Theta <- matrix(seq(-6,6, length.out=2000))
pr <- probtrace(extr, Theta) #min() of first item
ii<-which.min(abs(pr[,1]-.5))
Theta[ii] #you should be able to match this value to something for the $Comfort item
ii<-which.min(abs(pr[,1]+pr[,2]-.5))
Theta[ii] #this one should match as well
ii<-which.min(abs(pr[,1]+pr[,2]+pr[,3]-.5))
Theta[ii] #and this one


plot(mod,type="info")
plot(mod,type="rxx")
plot(mod,type="infotrace")
plot(mod,type="SE")
plot(mod,type="score")

###########################################################################################################
#you can now do something comparable with testing data.
set.seed(12311)
read.table("https://raw.githubusercontent.com/ben-domingue/252L/master/data/emp-reading-3pl-gpcm.txt",header=TRUE)->resp ##might take a while, could also download directly and then read.table locally
resp[rowSums(is.na(resp))==0,]->resp
resp[1:5000,]->resp

#first just the constructed response items
grep("^cr",names(resp))->index
resp[,index]->resp.cr
apply(resp,2,table)
#
mod <- mirt(resp.cr, itemtype="graded",1)
plot(mod, type = 'trace')

