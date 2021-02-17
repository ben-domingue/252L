##We have been treating dichotomous and polytomous items separately. But, the magic of IRT is that you can just use different IRFs for different items
set.seed(12311)
read.table("https://raw.githubusercontent.com/ben-domingue/252L/master/data/emp-reading-3pl-gpcm.txt",header=TRUE)->resp ##might take a while, could also download directly and then read.table locally
resp[rowSums(is.na(resp))==0,]->resp
resp[1:5000,]->resp

library(mirt)
mirt(resp,1,itemtype="Rasch")->mod1 #this applies the Rasch and PCM to dich/poly items respectively
mirt(resp,1)->mod2 #this applies the 2PL/GPCM to dich/poly items
##how would you interpret coef(mod1) and coef(mod2)
coef(mod1,IRTpars=TRUE) 
coef(mod2,IRTpars=TRUE) 

##what do you think about differences in information between MC and CR items
plot(mod1,type="infotrace")
plot(mod2,type="infotrace")
