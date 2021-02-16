set.seed(12311)
read.table("https://raw.githubusercontent.com/ben-domingue/252L_winter2018/master/data/emp-reading-3pl-gpcm.txt",header=TRUE)->resp
resp[rowSums(is.na(resp))==0,]->resp
resp[1:5000,]->resp

library(mirt)
mirt(resp,1,itemtype="Rasch")->mod1
mirt(resp,1)->mod2
##how would you interpret coef(mod1) and coef(mod2)

##what do you think about differences in information between MC and CR items
plot(mod1,type="infotrace")
plot(mod2,type="infotrace")
