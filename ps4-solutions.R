read.table(file="https://raw.githubusercontent.com/ben-domingue/252L/master/data/emp-3pl-math-reading.txt",header=TRUE)->x
dim(x)
x[rowSums(is.na(x))==0,]->x
dim(x)

grep("r.mc",names(x))->index
rowSums(x[,index])->rs
as.numeric(rs<mean(rs))-> gr #this can be treated as the confounding variable in the DIF analysis
grep("m.mc",names(x))->index
rowSums(x[,index])->th #this can be treated as ability in DIF analysis

resp<-x[,index]
tab<-list()
for (i in 1:ncol(resp)) {
    m<-glm(resp[,i]~th+gr,family="binomial")
    tab[[i]]<-summary(m)$coef[3,]
}
tab<-do.call("rbind",tab)
rownames(tab)<-colnames(resp)

##So, do we see DIF?
hist(tab[,4],breaks=seq(0,1,by=.05)) #histogram of p-values of 'gr' coefficients from above glm() models
##We do! When you look at distributions of p-values, the null hypothesis is basically always that they should be uniform on [0,1]. you can see a clear enrichment of p-values here near 0 which we might take as evidence of DIF

##What else can we do/say? Let me make 2 notes
##1 One thing is that we might worry about the sign (not just the magnitude of the coefficients
by(tab[,1],tab[,4]<.05,summary)
##You can see that the coefficients for gr are close to 0 for insignificant estimates. that is to be expected. for the significant estimates, there are a few negative ones but the majority are positive. this is to be expected!! if we are going to observe DIF here, i think it more likely that the math items would be potentially too easy for respondents with large verbal ability. this makes sense conceptually and is what the coefficients are largely telling us. 

##2 What about the multiple testing?
##We have condcuted 45 hypothesis tests which is a problem. We could potentially do a Bonferroni correction here:
alpha<-.05/ncol(resp)
tab[tab[,4]<alpha,] #the items that show some evidence for bias after adjustment. note that estimates are largely positive! 

##Analysis of DIF is both challenging (many available algorithms for DIF identification, lots of judgements to be made, etc) but obviously of crucial importance. I don't think there is a cookbook type approach that you can hope to rely on in every instance, but hopefully some of the topics we covered in this course provide you with a core set of ideas around which you can build an argument about bias. and, i'm always happy to talk more :)
