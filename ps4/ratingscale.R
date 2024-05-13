dataset <- redivis::user("datapages")$dataset("item_response_warehouse",version='v5.0')
nm <- c("ffm_CSN")
df <- dataset$table(nm)$to_data_frame()
##cutting it down
ids<-sample(unique(df$id),10000)
df<-df[df$id %in% ids,]
df$resp<-as.numeric(df$resp)

resp<-irw::long2resp(df)
resp$id<-NULL
rs<-rowSums(is.na(resp))
resp<-resp[rs==0,] #only complete cases

library(mirt)
m<-mirt(resp,1,'rsm') #note that we are using a polytomous model we haven't yet talked much about.
plot(m, type = 'trace')
coef(m,IRTpars=TRUE)
##AA. What do you notice about this model? Focus your attention in particular on the b1-b4 and c paramters. 

##BB. Please describe the key assumption in this model (see "gpcmIRT and rsm" part of the 'IRT models' part of the help page here: https://www.rdocumentation.org/packages/mirt/versions/1.41/topics/mirt).

##Let's now fit another model
m.pcm<-mirt(resp,1,'Rasch') #here we are fitting the pcm
plot(m.pcm, type = 'trace')
coef(m.pcm,IRTpars=TRUE)

##let's look at both of these. note the AIC, BIC, and number of estimated parameters for each
m
m.pcm
##Note that the AIC and BIC are lower for m.pcm compared to m. This would conventionally be indicative of better 'fit' for m.pcm in this case. Let's see what happens when we really reduce the sample size. 
index<-sample(1:nrow(resp),200)
resp.small<-resp[index,]
m.small<-mirt(resp.small,1,'rsm')
m.pcm.small<-mirt(resp.small,1,'Rasch')
m.small
m.pcm.small
##CC. How would you think about the difference between the RSM & the PCM in general? This is a test with relatively few items. How might you anticipate a test with more items (say 50 as opposed to 10) showing sensitivity to the difference between rsm and pcm were we to reduce the number of respondents? 
