dataset <- redivis::user("datapages")$dataset("item_response_warehouse",version='v5.0')
df <- dataset$table("chess_lnirt")$to_data_frame()
df<-df[!is.na(df$resp),]
library(irw)
resp<-irw::long2resp(df)
resp$id<-NULL

##first look at whether resp is relatively complete [if there was a lotof missingness, we'd have potential selection bias]

##second let's compute descriptives
pv<-colMeans(resp,na.rm=TRUE) #simple "p-values", which in psychometrics tends to just mean the mean number of points for an item
M<-by(df$rt,df$item,mean,na.rm=TRUE)
z<-merge(data.frame(item=names(pv),pv=pv),data.frame(item=names(M),rt=as.numeric(M)))
plot(z$rt,z$pv)
