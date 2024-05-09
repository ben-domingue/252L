dataset <- redivis::user("datapages")$dataset("item_response_warehouse",version='v5.0')
nm <- c("blattman2017")
df <- dataset$table(nm)$to_data_frame()
library(lme4)

m<-lmer(resp~0+(1|id)+item+item:treatment,df)
fe<-fixef(m)
tx<-grepl(":",names(fe))
plot(fe[!tx],fe[tx])
