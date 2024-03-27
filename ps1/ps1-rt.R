dataset <- redivis::user("datapages")$dataset("item_response_warehouse",version='v5.0')
df <- dataset$table("chess_lnirt")$to_data_frame()
df<-df[!is.na(df$resp),]
library(irw)
resp<-irw::long2resp(df)
resp$id<-NULL

##Now we have resp in a nice format. Let's consider the following descriptive approach to this problem.
##1. Let's first compute item-level mean responses. Given that (a) all items are dichotomous and (b) all respondents take virtually all items, we can use this as a rough proxy for item difficulty.
##2. Let's compute the average time for an item.
##3. If we look at the average item response as a function of average item response time, what do we see?

