nm <- c("enem_2013_1mil_ch")
dataset <- redivis::user("datapages")$dataset("item_response_warehouse")
df <- dataset$table(nm)$to_data_frame()

ids<-sample(unique(df$id),50000)
df<-df[df$id %in% ids,]

table(df$item,df$booklet) #just confirming items are repeated across booklet

L<-split(df,df$booklet)
f<-function(x) {
    library(mirt)
    x<-irw::long2resp(x)
    x$id<-NULL
    m<-mirt(x,1,'Rasch')
    z<-coef(m,simplify=TRUE)$items
    data.frame(item=rownames(z),b=z[,2])
}
est<-lapply(L,f)

x0<-est[[1]]
names(x0)[2]<-names(est)[1]
for (i in 2:length(est)) {
    x<-est[[i]]
    names(x)[2]<-names(est)[i]
    x0<-merge(x0,x)
}
plot(x0[,-1])
