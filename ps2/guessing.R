dataset <- redivis::user("datapages")$dataset("item_response_warehouse",version='v5.0')
dataset_names <- c("roar_lexical")
nm<-dataset_names
df <- dataset$table(nm)$to_data_frame()

x<-irw::long2resp(df)
x$id<-NULL
rs<-rowMeans(x,na.rm=TRUE)

plot(NULL,xlim=range(rs),ylim=0:1)
index<-sample(1:ncol(x),10)
for (i in index) {
    m<-loess(x[,i]~rs)
    z<-cbind(m$x,predict(m))
    z<-z[order(z[,1]),]
    lines(z)
}
