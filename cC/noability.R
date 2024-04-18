##let's work with an empirical dataset
dataset <- redivis::user("datapages")$dataset("item_response_warehouse",version='v5.0') #https://redivis.com/datasets/as2e-cv7jb41fd/tables/h4s7-21jrvyww2
df1 <- dataset$table("kim2023")$to_data_frame()
df2 <- dataset$table("roar_lexical")$to_data_frame()

##what feature of these datasets makes a comparison of the predictive task challenging?


m1<-by(df1$resp,df1$item,mean,na.RM=TRUE)
m2<-by(df2$resp,df2$item,mean,na.RM=TRUE)
plot(NULL,xlim=0:1,ylim=c(0,5))
lines(density(m1))
lines(density(m2),col='red')



library(mirt)
library(irw) 
set.seed(8675309)

for (df in list(df1,df2)) {
    items<-unique(df$item)
    if (all(items %in% 1:length(items))) {
        df$item<-paste("item_",df$item,sep='')
        items<-unique(df$item)
    }
    resp<-irw::long2resp(df)
    id<-resp$id
    resp$id<-NULL
    ##cross-validation for models estimated in mirt
    ntimes<-4
    df$gr<-sample(1:ntimes,nrow(df),replace=TRUE)
    x.hold<-df
    omega<-numeric()
    for (i in 1:ntimes) {
        x<-x.hold
        x$oos<-ifelse(x$gr==i,1,0)
        x0<-x[x$oos==0,]
        resp0<-data.frame(irw::long2resp(x0))
        id<-resp0$id
        resp0$id<-NULL
        ##no ability
        mm<-by(x0$resp,x0$item,mean,na.rm=TRUE)
        mm<-data.frame(item=names(mm),no=as.numeric(mm))
        ##rasch model
        m0<-mirt(resp0,1,'Rasch',verbose=FALSE)
        ##
        z0<-getp(m0,x=x[x$oos==1,],id=id)
        z<-merge(z0,mm)
        z<-z[,c("resp","no","p")]
        omega[i]<-imv(z,p1="no",p2="p")
    }
    print(mean(omega))
}

