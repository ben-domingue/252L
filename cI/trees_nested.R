##empirical
dataset<- redivis::user("datapages")$dataset("item_response_warehouse",version="v4.0")
df <- dataset$table("offlinefriend_bigfive")$to_data_frame()

##let's simplify things
df$resp<-ifelse(df$resp==1,2,df$resp)

x<-irw::long2resp(df)
x$id<-NULL

##Model 1. linear
L<-list()
for (i in 1:ncol(x)) {
    r1<-ifelse(x[,i]>2,1,0)
    r2<-ifelse(x[,i]<3,NA,0)
    r2<-ifelse(x[,i]>3,1,r2)
    r3<-ifelse(x[,i]<4,NA,0)
    r3<-ifelse(x[,i]>4,1,r3)
    df1<-data.frame(id=1:nrow(x),item=paste('item',i),node='n1',resp=r1)
    df2<-data.frame(id=1:nrow(x),item=paste('item',i),node='n2',resp=r2)
    df3<-data.frame(id=1:nrow(x),item=paste('item',i),node='n3',resp=r3)
    L[[i]]<-rbind(df1,df2,df3)
}
df<-data.frame(do.call("rbind",L))
##
library(lme4)
m1 <- lmer(resp ~ 0 + item:node + (0 + node | id), 
                                        #family = binomial, #let's use linear probabiliy model to save time
           data = df)
m1

##Model 2. Nested
L<-list()
for (i in 1:ncol(x)) {
    r1<-ifelse(x[,i]>3,1,0)
    r2<-ifelse(r1==1,NA,0)
    r2<-ifelse(x[,i]==3,1,r2)
    r3<-ifelse(r1==0,NA,0)
    r3<-ifelse(x[,i]==5,1,r3)
    df1<-data.frame(id=1:nrow(x),item=paste('item',i),node='n1',resp=r1)
    df2<-data.frame(id=1:nrow(x),item=paste('item',i),node='n2',resp=r2)
    df3<-data.frame(id=1:nrow(x),item=paste('item',i),node='n3',resp=r3)
    L[[i]]<-rbind(df1,df2,df3)
}
df<-data.frame(do.call("rbind",L))
##
library(lme4)
m2 <- lmer(resp ~ 0 + item:node + (0 + node | id), 
                                        #family = binomial, #let's use linear probability model to save time
           data = df)
m2

##m1 and m2 are weird. why do i say that?
##we will next test a theory that might be one plausible reason things aren't working here
