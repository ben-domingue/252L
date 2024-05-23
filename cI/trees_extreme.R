##empirical
dataset<- redivis::user("datapages")$dataset("item_response_warehouse",version="v4.0")
df <- dataset$table("offlinefriend_bigfive")$to_data_frame()

##let's simplify things
df<-df[df$resp!=3,] #remove the middle

x<-irw::long2resp(df)
x$id<-NULL

##let's first have a model wherein you choose high/low and then it is all whether or not you prefer extreme responses
L<-list()
for (i in 1:ncol(x)) {
    r1<-ifelse(x[,i]<3,0,1) #this is low/high indicator
    r2<-ifelse(x[,i] %in% c(1,5),1,0) #note this, weird!
    df1<-data.frame(id=1:nrow(x),item=paste('item',i),node='n1',resp=r1)
    df2<-data.frame(id=1:nrow(x),item=paste('item',i),node='n2',resp=r2)
    L[[i]]<-rbind(df1,df2)
}
df<-data.frame(do.call("rbind",L))
##
library(lme4)
m3 <- lmer(resp ~ 0 + item:node + (0 + node | id), 
                                        #family = binomial, #let's use linear probability model to save time
           data = df)
m3
