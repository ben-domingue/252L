fun<-function(x) {
    ##prep data
    L<-list()
    for (i in 1:ncol(x)) {
        r1<-ifelse(x[,i]==0,0,1)
        r2<-ifelse(x[,i]>1,1,0)
        r2<-ifelse(r1==0,NA,r2)
        df1<-data.frame(id=1:nrow(x),item=paste('item',i),node='n1',resp=r1)
        df2<-data.frame(id=1:nrow(x),item=paste('item',i),node='n2',resp=r2)
        L[[i]]<-rbind(df1,df2)
    }
    df<-data.frame(do.call("rbind",L))
    ##
    library(lme4)
    m1 <- lmer(resp ~ 0 + item:node + (0 + node | id), 
                                        #family = binomial, #let's use linear probability model to save time
               data = df)
    m1
}


##simulated
ni<-20 #number of items
n<-1000 #number of people
library(mirt)
a<-matrix(rep(1,ni),ncol=1)
##gpcm
set.seed(10101)
d<-list()
for (i in 1:20) d[[i]]<-runif(2,min=-1.5,max=1.5)+rnorm(1,sd=.5)
d<-do.call("rbind",d)
d<-cbind(0,d)
x <- simdata(a, d,
             N=n,
             itemtype = 'gpcm')  #We are simulating data from the GPCM model [remember this? https://docs.google.com/presentation/d/1CovCiFmIOpKNotlwPdx3hdq1LIE5R8GY8zDSdDLbUO8/edit#slide=id.g2709dcad0ed_1_25]
fun(x) #what do you note about this?

##empirical
dataset<- redivis::user("datapages")$dataset("item_response_warehouse",version="v4.0")
df <- dataset$table("offlinefriend_bigfive")$to_data_frame()

##let's simplify things
df$resp<-ifelse(df$resp==1,2,df$resp)
df$resp<-ifelse(df$resp==5,4,df$resp)
df$resp<-df$resp-2
table(df$resp)

x<-irw::long2resp(df)
x$id<-NULL
m0<-fun(x)  
m0 #what do you note about this? thoughts? 

