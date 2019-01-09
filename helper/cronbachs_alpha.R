kr20<-function(resp) {
    k<-ncol(resp)
    p<-colMeans(resp,na.rm=TRUE)
    q<-1-p
    o<-rowSums(resp)
    (k/(k-1))*(1-sum(p*q)/var(o))
}
cronbach_alpha<-function(resp) {
    k<-ncol(resp)
    v.i<-apply(resp,2,var)
    o<-rowSums(resp)
    (k/(k-1))*(1-sum(v.i)/var(o))
}

## #testing
## setwd("./data")
## list.files()->lf
## resp.L<-list()
## for (fn in lf) {
##     read.table(fn)->resp.L[[fn]]
## }
## lapply(resp.L,cronbach_alpha)
