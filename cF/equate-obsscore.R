##the goal here is to do some observed score equating
##note: i'm going to occasionally equate things that *should not* be equated. if you're following closely, you should be able to see this coming.

read.table("https://raw.githubusercontent.com/ben-domingue/252L/master/data/rasch.txt")->resp
##we're now going to form a bunch of groups to be equated. make sure you understand what each one is!!! some of them are randomly equivalent. others, not at all!
##first i'm going to do some ordering with rows and columns, don't worry about this bit.
sample(1:nrow(resp),replace=FALSE)->index
resp[index,]->resp 
sample(1:ncol(resp),replace=FALSE)->index
resp[,index]->resp 

pairs<-list( #let's create a variety of different groups
    re.people=list(rowSums(resp[1:500,]),rowSums(resp[501:1000,])), #here we're just taking the first and second half of the data. randomly equivalent due to our reshuffling above. 
    re.items=list(rowSums(resp[,1:20]),rowSums(resp[,21:50])) #now we're not taking groups of respondents but groups of items! basically creating something like a quasi-parallel form
    )
##now let's take systematically varying splits of the data
resp[,order(colSums(resp))]->resp2
pairs$diff.items <- list(rowSums(resp2[,1:15]),rowSums(resp2[,16:50])) #again we are grouping by items, but in a very specific way. the first list element is all hard items, the second element all easy items. so, very different test forms
resp[order(rowSums(resp)),]->resp2
pairs$diff.people <- list(rowSums(resp2[1:500,]),rowSums(resp2[501:1000,])) #now we're creating two clearly non-equivalent groups. first half is low-achieving people, second half is high-achieving.

#############################################################################################################################################
##observed score equating: linear equating
linear_equate<-function(nm,L) { ##recall that this is just equating scores equally distant (in SD units) from the mean
    L[[nm]][[1]]->s1
    L[[nm]][[2]]->s2
    mod<-function(x,y) {
        mean(x)->mx
        mean(y)->my
        sd(x)->sx
        sd(y)->sy
        x.new<-(sy/sx)*x+(my-(sy/sx)*mx)
        x.new
    }
    mod(s1,s2)->s1.new
    mod(s2,s1)->s2.new
    #
    range(c(s1,s2))->xl
    range(c(s1.new,s2.new))->yl
    plot(s1,s1.new,type="l",xlab="s1",ylab="s2",col="red",main=nm,xlim=xl,ylim=yl)
    abline(0,1,lty=2)
    plot(s2,s2.new,type="l",xlab="s2",ylab="s1",col="red",xlim=xl,ylim=yl)
    abline(0,1,lty=2)
}
par(mfrow=c(4,2),mar=c(3,3,1,1),mgp=c(2,1,0))
lapply(names(pairs),linear_equate,pairs)
##let's walk through figures. left hand is map of group 1 scores onto group 2 scale, right hand is reversed.
##black dashed line is 45 degree line, red line is the map
##rows are the different ways of splitting the data from above. which of these equating schemes is sensible?

#############################################################################################################################################
##observed score equating: equipercentile equating
equipercentile_equate<-function(nm,L) { #now we're going to equate scores at same percentile of each distribution
    L[[nm]][[1]]->s1
    L[[nm]][[2]]->s2
    quantile(s1,(1:999)/1000)->q1
    quantile(s2,(1:999)/1000)->q2
    #
    s1.new<-numeric()
    for (i in 1:length(s1)) {
        which.min(abs(s1[i]-q1))->j
        q2[j]->s1.new[i]
    }
    plot(q1,q2,type="l",xlab="s1",ylab="s2",main=nm)
    abline(0,1,lty=2)
    points(s1,s1.new,col="red",pch=19)
    #
    s2.new<-numeric()
    for (i in 1:length(s2)) {
        which.min(abs(s2[i]-q2))->j
        q1[j]->s2.new[i]
    }
    plot(q2,q1,type="l",xlab="s2",ylab="s1")
    abline(0,1,lty=2)
    points(s2,s2.new,col="red",pch=19)
    #
}
par(mfrow=c(4,2),mar=c(3,3,1,1),mgp=c(2,1,0))
lapply(names(pairs),equipercentile_equate,pairs)

