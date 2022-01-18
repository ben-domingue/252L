##CTT focuses on a decomposition of observed score variance
##IRT is going to focus on the probability of a correct response to a given item by a person of a specific ability
##Goal here is to generate some intuition into why this is a reasonable idea to consider.

#########################################################
##we are first going to read in an empirical dichotomously coded item response dataset
resp<-read.table("https://github.com/ben-domingue/252L/raw/master/data/emp-rasch.txt",header=FALSE)
resp[rowSums(is.na(resp))==0,]->resp


#########################################################
##what we want to do is look at the proportion of correct responses for different observed scores / sum scores
##the first step in this process wil be to organize data appropriately
##in particular, we want a matrix that has:
##items going from hardest to easiest
##persons going from least to most able

##we'll do the person-side sorting first
##we're going to just go through each observed sum score and collect the people
tmp<-list()
rowSums(resp)->rs
for (i in sort(unique(rs))) {
    resp[rs==i,]->tmp[[as.character(i)]]
} #so what is structure of tmp?
do.call("rbind",tmp)->resp #this is a kind of tough command. see if you can make sense of it. i find working in this way with lists is super intuitive once you see the logic (let's talk if you don't!).

##we'll do the items a little more succinctly. we could have done something like this for the people.
colSums(resp)->cs
resp[,order(cs,decreasing=FALSE)]->resp 

##just a quick double check that everything is monotonic in the ways we'd expect
##what do you expect to see? before running the next set of commands, draw a pictue for yourself. 
par(mfrow=c(2,1))
plot(colMeans(resp),type="l")
plot(rowMeans(resp),type="l")
##pause at this point to check in with ben

#############################################################
##now we have most able examinees on the bottom and the hardest items on the left in 'resp'.
##aside: my entire dissertation was spent futzing about with implications that following from such orderings. https://link.springer.com/article/10.1007/s11336-013-9342-4
##let's condense this by collapsing rows so that all individuals with a common score are represented in a single row.
##a cell will now tell us the proportion of respondents in that row who responded correctly to a given item
rowSums(resp)->rs
tmp<-list()
sort(unique(rs))->rs.sorted
for (i in rs.sorted) {
    resp[rs==i,,drop=FALSE]->z
    colMeans(z)->tmp[[as.character(i)]]
}
do.call("rbind",tmp)->prop
rs.sorted->rownames(prop)
##note: it is this sort of list usage that i find very convenient. for each element of the list, we transformed a matrix (containing all respondents with a given sum score) into a single row vector (containing the proportion of correct responses to each item for the group of examinees with common sum score).
##that was handy!                                        


#################################################################
##let's now look at the proportion of correct responsees as a function of sum score (for every row) for each item
##again, before running, what do you expect to see?
as.numeric(rownames(prop))->rs
5->i #first with just a single item
plot(rs,prop[,i],xlim=range(rs),ylim=0:1,xlab="sum score",ylab="% correct",type="l")

##Now all items
par(mfrow=c(10,5),mar=c(0,0,0,0))
for (i in 1:50) {
    plot(rs,prop[,i],xlim=range(rs),ylim=0:1,xlab="",ylab="",type="l",xaxt="n",yaxt="n")
}

##questions
##what are qualitative differences between curves
##why is curve smoothest generally in the middle?

##these kinds of non-parametric "ogives" are intimately related to what we're going to start talking about next week as "item characteristic curves" or "item response functions"
##the two big differences:
##1. we'll impose a parametric structure (although one doesn't necessarily have to, https://en.wikipedia.org/wiki/Mokken_scale)
##2. we won't observe an individual's location on the x-axis. this is the hard part!

