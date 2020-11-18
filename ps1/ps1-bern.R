#################################################
##Exploration of collections of bernoulli variables
#################################################

set.seed(12311)
x1<-matrix(rbinom(1000,1,.5),100,10)
##Let's pretend that x1 is item response data from a test. So 1s and 0s are correct/incorrect responses (rows are people and columns are items).
##For fun we can look at the correlations across items and the variation in row sums (ie, total scores)
cor(x1)
var(rowSums(x1))
##Q. If you considered the 1s/0s correct and incorrect responses to test items (where the rows are people and the columns are items), does this seem like it could have come from a realistic scenario? How might we know?


#################################################
##Feel free to ignore this chunk of code (skip ahead to below question). I'm going to generate a new set of data. 
set.seed(12311)
th<-matrix(rnorm(100),100,10,byrow=FALSE)
diff<-matrix<-matrix(rnorm(10),100,10,byrow=TRUE)
kern<- exp(th - diff)
pr<-kern/(1+kern)
test<-matrix(runif(1000),100,10)
x2<-ifelse(pr>test,1,0)

##Q. Now, let's ask the same questino of the new matrix x2. Does it seem like realistic item response data? Specifically, how does it compare to the first matrix x1 in terms of whether it seems like a realistic set of item responses? What characteristics influence your opinion on this point?
