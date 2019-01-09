#################################################
##Exploration of collections of bernoulli variables
#################################################

set.seed(12311)
x1<-matrix(rbinom(1000,1,.5),100,10)
##Q. Compute all the correlations of the columns of this matrix (x1). What do you notice?
cor(x1)
##Q. Compute the row sums. What is the variation in row sums?
var(rowSums(x1))
##Q. If you considered the 1s/0s correct and incorrect responses to test items (where the rows are people and the columns are items), does this seem like it could have come from a realistic scenario?


#################################################
##Feel free to ignore this bit. I'm going to generate a new set of data. 
set.seed(12311)
th<-matrix(rnorm(100),100,10,byrow=FALSE)
diff<-matrix<-matrix(rnorm(10),100,10,byrow=TRUE)
kern<- exp(th - diff)
pr<-kern/(1+kern)
test<-matrix(runif(1000),100,10)
x2<-ifelse(pr>test,1,0)

##Q. Now go back through the above questions and see what you make of this new matrix x2. Specifically, how does it compare to the first matrix x1 in terms of whether it seems like a realistic set of item responses? What characteristics (feel free to explore other features of the data) influence your opinion on this point?
