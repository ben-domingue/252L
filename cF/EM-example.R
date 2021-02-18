##i'm assuming you have some function EM() that will compute the relevant probabilities.
##the data here will consist of the sums of heads (n) from N (probably 10) coin tosses
EM(dat=c(5,9,8,4,7),init=c(.52,.5)) #http://www.nature.com/nbt/journal/v26/n8/full/nbt1406.html

##using the EM function you can start to ask questions.
##a. how sensitive is estimation (e.g., in terms of bias or mean squared error) to initial values (e.g., c(.52,.5) in above)?
##b. how sensitive is estimation to the location of true parameters (e.g., the "p" argument below)?
##c. to the values of n and N?

##you don't necessarily need to answer all of these. i am just hoping you experiment enough with this to develop some intuition for how estimation depends on the specifics of a given scenario.

##in case it helps, here is a function to simulate data
sim<-function(N.runs=5,N.flips.per.run=10,p=NULL) {
    if (is.null(p)) runif(2)->p
    sort(p)->p
    dat<-numeric()
    for (ii in 1:N.runs) {
        sample(1:2,1)->index
        rbinom(1,size=N.flips.per.run,prob=p[index])->dat[ii]
    }
    dat
}
sim()->dat

EM(dat)
