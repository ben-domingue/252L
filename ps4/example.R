##deprecated

## ##the data here will consist of the sums of heads (n) from N (probably 10) coin tosses
## dat<-c(5,9,8,4,7)
## N<-10
## ##my work is in EM-coins.R

## ##1. write the E-step that computes the exp number of h & t from each coin using data and preliminary values for the coin biases
## ##see Estep function

## ##2. now write an M-step function that maximizes coin biases based on expected h/t counts for each coin.
## ##see Mstep

## ##3. create a wrapper that will iterate over the E and M steps until convergence.
## ##see EM()
## EM(dat=c(5,9,8,4,7),init=c(.52,.5)) #http://www.nature.com/nbt/journal/v26/n8/full/nbt1406.html

## ##4. now you can start to ask questions. is quality of estimates sensitive to initial values? to the location of true parameters? to the values of n and N?
## ##in case it helps, here is a function to simulate data

## sim<-function(N.runs=5,N.flips.per.run=10) {
##     runif(2)->p
##     sort(p)->p
##     dat<-numeric()
##     for (ii in 1:N.runs) {
##         sample(1:2,1)->index
##         rbinom(1,size=N.flips.per.run,prob=p[index])->dat[ii]
##     }
##     dat
## }
## sim()->dat

