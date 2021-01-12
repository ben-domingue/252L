x<-rnorm(100)
y<-1.2*x+rnorm(length(x))
plot(x,y)
abline(lm(y~x))

         
