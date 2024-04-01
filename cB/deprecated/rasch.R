## ##this will be our maiden voyage estimating IRT models
## ##the main goal is to get oriented with the key output that we get from applications of these models to item response data


## ##get resp1, resp2, and resp4 from cB/item_analysis.R
## ##i'll write code for a generic 'resp' dataset. you can experiment with the different ones (e.g., change 'resp1' in below line to resp2 or whatever) as you see fit.
## resp1->resp

## library(mirt)
## m1<-mirt(resp1,1,itemtype="Rasch")
## itemplot(m1,1) #what do we have here?
## plot(m1,type="trace")
## ##which is the easiest item? the most difficult item?

## coef(m1)
## ##what do we have here? in particular, i would look over this closely vis-a-vis the relevant part of the mirt manual:
##      ## Rasch Only one intercept estimated, and the latent variance of
##      ##      theta is freely estimated. If the data have more than two
##      ##      categories then a partial credit model is used instead (see
##      ##      'gpcm' below).

##      ##           P(x = 1|theta, d) = \frac{1}{1 + exp(-(theta + d))}      
          
##      ## 2-4PL Depending on the model u may be equal to 1 and g may be
##      ##      equal to 0.

##      ##      P(x = 1|theta, psi) = g + \frac{(u - g)}{
##      ##            1 + exp(-(a_1 * theta_1 + a_2 * theta_2 + d))}
## ##question: what was the scale identification restriction used here?

## ##this might be useful
## get_coef<-function(mod) {
##     coef(mod)->co
##     co[-length(co)]->co #why do i get rid of this last bit?
##     do.call("rbind",co)
## }
## get_coef(m1)
## ##can you compare this "stuff" to the ctt item statistics we produced in the last analysis (item_analysis.R)?

## ##question: where are our theta estimates?

## ##what if you consider rasch models for resp2 or resp3?
## ##IMPORTANT: recall that one of the items had very little variation in the CTT discrimination coefficients. is that showing up here in any way? 

## ##here is a fun way of looking at comparing the estimated icc to empirical data
## itemfit(m1,empirical.plot=3)
## ##if you studied these figures carefully, you should be able to develop some of the geometric intuition discussed in Wu & Adams related to outfit statistics (which we'll tackle momentarily)
