library(TeachingDemos)

## The teaching demo library can be a useful starting point for simulating
## data for mixed models. We use it here to create a simple dataframe that we can add to later ##

## Here simulates a response variable for 24 individuals each sampled 10 times,
## giving a total sample size of 240. Half the individuals are classed as 
## female, the other  half as male. The mean response variable for females
## had a mean of 10 and for males this was 20. 


## Between-individual standard deviation was set at 1 and the residual standard deviation was set at 3 ##
sig.id<-1
sig.e <-3

simul <- simfun({
    n.ids <- 24
    n <- 240
    response <- h[sex] +rnorm(n.ids,0,sig.id)[id] + rnorm(n,0,sig.e)
}, sig.id=1,  sig.e=3, h=c(10,20),
drop=c('sig.id','sig.e','h','n.ids','n'))


## useful for simple mixed models if no differences in within-individual variance
## i.e. homoscedastic residual variance ##

tmpdat <- data.frame(id=gl(24,10),sex=gl(2,10,length=240, labels=c('F','M')))
simdat <- simul(tmpdat)



#
library(arm)

## Confirm that simulated values roughly match up with results ##
## lmer provides a quick and easy way to check this with a 
## random-intercept mixed model ##

m1 <- lmer(response~ sex + (1|id), data=simdat)

summary(m1)

display(m1)
