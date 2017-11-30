## NR 995 Module 14
## Intro to Bayes
## Collaborators: Erica Holm & Katie Moran
## Last modified: 11/29/2017

getwd()

###############################################
## Read in data and check for collinearity, etc
###############################################

pheno = read.table("../Bayes_pheno.csv", sep=",", header = T)
head(pheno)
summary(pheno)
is.na(pheno)
str(pheno) # no missing data

## check for collinearity in predictors
plot(pheno$Feb_temp, pheno$Feb_snow) ## no obvious relationship
abline(lm(pheno$Feb_snow ~ pheno$Feb_temp, data = pheno))
m = lm(pheno$Feb_snow ~ pheno$Feb_temp)
plot(m)
out = summary(m)
head(out)
out$r.squared

## plot predictors and response just for fun
plot(pheno$leaf_DOY, pheno$Feb_temp) #earlier leaf out with warmer mean temps, no surprise
plot(pheno$leaf_DOY, pheno$Feb_snow) #not an obvious relationship between snow and leaf out

###############################################
## set up and run 3 chain linear regression model for JAGS
###############################################

## install and load rjags
install.packages("rjags")
library(rjags)

## write the JAGS model

model_string <- "model{

# Likelihood
for (i in 1:Ntotal){
  y[i] ~ dnorm(mu[i], inv.var)
  mu[i] <- beta[1] + beta[2] * x1[i]

}

# Priors for beta, one for each beta parameter
  for(j in 1:2){
  beta[j] ~ dnorm(0, 0.0001)
}

# Prior for inverse variance
  inv.var ~ dgamma(0.01, 0.01)

}"

## set up data

setup <- list(y = pheno$leaf_DOY, x1 = pheno$Feb_temp, Ntotal = length(pheno$leaf_DOY))

## Compile model

model.obj <- jags.model(textConnection(model_string), data = setup, n.chains = 3)

## run model: burn-in

update(model.obj, 10000)

## sample posterior

samp <- coda.samples(model.obj, variable.names = c("beta"), n.iter = 20000)

###############################################
## check convergence and autocorrelation
###############################################

## Trace and density plots

plot(samp) # good mixing and horizontal

## autocorrelation plots

autocorr.plot(samp) # there is a small amount of autocorrelation

## gelman / BGR plot

gelman.plot(samp) # dropping to zero quickly but mixing a little funky and not all the way to zero

###############################################
## tune the model
###############################################

## Compile model

model.obj1 <- jags.model(textConnection(model_string), data = setup, n.chains = 3)

## run model: burn-in

update(model.obj1, 50000)

## sample posterior

samp1 <- coda.samples(model.obj1, variable.names = c("beta"), n.iter = 100000)

## Trace and density plots

plot(samp1) # good mixing and horizontal

## autocorrelation plots

autocorr.plot(samp1) # there is a small amount of autocorrelation

## gelman / BGR plot

gelman.plot(samp1) ## gelman plot is worse

### To KM: I can't figure this part out - increasing burn-in and iterations 
### doesn't fix the autocorrelation, and gelman for beta 2 gets worse


##################################################################
## interpretation
##################################################################

summary(samp)

20000*3 ## total posterior sample size

## Med val of post beta1 - 86.84 with a 95% credible interval of 85.84 - 88.09
## Med val of post beta2 - -0.71 with a 95% credible interval of -0.88 - -0.54

## For a unit increase in Feb temperature, you could expect an 86.84 increase in leaf doy of year 
## Does this even make sense? Why is it so large?
## Do we need to thin the model?
## Are we supposed to make a Bayesian model for snow too?
## It looks like a positive relationship DOES exist in snow vs. leaf out
## But the question says run one model with whatever preddictors you would need.
## How would you add in another predictor to the same model?

## Honestly, this week does not make sense to me


