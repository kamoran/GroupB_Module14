## NR 995 Module 14
## Intro to Bayes
## Collaborators: Erica Holm & Katie Moran
## Last modified: 11/29/2017

getwd()

###############################################
## Read in data and check for collinearity, etc
###############################################

pheno = read.table("Bayes_pheno.csv", sep=",", header = T)
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
out$r.squared ## confirmed no signification relationship R2 = 0.004


## plot predictors and response just for fun
plot(pheno$Feb_temp, pheno$leaf_DOY) #earlier leaf out with warmer mean temps, no surprise
plot(pheno$Feb_snow, pheno$leaf_DOY) #not an obvious relationship between snow and leaf out

###############################################
## set up and run 3 chain linear regression model for JAGS
###############################################

## install and load rjags
install.packages("rjags")
library(rjags)

## write the JAGS model
## need to include both predictors and their priors

model_string <- "model{

# Likelihood
for (i in 1:Ntotal){
y[i] ~ dnorm(mu[i], inv.var)
mu[i] <- beta[1] + beta[2] * x1[i] + beta[3] * x2[i]

}

# Priors for beta, one for each beta parameter
for(j in 1:3){
beta[j] ~ dnorm(0, 0.0001)
}

# Prior for inverse variance
inv.var ~ dgamma(0.01, 0.01)

}"

## data setup
dataList = list(y = pheno$leaf_DOY, x1 = pheno$Feb_temp, x2 = pheno$Feb_snow,
                Ntotal = length(pheno$leaf_DOY))

## compile and run model

model <- jags.model(textConnection(model_string), 
                    data = dataList, n.chains = 3)

## burn in
update(model, 10000)

## sample the posterior
samp <- coda.samples(model, variable.names = c("beta"),
                     n.iter = 20000)

###############################################
## Check convergence and autocorrelation of chains post burn-in
###############################################

## graphical summary
plot(samp)
## trace plots show good mixing, no trending up or down
## density plots show smooth, normal distribution
## model looks good on this end

## autocorrelation
autocorr.plot(samp)
## long lag times (>30) for betas 1 and 3 in each chain
## beta 2 looks good across all chains
## need to thin by 30

## BGR plot
gelman.plot(samp, xlim = c(0,50000), ylim=c(1,1.05))
# confirms beta 1 and 3 don't stabilize until past 20,000 

## The graphical summary shows that the chains are well mixed
## and the model has converged, parameters not dependent on timing. 
## The autocorrelation plots show that thinning needs to occur 
## because the lag time was so long. 

##########################################################
## Model Tuning
#########################################################

## recompile model with thinning, more iterations, and longer burn-in time

model1 <- jags.model(textConnection(model_string), 
                    data = dataList, n.chains = 3)

update(model1, 50000) 
## beta[2] BGR fell apart when 30000 used, 50000 corrected

samp1 <- coda.samples(model1, variable.names = c("beta"),
                     n.iter = 100000, thin = 100) 
#tried thinning by 30 first, wasn't enough, so did 100

autocorr.plot(samp1) #that looks better! 
plot(samp1) #still shows good mixing
gelman.plot(samp1) #beta[2] still doesn't look great but better
# tried a bunch of combinations but this seemed to be best overall fit of 
# burn in, iterations, thinning


