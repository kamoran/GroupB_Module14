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
out$r.squared


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



