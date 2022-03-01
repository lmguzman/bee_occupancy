library('abind')
library('rjags')
library('R2jags')
library('runjags')
library('R2WinBUGS')
library('parallel')

## expit and logit functions
expit <- function(x) 1/(1+exp(-x))
logit <- function(x) log(x/(1-x))

## make a summary for a run.jags model
make.summary <- function(jags.out) {
  ## create sims.matrix
  sims.mat <- do.call(rbind, jags.out$mcmc)
  vars <- colnames(sims.mat)
  mean <- colMeans(sims.mat)
  quantiles <- apply(sims.mat, 2, quantile, p=c(0.025,0.975))
  cbind(mean, t(quantiles))
}

## get mean and bci for parameter from jags output
get.mean.bci <- function(out, param) {
  sims.arr <-
    aperm(sapply(out$mcmc, I, simplify='array'), c(1,3,2))
  c(mean=mean(sims.arr[,,param]),
    quantile(sims.arr[,,param], p=c(0.025,0.975)))
}

