library(rstanarm)
library(RcppCNPy)
library(specklestar) # ! remove

ps <- npyLoad('/Users/leda/home/programs/python/SI/2014-07-14/ps.npy')

an <- annulus(ps, 60)

stan_fit <- stan_nlmer(an ~ alpha[nu] + beta[nu] * cos(2 * pi * nu * rho))

