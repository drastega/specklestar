library(tidyverse)
library(imager)
library(rgl)
library(fftwtools) # for fftw
library(mrbsizeR) # for fftshift
library(Rcpp)
library(animation)

t0 <- Sys.time() # start time
# N_speckle, n_x and n_y are defined in speckle_gen.cpp
# N_speckle = 300 # Number of speckles from single star
# n_x <- 512; n_y <- n_x # Number of pixels in x and y

# Weather conditions + optics:
seeing <- 30 # in relative units
wind_speed <- 20 # in relative units
speckle_sigma <- 1.5

# Parameters of binary system:
m1 <- 1000 # Relative magnitude of primary component
m2 <- 700 # Relative magnitude of secondary component
rho_x <- 30; rho_y <-30 # Projections of separation vector Rho

sourceCpp('speckle_gen.cpp')
speckle_field <- speckle_gen(seeing = seeing, speckle_sigma = speckle_sigma,
  m1 = m1, m2 = m2, rho_x = rho_x, rho_y = rho_y, wind = wind_speed) %>% matrix(n_x, n_y)

sigma_noise <- 50; mean_noise <- 400
noise <- rnorm(n_x * n_y, mean = mean_noise, sd = sigma_noise) %>% matrix(n_x, n_y)

speckle_frame <- speckle_field + noise

print(Sys.time() - t0)

# Visualization
par(mar = c(0, 0, 0, 0))
plot(as.cimg(speckle_frame), axes = FALSE)

saveGIF({
  for (i in 1:10){
    speckle_field <- speckle_gen(seeing = seeing, speckle_sigma = speckle_sigma,
      m1 = m1, m2 = m2, rho_x = rho_x, rho_y = rho_y, wind = wind_speed) %>% matrix(n_x, n_y)
    speckle_frame <- speckle_field + noise
    plot(as.cimg(speckle_frame), axes = FALSE)
  }
}, interval = 1)

PS_model <- abs(fftw2d(speckle_frame))^2 %>% fftshift(dimension = -1)
#plot(as.cimg(PS_model^0.01), axes = FALSE)

par(mar = c(5.1, 4.1, 4.1, 2.1)) # standard margin parameters
