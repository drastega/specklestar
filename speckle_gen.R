library(tidyverse)
library(imager)
library(rgl)
library(fftwtools) # for fftw
library(mrbsizeR) # for fftshift
library(Rcpp)

t0 <- Sys.time() # start time
# N_speckle, n_x and n_y are defined in speckle_gen.cpp
# N_speckle = 300 # Number of speckles from single star
# n_x <- 512; n_y <- n_x # Number of pixels in x and y

# Weather conditions + optics:
seeing <- 20 # in relative units
wind_speed <- 0 # in relative units
speckle_sigma <- 2

# Parameters of binary system:
m1 <- 200 # Relative magnitude of primary component
m2 <- 200 # Relative magnitude of secondary component
rho_x <- 70; rho_y <- 30 # Projections of separation vector Rho

sourceCpp('speckle_gen.cpp')
speckle_field <- speckle_gen(seeing = seeing, speckle_sigma = speckle_sigma,
  m1 = m1, m2 = m2, rho_x = rho_x, rho_y = rho_y, wind = wind_speed) %>% matrix(n_x, n_y)

sigma_noise <- 50; mean_noise <- 400
noise <- rnorm(n_x * n_y, mean = mean_noise, sd = sigma_noise) %>% matrix(n_x, n_y)

speckle_frame <- speckle_field + noise

# Visualization
par(mar = c(1, 1, 1, 1))
plot(as.cimg(speckle_frame), axes = FALSE)
par(mar = c(5.1, 4.1, 4.1, 2.1)) # standard margin parameters
print(Sys.time() - t0)

#PS_model <- abs(fftw2d(speckle_frame))^2 %>% fftshift(dimension = -1)
#plot(as.cimg(PS_model^0.001), axes = FALSE)
