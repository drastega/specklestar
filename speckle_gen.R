library(tidyverse)
library(imager)
library(rgl)

# speckle_gen <- function(rho = 0.5, theta = 0, dm = 0, seeing = 1, wind_speed = 0, N_frames = 1){}
t0 <- Sys.time() # start time

n_x <- 512; n_y <- n_x # Number of pixels in x and y
sigma_noise <- 30
mean_noise <- 400
noise <- runif(n_x * n_y) * sigma_noise + mean_noise %>% matrix(512, 512)

seeing <- 40 # in relative units
wind_speed <- 0 # in relative units

N_speckle <- 300 # Number of speckles
speckle_amplitude1 <- 200
speckle_amplitude2 <- 200
speckle_sigma <- 2

gaussian_2d <- matrix(0, nrow = n_x, ncol = n_y)
speckle_field <- matrix(0, nrow = n_x, ncol = n_y)

stellar_center_x <- rnorm(1, mean = 257, sd = wind_speed)
stellar_center_y <- rnorm(1, mean = 257, sd = wind_speed)

rho_x <- 10
rho_y <- 10

for (i in seq(N_speckle)) {
  x0 <- rnorm(1, mean = stellar_center_x, sd = seeing) - rho_x / 2
  y0 <- rnorm(1, mean = stellar_center_y, sd = seeing) - rho_y / 2
  for (x in seq(n_x)) {
    for (y in seq(n_y)) {
    gaussian_2d[x, y] <- speckle_amplitude1 * exp(-((x - x0)^2/(2 * speckle_sigma^2) + (y - y0)^2/(2 * speckle_sigma^2))) +
      speckle_amplitude2 * exp(-((x - x0 - rho_x)^2/(2 * speckle_sigma^2) + (y - y0 - rho_y)^2/(2 * speckle_sigma^2)))
    }
  }
  speckle_field <- speckle_field + gaussian_2d
}

speckle_frame <- speckle_field + noise
plot(as.cimg(speckle_frame), axes = FALSE)
print(Sys.time() - t0)

PS_model <- abs(fftw2d(speckle_frame))^2 %>% fftshift(dimension = -1)
plot(as.cimg(PS_model^0.001), axes = FALSE)
