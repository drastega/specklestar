library(tidyverse)
library(imager)
library(rgl)

# speckle_gen <- function(rho = 0.5, theta = 0, dm = 0, seeing = 1, wind_speed = 0, N_frames = 1){}

N_pix <- 512 * 512 # Number of pixels in the frame
sigma_noise <- 30
mean_noise <- 400
noise <- runif(N_pix) * sigma_noise + mean_noise %>% matrix(512, 512)
N_speckle <- 300
seeing <- 40 # in relative units
wind_speed <- 20 # in relative units
x_center <- rnorm(1, mean = 257, sd = wind_speed) ; y_center <- rnorm(1, mean = 257, sd = wind_speed)

#speckle_frame <- matrix(0, 512, 512)

### Visualization
plot(as.cimg(noise))
points(x = rnorm(N_speckle, mean = x_center, sd = seeing), y = rnorm(N_speckle, mean = y_center, sd = seeing),
       col = 'white', pch = 20)

#open3d(useNULL = FALSE, windowRect=c(1,1,512,512))
#persp3d(z = noise, col = 'green')
