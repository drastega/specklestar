library(tidyverse)
library(imager)
library(rgl)
library(fftwtools) # for fftw
library(mrbsizeR) # for fftshift
library(Rcpp)
library(inline)

# speckle_gen <- function(rho = 0.5, theta = 0, dm = 0, seeing = 1, wind_speed = 0, N_frames = 1){}
t0 <- Sys.time() # start time

n_x <- 512; n_y <- n_x # Number of pixels in x and y
sigma_noise <- 30
mean_noise <- 400
noise <- runif(n_x * n_y) * sigma_noise + mean_noise %>% matrix(512, 512)

seeing <- 40 # in relative units
wind_speed <- 0 # in relative units

N_speckle <- 300 # Number of speckles
speckle_amplitude1 <- 200 # Relative magnitude of primary component
speckle_amplitude2 <- 200 # Relative magnitude of secondary component
speckle_sigma <- 2

gaussian_2d <- matrix(0, nrow = n_x, ncol = n_y)
speckle_field <- matrix(0, nrow = n_x, ncol = n_y)

stellar_center_x <- rnorm(1, mean = 257, sd = wind_speed)
stellar_center_y <- rnorm(1, mean = 257, sd = wind_speed)

rho_x <- 10
rho_y <- 10

#N_speckle, n_x, n_y, stellar_center_x, stellar_center_y, seeing, rho_x, rho_y, speckle_amplitude1, speckle_amplitude2,
#speckle_sigma, speckle_field, gaussian_2d

src <- '
int N_speckle = 300;
int n_x = 512;
int n_y = 512;

double seeing = 30;
double rho_x = 30;
double rho_y = 30;
double speckle_amplitude1 = 200;
double speckle_amplitude2 = 200;
double speckle_sigma = 1.5;

std::vector<double> speckle_field(262144);
std::vector<double> gaussian_2d(262144);

double stellar_center_x = 257;
double stellar_center_y = 257;
double x0;
double y0;

for (int i = 0; i < N_speckle; i++) {
  x0 = R::rnorm(stellar_center_x, seeing) - rho_x / 2;
  y0 = R::rnorm(stellar_center_y, seeing) - rho_y / 2;
  for (int x = 0;  x < n_x; x++) {
    for (int y = 0; y < n_y; y++) {
    gaussian_2d[x * n_y + y] = speckle_amplitude1 * exp(-(pow((x - x0), 2)/(2 * pow(speckle_sigma, 2)) + pow((y - y0), 2)/(2 * pow(speckle_sigma, 2)))) + speckle_amplitude2 * exp(-(pow((x - x0 - rho_x), 2)/(2 * pow(speckle_sigma, 2)) + pow((y - y0 - rho_y), 2)/(2 * pow(speckle_sigma, 2))));
//  gaussian_2d[x * n_y + y] = x * y;
    }
  }
  for (int c = 0 ; c < 262144 ; c++) {
    speckle_field[c] = speckle_field[c] + gaussian_2d[c];
  }
}
return Rcpp::wrap(speckle_field);
//return Rcpp::wrap(dist_r);
'

speckle_cpp <- rcpp(signature(), src)
speckle_field <- speckle_cpp() %>% matrix(512, 512)

speckle_frame <- speckle_field + noise
plot(as.cimg(speckle_frame), axes = FALSE)
print(Sys.time() - t0)

#PS_model <- abs(fftw2d(speckle_frame))^2 %>% fftshift(dimension = -1)
#plot(as.cimg(PS_model^0.001), axes = FALSE)
