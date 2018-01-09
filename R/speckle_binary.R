library(tidyverse)
library(ff) # ff classes for big data
library(fftwtools) # for fftw
library(mrbsizeR) # for fftshift
library(imager) # for visualization (eg. as.cimg())
library(rgl)

speckle_binary <- function(data_file) {
#data_file <- file.choose()

Sys.setenv(TZ="Europe/Moscow")
t0 <- Sys.time() # start time

N_frames <- file.info(data_file)$size/(512 * 512 * 2)
data <- ff(filename = data_file, readonly = TRUE, dim = c(512, 512, N_frames), vmode = 'ushort')

### Power Spectrum calculation (try rewrite next for loop using map)
PS_line <- 0
PS_short <- 0
frame <- matrix(0, 1024, 1024)
for (i in seq(N_frames-1)) {
#for (i in seq(100)) {
#  mean_data <- data[, , i] - mean(data[, , i])
  mean_data <- data[, , i + 1] - data[, , i]
  frame[1:512, 1:512] <- mean_data
  PS_line <- PS_line + abs(fftw(frame))^2
  PS_short <- PS_short + abs(fftw2d(mean_data))^2
}
PS <- matrix(PS_line, 1024, 1024, byrow = TRUE)
PS <- fftshift(PS, dimension = -1) / (N_frames - 1)

PS_short <- fftshift(PS_short, dimension = -1) / (N_frames - 1)

ACF <- fftw2d(PS, inverse = TRUE, HermConj = 0) %>% abs() %>% fftshift(dimension = -1)
ACF_short <- fftw2d(PS_short, inverse = TRUE, HermConj = 0) %>% abs() %>% fftshift(dimension = -1)

print(Sys.time() - t0) # runtime calculation

### Visualization of secondary peak in ACF
## imager & rgl
plot(as.cimg(ACF_short^0.1))
print('Point position of upper secondary maximum on the plot ...')
max_pos <- locator(1) # Get position of secondary maximum from cursor
wind_width <- 5 # Set width of window with local max
ACF_short_magn <- ACF_short[(max_pos$x - wind_width):(max_pos$x + wind_width),
                            (max_pos$y - wind_width):(max_pos$y + wind_width)]

# Visualize here ACF_short_magn to sure the local max == global max
#open3d(useNULL = FALSE, windowRect=c(1,1,512,512))
#persp3d(z = ACF_short_magn, col = 'green')

max_indx_magn <- which(ACF_short_magn == max(ACF_short_magn), arr.ind = TRUE)
max_indx <- matrix(c(round(max_pos$x), round(max_pos$y)), ncol = 2,
 dimnames = list(1, c('row', 'col'))) + max_indx_magn - wind_width - 1

points(max_indx, col = 'green')

### PS fitting
x0 <- 257; y0 <- 257 # why not 256 ?
rho = sqrt((max_indx[, 1] - x0) ^ 2 + (max_indx[, 2] - y0) ^ 2) # Distance from center to local max
theta <- atan2(abs(max_indx[, 2] - y0), max_indx[, 1] - x0)

xy_matrix <- matrix(1:512, ncol = 512, nrow = 512, byrow = T)
xy <- reshape2::melt(xy_matrix) %>% select(x = Var1, y = Var2) %>% as.matrix

## Polar coordinates
r <- sqrt((xy[ , 'x'] - x0) ^ 2 + (xy[ , 'y'] - y0) ^ 2)
phi <- atan2((xy[ , 'y'] - y0), (xy[ , 'x'] - x0)) # use atan2(y, x) not atan(y / x)
gradient <- 2 * pi * r * (rho / 512) * cos( (90 * pi / 180) - (phi - theta))
#plot(as.cimg(cos(matrix(gradient, 512, 512, byrow = T))))
#points(x0, y0, col = 'red') ; points(max_indx, col = 'green')

## PS annular zones
plot(as.cimg(PS_short^0.1))

PS_short_polar <- cbind(r, phi, z = reshape2::melt(PS_short)$value) %>% as.tibble() %>%
  filter(phi < pi / 2 & phi > -pi / 2) # Filter for using only right half of annulus

fit_func <- cbind(r, phi, z = cos(gradient)) %>% as.tibble() %>%
  filter(phi < pi / 2 & phi > -pi / 2) # Filter for using only right half of annulus

R_start <- 20 # First annulus (in pix)
R_end <- 200 # Last annulus (in pix)
dr <- 1 # Step (in pix)
#C <- vector("integer", R_end) # Contrast = alpha / beta
C <- NULL
for (R in seq(R_start, R_end, dr)) {
  r1 <- R; r2 <- r1 + dr
  annulus_PS <- PS_short_polar %>% filter(r < r2 & r > r1)
  annulus_fit_func <- fit_func %>% filter(r < r2 & r > r1)
  linear_fit <- lm(annulus_PS$z ~ annulus_fit_func$z)
  C <- c(C, linear_fit$coefficients[1] / linear_fit$coefficients[2]) # [R] vs c()
}

plot(C, type = 'l', ylim = c(-1000, 1000))

### Nonlinear fitting
#R <- 40; dr <- 1
r_phi <- cbind(r, phi) %>% as.tibble()
C_nln <- NULL; rho_tmp <- NULL; theta_tmp <- NULL
R_start <- 25 # First annulus (in pix)
R_end <- 60 # Last annulus (in pix)
dr <- 1 # Step (in pix)
for (R in seq(R_start, R_end, dr)) {
  r1 <- R; r2 <- r1 + dr
  annulus_PS <- PS_short_polar %>% filter(r < r2 & r > r1)
  annulus_fit_func <- fit_func %>% filter(r < r2 & r > r1)
  linear_fit <- lm(annulus_PS$z ~ annulus_fit_func$z)
  annulus_PS_z <- annulus_PS$z # avoid this!!!!!
  r_phi_n <- r_phi %>% filter(r < r2 & r > r1 & phi < pi / 2 & phi > -pi / 2)
  r_n <- r_phi_n$r
  phi_n <- r_phi_n$phi
  try(nonlinear_fit <- nls(annulus_PS_z ~ alpha + beta * cos( 2 * pi * r_n * (rho_n / 512) * cos( (90 * pi / 180) -
      (phi_n - theta_n))), start=list(alpha = linear_fit$coefficients[1], beta = linear_fit$coefficients[2],
      rho_n = rho, theta_n = theta)), silent = TRUE)
  C_nln <- c(C_nln, coef(nonlinear_fit)[1] / coef(nonlinear_fit)[2])
  rho_tmp <- c(rho_tmp, coef(nonlinear_fit)[3])
  theta_tmp <- c(theta_tmp, coef(nonlinear_fit)[4])
}


#coef(nonlinear_fit)
#fit_func <- alpha + beta * cos( 2 * pi * r * (rho / 512) * cos( (90 * pi / 180) - (phi - theta)))
plot(coef(nonlinear_fit)[1] + coef(nonlinear_fit)[2] * cos( 2 * pi * r_n * (coef(nonlinear_fit)[3] / 512) * cos( (90 * pi / 180) -
  (phi_n - coef(nonlinear_fit)[4]))), type = 'l', col = 'blue')

} # end of speckle_binary

### Useful functions, approaches, etc.
circle <- function(r_plot, color, half = FALSE){
  if(half == FALSE) {
  phi_plot <- seq(0, 2 * pi, length = 200)
  } else phi_plot <- seq(-pi / 2, pi / 2, length = 100)
  lines(x = r_plot * cos(phi_plot) + x0, y = r_plot * sin(phi_plot) + y0, col = color) # x0 & y0 here!
}

annulus_PS_model_plot <- function(R, dr = 1){
  r1 <- R; r2 <- r1 + dr
  annulus_PS <- PS_short_polar %>% filter(r < r2 & r > r1)
  annulus_fit_func <- fit_func %>% filter(r < r2 & r > r1)
  linear_fit <- lm(annulus_PS$z ~ annulus_fit_func$z)
  alpha <- linear_fit$coefficients[1] ; beta <-  linear_fit$coefficients[2]
  contrast <- alpha / beta
  print(c(contrast, alpha, beta))
  par(mfrow = c(2,2))
  plot(annulus_PS$z, type = 'l', col = 'red')
  plot(annulus_fit_func$z, type = 'l', col = 'blue')
  plot(as.cimg(PS_short^0.1))
#  circle(R, 'green', 1)
  points(x = annulus_PS$r * cos(annulus_PS$phi) + x0, y = annulus_PS$r * sin(annulus_PS$phi) + y0,
         pch = 46, col = 'green')
  text(400, 100, paste('R=', as.character(R), ', dr=', as.character(dr), sep = ''), col = 'white')
  annulus_PS_z <- annulus_PS$z # avoid this!!!!!
  r_phi_n <- r_phi %>% filter(r < r2 & r > r1 & phi < pi / 2 & phi > -pi / 2)
  r_n <- r_phi_n$r
  phi_n <- r_phi_n$phi
  nonlinear_fit <- nls(annulus_PS_z ~ alpha + beta * cos( 2 * pi * r_n * (rho_n / 512) * cos( (90 * pi / 180) - (phi_n - theta_n))),
                       start=list(alpha = linear_fit$coefficients[1], beta = linear_fit$coefficients[2], rho_n = rho, theta_n = theta))
  plot(annulus_PS$z, col = 'red', type = 'l')
  lines(coef(nonlinear_fit)[1] + coef(nonlinear_fit)[2] * cos( 2 * pi * r_n * (coef(nonlinear_fit)[3] / 512) * cos( (90 * pi / 180) -
    (phi_n - coef(nonlinear_fit)[4]))), col = 'blue')
#    plot(resid(linear_fit), type = 'l', col = 'red')
  par(mfrow = c(1,1))
}

#points(x = annulus_PS$r * cos(annulus_PS$phi) + x0, y = annulus_PS$r * sin(annulus_PS$phi) + y0,
#       pch = 46, col = 'green')

scale_550 <- 0.00878 # +/- 0.00005 "/pix (for observations in 2009)
scale_800 <- 0.00888 # +/- 0.00005 "/pix

#ls_fit <- lsfit(annulus_fit_func$z, annulus_PS$z) # vs lm() ?

#indx <- which(diff(sign(diff(ACF_short[(max_pos$x - wind_width):(max_pos$x + wind_width),
#                                       (max_pos$y - wind_width):(max_pos$y + wind_width)])))==-2, arr.ind = TRUE)+1
#x <- indx[, 1] + round(max_pos$x) - wind_width
#y <- indx[, 2] + round(max_pos$y) - wind_width
#z <- ACF_short[cbind(x, y)] # Remember this approach
#points(indx[, 1] + round(max_pos$x) - wind_width, indx[, 2] + round(max_pos$y) - wind_width, col = 'green')
