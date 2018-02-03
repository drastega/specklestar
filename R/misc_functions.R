#' @useDynLib specklestar
#' @importFrom Rcpp sourceCpp
NULL

#--------------------------------------------------------------------------------
#' SPE to dat converter
#'
#' Convert SPE to dat (i.e. removing 4100 byte SPE header)
#'
#' @param SPE_file A string.
#' @return Save converted dat file on the disk in the same folder.
#' @examples
#' # spe2dat(file.choose())
#' @export
spe2dat <- function(SPE_file) {
  dat_file <- tools::file_path_sans_ext(SPE_file)
  dat_file <- paste(dat_file, 'dat', sep = '.')
  system(sprintf("dd if=%s of=%s bs=4100 skip=1", SPE_file, dat_file))
  print(dat_file)
}

#--------------------------------------------------------------------------------
#' N frames from file
#'
#' Take N frames from series of 512 x 512 speckle images
#' and write to separate binary file.
#'
#' @param n A number.
#' @param start A number.
#' @return The array of \code{n} frames.
#' @examples
#' # n_frames(50) # take first 50 frames
#' # n_frames(15, 10) or
#' @export
n_frames <- function(n, start = 1) {
  input_file <- file.choose() # choose .dat file
  output_file <- tools::file_path_sans_ext(input_file)
  output_file <- paste(output_file, '_',as.character(n), '_frames.dat', sep = '')
  system(sprintf("dd if=%s of=%s bs=524288 count=%d skip=%d",
                 input_file, output_file, n, (start - 1)))
  print(output_file)
}

#--------------------------------------------------------------------------------
#' Assemble spool of speckle images
#'
#' Convert spool of speckle images to single .dat file
#'
#' @return The .dat file.
#' @examples
#' # spool_assemble(50)
#' @export
spool_assemble <- function(){

}

#--------------------------------------------------------------------------------
circle <- function(r_plot, half = FALSE, color = 'red', x0 = 257, y0 = 257){
  if(half == FALSE) {
    phi_plot <- seq(0, 2 * pi, length = 200)
  } else phi_plot <- seq(-pi / 2, pi / 2, length = 100)
  lines(x = r_plot * cos(phi_plot) + x0, y = r_plot * sin(phi_plot) + y0, col = color)
}

#--------------------------------------------------------------------------------
annulus_PS_model_plot <- function(R, dr = 1){
  x0 <- 257; y0 <- 257
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
  plot(imager::as.cimg(PS_short^0.1))
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
