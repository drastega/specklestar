#' Detection limits from acf
#'
#' Calculate detection limits (dm for given rho) from empirical autocorrelation function
#' using formalism described in 2010AJ....139..743T (Tokovinin, Mason & Hartkopf, 2010)
#'
#' @param im matrix with acf.
#' @param band a character string '550', '600', '700', or '800'.
#' @param rho_lim limit in arc seconds (default is 1'', maximum is 2'').
#' @return List of x and y with limits (x is rho and y is dm).
#' @examples
#' obj_filename <- system.file("extdata", "ads15182_550_2_frames.dat", package = "specklestar")
#' pow_spec_diff <- speckle_ps_diff(obj_filename)
#' acf <- speckle_acf(pow_spec_diff)
#' lim <- speckle_limits(acf, '550')
#' @export
speckle_limits <- function(im, band, rho_lim = 1) {

  fluct <- vector()
  for (i in seq(1, 250, 2)) {
    annulus_points <- annulus(im, i)
    fluct <- c(fluct, sqrt(mean(annulus_points^2)))
  }

  scale_550 <- 0.00885267819
  scale_600 <- scale_550 * 0.99651
  scale_700 <- scale_550 * 0.9922462
  scale_800 <- scale_550 * 0.98751

  N <- ceiling(rho_lim * 58)

  if (band == '550') {
    x <- seq(1, N) * scale_550 * 2
  }

  if (band == '600') {
    x <- seq(1, N) * scale_600 * 2
  }

  if (band == '700') {
    x <- seq(1, N) * scale_700 * 2
  }

  if (band == '800') {
    x <- seq(1, N) * scale_800 * 2
  }

  y <- log(max(fluct[1:N]) / fluct[1:N])

  return(list(x = x, y = y))
}
