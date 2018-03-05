#' Detection limits from power spectrum
#'
#' Calculate detection limits (dm for given rho) from empirical power spectrum
#'
#' @param ps matrix with power spectrum.
#' @return Vector with limits.
#' @examples
#' obj_filename <- system.file("extdata", "ads15182_550_2_frames.dat", package = "specklestar")
#' pow_spec_diff <- speckle_ps_diff(obj_filename)
#' profile <- speckle_limits(ps = pow_spec_diff)
#' @export
speckle_limits <- function(ps) {
  profile <- wvtool::integ.profile(ps, axis = 'V', h = c(1, nrow(ps)) , v = c(1, ncol(ps)), disp = TRUE)
}
