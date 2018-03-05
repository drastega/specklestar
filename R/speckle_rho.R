#' Rho from pixels to arcseconds
#'
#' Convert rho from pixels to arcseconds for given band
#'
#' @param rho_px rho in px
#' @param rho_px_err error of rho in px
#' @param band a character string with band ('550', 600', '676', '694', '800', '850')
#' @return List with rho and it's error in arcseconds.
#' @examples
#' speckle_rho(rho_px = 12.1685, rho_px_err = 0.0865, band = '800')
#' @export
speckle_rho <- function(rho_px = NULL, rho_px_err = NULL, band = NULL) {
  base_550 <- 0.00885267819
  base_550_err <- 5.16421857e-05

  q_600 <- 0.99651
  q_600_err <- 0.00030

  q_676 <- 0.99637
  q_676_err <- 0.00043

  q_694 <- 0.99556
  q_694_err <- 0.00054

  q_800 <- 0.98751
  q_800_err <- 0.00059

  q_850 <- 0.98982
  q_850_err <- 0.00078

  rho_sec_550 <- rho_px * base_550
  rho_sec_err_550 <- rho_sec_550 * (base_550_err / base_550 + rho_px_err / rho_px)

  if (band == '550') {
    rho_sec <- rho_sec_550
    rho_sec_err <- rho_sec_err_550
  }

  if (band == '600') {
    rho_sec <- rho_sec_550 * q_600
    rho_sec_err <- rho_sec * (rho_sec_err_550 / rho_sec_550 + q_600_err / q_600)
  }

  if (band == '676') {
    rho_sec <- rho_sec_550 * q_676
    rho_sec_err <- rho_sec * (rho_sec_err_550 / rho_sec_550 + q_676_err / q_676)
  }

  if (band == '694') {
    rho_sec <- rho_sec_550 * q_694
    rho_sec_err <- rho_sec * (rho_sec_err_550 / rho_sec_550 + q_694_err / q_694)
  }

  if (band == '800') {
    rho_sec <- rho_sec_550 * q_800
    rho_sec_err <- rho_sec * (rho_sec_err_550 / rho_sec_550 + q_800_err / q_800)
  }

  if (band == '850') {
    rho_sec <- rho_sec_550 * q_850
    rho_sec_err <- rho_sec * (rho_sec_err_550 / rho_sec_550 + q_850_err / q_850)
  }

  return(list(rho_sec = rho_sec, rho_sec_err = rho_sec_err))

}
