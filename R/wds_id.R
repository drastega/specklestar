#' WDS index
#'
#' Return WDS index based on right ascension and declination
#'
#' @param ra a character string with right ascension.
#' @param dec a character string with declination.
#' @param delim a character string with delimiter used in both ra and dec.
#' @param name a character string with extra name.
#' @return A character with WDS index.
#' @examples
#' wds_indx1 <- wds_id('00 00 43.63', '+45 15 12.0')
#' wds_indx2 <- wds_id('16:37:44.07', '+19:56:55.1', ':', 'wds')
#' @export
wds_id <- function (ra, dec, delim = ' ', name = '')
{
  temphms <- stringr::str_split_fixed(ra, delim, 3)
  ra_h <- temphms[, 1]
  ra_m <- as.numeric(temphms[, 2])
  ra_s <- round(as.numeric(temphms[, 3]) / 6)
  ra_m <- ifelse(ra_s == 10, ra_m + 1, ra_m)
  ra_s <- ifelse(ra_s == 10, 0, ra_s)

  tempdms = stringr::str_split_fixed(dec, delim, 3)
  dec_d <- tempdms[, 1]
  dec_m <- as.numeric(tempdms[, 2])
  dec_s <- round(as.numeric(tempdms[, 3]) / 6)
  dec_m <- ifelse(dec_s > 5, dec_m + 1, dec_m)

  ra_h <- formatC(ra_h, width = 2, format = 'd', flag = '0')
  ra_m <- formatC(ra_m, width = 2, format = 'd', flag = '0')
  dec_d <- formatC(dec_d, width = 2, format = 'd', flag = '0')
  dec_m <- formatC(dec_m, width = 2, format = 'd', flag = '0')

  WDSname = paste0(name, ra_h, ra_m, ra_s, dec_d, dec_m)
  return(WDSname)
}
