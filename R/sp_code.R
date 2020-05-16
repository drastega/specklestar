#' Spectral code
#'
#' Return a number based on spectral type of star
#' (first two characters)
#'
#' @param spectr a character string with spectr type of star.
#' @return A number.
#' @examples
#' spectr_code <- sp_code('B9IVpSi')
#' @importFrom stringr str_replace_all
#' @export
sp_code <- function(spectr) {

  code <- str_replace_all(substr(spectr, 1, 2), c('O' = '0', 'B' = '1', 'A' = '2', 'F' = '3', 'G' = '4',
                                                  'K' = '5', 'M' = '6', 'L' = '7', 'T' = '8', 'Y' = '9'))
  return(code)
}
