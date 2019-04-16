#' Spectral code
#'
#' Return a number based on spectral type of star
#' (first two characters)
#'
#' @param spectr a character string with spectr type of star.
#' @return A number.
#' @examples
#' spectr_code <- sp_code('B9IVpSi')
#' @export
sp_code <- function(spectr)
{
  library(tidyverse)
  tibble_for_manipulation <- tibble(SP_code = spectr) %>%
    mutate(SP_code = str_sub(SP_code, 1, 2)) %>%
    mutate(SP_code = str_replace_all(SP_code, c('O' = '0', 'B' = '1', 'A' = '2', 'F' = '3', 'G' = '4',
                                                'K' = '5', 'M' = '6', 'L' = '7', 'T' = '8', 'Y' = '9'))) %>%
    mutate(SP_code = as.numeric(SP_code))

  return(tibble_for_manipulation$SP_code)
}
