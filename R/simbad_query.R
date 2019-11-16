#' Query from Simbad
#'
#' Returns tibble with data from Simbad
#'
#' @param objects a character vector with objects.
#' @param params a character vector with parameters
#' (See http://simbad.u-strasbg.fr/simbad/sim-help?Page=sim-fscript#Formats).
#' @param col_names a character vector with column names
#' (the same lenght as params).
#' @return Tibble with data from Simbad.
#' @examples
#' simbad_data <- simbad_query(c('HD 6757', 'HIP 11569'),
#'                             c('%COO(A)', '%COO(D)', '%FLUXLIST(V;F)'),
#'                             c('RA', 'DEC', 'V'))
#' @export
simbad_query <- function(objects = NULL, params = NULL, col_names = NULL) {

  base_sim_script_url <- 'http://simbad.u-strasbg.fr/simbad/sim-script'

  objects <- objects %>% paste0(collapse = '\n')
  params <- params %>% paste0(collapse = ';')

  get_data <- httr::GET(base_sim_script_url,
                        query = list(script = paste0('format object \"', params, '\"\n',
                                                     "output console=off script=off\n", objects)))

  simbad_data <- get_data %>% httr::content() %>% str_split('\n') %>% unlist() %>% tibble(Data = .) %>%
    slice(-1:-6) %>% filter(Data != '') %>% mutate(Data = str_squish(Data)) %>%
    separate(Data, col_names, sep = ';')

  return(simbad_data)
}
