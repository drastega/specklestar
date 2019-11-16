#' Query from Simbad
#'
#' Returns data.frame with data from Simbad
#'
#' @param objects a character vector with objects.
#' @param params a character vector with parameters
#' (See http://simbad.u-strasbg.fr/simbad/sim-help?Page=sim-fscript#Formats).
#' @param col_names a character vector with column names
#' (the same lenght as params).
#' @return data.frame with data from Simbad.
#' @examples
#' simbad_data <- simbad_query(c('HD 6757', 'HIP 11569'),
#'                             c('%COO(A)', '%COO(D)', '%FLUXLIST(V;F)'),
#'                             c('RA', 'DEC', 'V'))
#' @export
simbad_query <- function(objects = NULL, params = NULL, col_names = NULL) {

  base_sim_script_url <- 'http://simbad.u-strasbg.fr/simbad/sim-script'

  objects <- paste0(objects, collapse = '\n')
  params <- paste0(params, collapse = ';')

  get_data <- httr::GET(base_sim_script_url,
                        query = list(script = paste0('format object \"', params, '\"\n',
                                                     "output console=off script=off\n", objects)))

  simbad_data <- httr::content(get_data)
  simbad_data <- str_split(simbad_data, '\n')
  simbad_data <- unlist(simbad_data)
  simbad_data <- simbad_data[-1:-6]
  simbad_data <- simbad_data[simbad_data != '']
  simbad_data <- data.frame(data = simbad_data, stringsAsFactors = FALSE)
  simbad_data <- separate(simbad_data, data, col_names, sep = ';', convert = TRUE)

  return(simbad_data)
}
