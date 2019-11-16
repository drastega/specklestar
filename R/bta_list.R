#' Prepare data for observations on BTA
#' with bta_list program
#'
#' Returns data.frame with objects formatted for bta_list
#'
#' @param objects a character vector with objects.
#' @param epoch a character with epoch (default is '01-Jan-2000').
#' @return data.frame with data formatted for bta_list.
#' @examples
#' bta_list_data <- bta_list(c('HD 6757', 'HIP 11569'))
#' @export
bta_list <- function(objects = NULL, epoch = '01-Jan-2000') {

  base_sim_script_url <- 'http://simbad.u-strasbg.fr/simbad/sim-script'

  objects <- paste0(objects, collapse = '\n')

  get_data <- httr::GET(base_sim_script_url,
                  query = list(script = paste0("format object \"%COO(A) : %COO(D) ; ", epoch, " # %OBJECT %FLUXLIST(V;F)\"\n",
                                               "output console=off script=off\n", objects)))

  bta_list_data <- httr::content(get_data)
  bta_list_data <- str_split(bta_list_data, '\n')
  bta_list_data <- unlist(bta_list_data)
  bta_list_data <- bta_list_data[-1:-6]
  bta_list_data <- bta_list_data[bta_list_data != '']

  return(as.data.frame(bta_list_data))
}
