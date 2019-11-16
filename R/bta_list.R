#' Prepare data for observations on BTA
#' with bta_list program
#'
#' Returns tibble with objects formatted for bta_list
#'
#' @param objects a character vector with objects.
#' @return Tibble with data formatted for bta_list.
#' @examples
#' bta_list_data <- bta_list(c('HD 6757', 'HIP 11569'))
#' @export
bta_list <- function(objects = NULL) {

  base_sim_script_url <- 'http://simbad.u-strasbg.fr/simbad/sim-script'

  objects <- objects %>% paste0(collapse = '\n')

  get_data <- httr::GET(base_sim_script_url,
                  query = list(script = paste0("format object \"%COO(A) : %COO(D) ; 01-Jan-2000 # %OBJECT %FLUXLIST(V;F)\"\n",
                                               "output console=off script=off\n", objects)))

  bta_list <- get_data %>% httr::content() %>% str_split('\n') %>% unlist() %>% tibble(Data = .) %>% slice(-1:-6) %>%
    filter(Data != '') %>% mutate(Data = str_squish(Data))

  return(bta_list)
}
