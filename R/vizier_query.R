#' VizieR query
#'
#' Returns tibble of selected table from VizieR
#'
#' @param vizier_table a character string with name of VizieR table.
#' @return Tibble with data.
#' @examples
#' vizier_data <- vizier_query('J/other/AstBu/63.278/table1')
#' @export
vizier_query <- function(vizier_table = NULL) {
  library(httr)

  base_vizier_url <- 'http://vizier.u-strasbg.fr/viz-bin/asu-tsv'

  vizier_response <- GET(base_vizier_url, query = list('-source' = vizier_table))

  if (vizier_response$status_code != 200) print('####### Bad request #######')

  data_vizier_tbbl <- vizier_response %>% content(as = 'text') %>% str_split('\n') %>%
    unlist %>% tibble(Data = .) %>% filter(grepl('^[^#]', Data)) %>% filter(Data != '')

  n_columns <- data_vizier_tbbl[4,] %>% str_split('\t') %>% unlist %>% length()

  data_vizier_tbbl <- data_vizier_tbbl %>%
    separate(Data, as.character(1:n_columns), sep = '\t')

  colnames(data_vizier_tbbl) <- data_vizier_tbbl[1,]

  data_vizier_tbbl <- data_vizier_tbbl %>% slice(-1:-3)

  return(data_vizier_tbbl)
}
