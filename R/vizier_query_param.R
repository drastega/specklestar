#' VizieR query by constrain on parameter
#'
#' Returns data.frame of selected table from VizieR
#'
#' @param vizier_table a character string with name of VizieR table.
#' @param par a character string with parameter.
#' @param constrain a character string with constrain.
#' @return data.frame with data.
#' @details http://vizier.u-strasbg.fr/vizier/doc/asu-summary.htx
#'
#' https://vizier.u-strasbg.fr/vizier/vizHelp/args.htx
#' @examples
#' vizier_data <- vizier_query_param('J/other/AstBu/63.278/table1', 'dm', '<2')
#' vizier_wds <- vizier_query_param('J/other/AstBu/63.278/table1', 'Name', c('G76-21', 'G63-46'))
#' @export
vizier_query_param <- function(vizier_table = NULL, par = NULL, constrain = NULL) {

  base_vizier_url <- 'http://vizier.u-strasbg.fr/viz-bin/asu-tsv'

  if (length(constrain) > 1) {
    constrain <- constrain %>% paste0(collapse = ';') %>% paste0('<<;', .)
  }

  character.vector <- c('-source', par, '-out')
  values.vector <- c(vizier_table, constrain, '**')

  query_list <- as.list(setNames(values.vector, character.vector))

  vizier_response <- httr::GET(base_vizier_url, query = query_list)

  if (vizier_response$status_code != 200) print('####### Bad request #######')

  data_vizier_df <- vizier_response %>% httr::content(as = 'text') %>% str_split('\n') %>%
    unlist %>% data.frame(Data = .) %>% filter(grepl('^[^#]', Data)) %>% filter(Data != '') %>%
    distinct()


###
  # data_vizier_df <- httr::content(vizier_response, as = 'text')
  # data_vizier_df <- str_split(data_vizier_df, '\n')
  # data_vizier_df <- unlist(data_vizier_df)
  # data_vizier_df <- as.data.frame(data_vizier_df)
  # data_vizier_df <- data_vizier_df[]

###



  n_columns <- data_vizier_df[4, ] %>% str_split('\t') %>% unlist %>% length()

  data_vizier_df <- data_vizier_df %>%
    separate(Data, as.character(1 : n_columns), sep = '\t')

  colnames(data_vizier_df) <- data_vizier_df[1, ]

  data_vizier_df <- data_vizier_df %>% slice(-1 : -3)

  data_vizier_df <- data_vizier_df %>% mutate_all(str_squish)

  return(data_vizier_df)
}
