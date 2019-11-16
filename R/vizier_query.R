#' VizieR query
#'
#' Returns data.frame of selected table from VizieR
#'
#' @param vizier_table a character string with name of VizieR table.
#' @param objects a character vector with objects.
#' @param radius_arcsec raduis in arc seconds (number).
#' @return data.frame with data.
#' @details http://vizier.u-strasbg.fr/vizier/doc/asu-summary.htx
#'
#' https://vizier.u-strasbg.fr/vizier/vizHelp/args.htx
#' @examples
#' vizier_data <- vizier_query('J/other/AstBu/63.278/table1')
#' vizier_wds <- vizier_query('B/wds/wds', c('HD 6757', 'HIP 11569'), 5)
#' @export
vizier_query <- function(vizier_table = NULL, objects = NULL, radius_arcsec = NULL) {

  base_vizier_url <- 'http://vizier.u-strasbg.fr/viz-bin/asu-tsv'

  if (!is.null(objects)) objects <- objects %>% paste0(collapse = ';') %>% paste0('<<;', .)
  out.add_1 <- if(is.null(objects)) NULL else {'_1'}
  out.add_r <- if(is.null(objects)) NULL else {'_r'}

  vizier_response <- httr::GET(base_vizier_url, query = list('-source' = vizier_table,
                                                             '-c' = objects,
                                                             '-c.rs' = radius_arcsec,
                                                             '-out.add' = out.add_r,
                                                             '-out.add' = out.add_1,
                                                             '-out' = '**'))

  if (vizier_response$status_code != 200) print('####### Bad request #######')

  data_vizier_df <- vizier_response %>% httr::content(as = 'text') %>% str_split('\n') %>%
    unlist %>% data.frame(Data = .) %>% filter(grepl('^[^#]', Data)) %>% filter(Data != '') %>%
    distinct()

  n_columns <- data_vizier_df[4,] %>% str_split('\t') %>% unlist %>% length()

  data_vizier_df <- data_vizier_df %>%
    separate(Data, as.character(1:n_columns), sep = '\t')

  colnames(data_vizier_df) <- data_vizier_df[1,]

  data_vizier_df <- data_vizier_df %>% slice(-1 : -3)

  if (!is.null(objects)) {
    data_vizier_df <- data_vizier_df %>%
      mutate(`_1` = str_squish(`_1`)) %>%
      mutate(`_r` = as.numeric(`_r`))
  }

  if ('Simbad' %in% names(data_vizier_df)) {
  data_vizier_df <- data_vizier_df %>% select(-Simbad)
  }

  data_vizier_df <- data_vizier_df %>% mutate_all(str_squish)

  return(data_vizier_df)
}
