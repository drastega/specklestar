#' Observational journals
#'
#' Returns data frame of BTA observational journals
#'
#' @param logs_dir a character string of path to folder with observational journals.
#' @return Tibble with observational journals.
#' @examples
#' db <- obs_database()
#' @export
obs_database <- function(logs_dir = NULL) {
  library(tidyverse)

  if (is.null(logs_dir)) {
    logs_dir <- '/Volumes/E/Data/new_telescope_EDIT'
    print(logs_dir)
  }

  file_list <- list.files(path = logs_dir, recursive = TRUE, pattern = ".tel", full.names = T)

  print('Excluded files:')
  for (file in file_list) {
   # if (max(count.fields(file, sep = ' ')) != 22) print(c(file, max(count.fields(file, sep = ' '))))
   # if (min(count.fields(file, sep = ' ')) != 22) print(c(file, min(count.fields(file, sep = ' '))))
    if (max(count.fields(file, sep = ' ')) != 22) print(c(file, count.fields(file, sep = ' ')))
 	  if (min(count.fields(file, sep = ' ')) != 22) print(c(file, count.fields(file, sep = ' ')))
  }

  excluded_files <- c()
  for (file in file_list) {
    if (!all(count.fields(file, sep = ' ') == 22)) excluded_files <- c(excluded_files, file)
  }

  included_files <- file_list[!file_list %in% excluded_files]

  column_names <- c('Name', 'Alpha_h', 'Alpha_m', 'Alpha_s', 'Delta_d', 'Delta_m', 'Delta_s', 'Mtime', 'Stime', 'Z', 'Focus')

  database <- read.table(included_files[1], col.names = column_names)
  database$Date <- basename(tools::file_path_sans_ext(included_files[1]))

  for (file in included_files[-1]){
    night_database <- read.table(file, col.names = column_names)
    night_database$Date <- basename(tools::file_path_sans_ext(file))
    database <- rbind(database, night_database)
  }

  BTA_latitude_rad <- 0.761779

  database <- database %>%
    mutate(Name = as.character(Name)) %>%

    mutate(Alpha_h = as.integer(str_replace(Alpha_h, 'Alpha=', ''))) %>%
    mutate(Alpha = paste(Alpha_h, Alpha_m, Alpha_s, sep = ':')) %>%
    mutate(Alpha_deg = celestial::hms2deg(Alpha)) %>%
    mutate(Alpha_rad = Alpha_deg * pi / 180) %>%

    mutate(Delta_d = str_replace(Delta_d, 'Delta=', '')) %>%
    mutate(Delta = paste(Delta_d, Delta_m, Delta_s, sep = ':')) %>%
    mutate(Delta_deg = celestial::dms2deg(Delta)) %>%
    mutate(Delta_rad = Delta_deg * pi / 180) %>%

    mutate(Stime = str_replace(Stime, 'Stime=', '')) %>%
    mutate(Stime_s = as.integer(as.integer(str_sub(Stime, start = -1)) * 6)) %>%
    mutate(Stime = str_sub(Stime, start = 1, end = 5)) %>%
    mutate(Stime = paste(Stime, Stime_s, sep = ':')) %>%
    mutate(Stime = str_replace(Stime, '60:0', '59:59')) %>%
    mutate(Stime_rad = celestial::hms2deg(Stime) * pi / 180) %>%

    mutate(Mtime = str_replace(Mtime, 'Mtime=', '')) %>%
    mutate(Mtime = str_replace(Mtime, '60.0', '59.9')) %>%
    mutate(Mtime_s = as.integer(as.integer(str_sub(Mtime, start = -1)) * 6)) %>%
    mutate(Mtime = str_sub(Mtime, start = 1, end = 5)) %>%
    mutate(Mtime = paste(Mtime, Mtime_s, sep = ':')) %>%

    mutate(Mdate_time = paste(Date, Mtime, sep = ' ')) %>%
    mutate(Mdate_time = lubridate::dmy_hms(Mdate_time)) %>%

    mutate(Z = str_replace(Z, 'Z=', '')) %>%
    mutate(Focus = str_replace(Focus, 'Focus=', '')) %>%
    mutate(Date = lubridate::dmy(Date)) %>%

    select(c(Name, Alpha, Delta, Mtime, Stime, Date, Z, Focus, Alpha_deg, Delta_deg, Mdate_time)) %>%

#    group_by(Name) %>%
#    mutate(n = n()) %>%
#    filter(!str_detect(Name, "dark*")) %>%
#    filter(!str_detect(Name, "flat*")) %>%
#    arrange(desc(n)) %>%

    as.tibble()

  return(database)
}
