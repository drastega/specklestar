#' Calculate parallactic angles from BTA log file
#'
#' @param log_file a character string with the path name to a log file.
#' @param log_date a character string with date of observations (YYYY-MM-DD).
#' @return Tibble with names and parallactic angles calculated from BTA log file.
#' @examples
#' log_filename <- system.file("extdata", "2010-02-27.tel", package = "specklestar")
#' par_angle(log_filename, '2010-02-27')
#' @export
par_angle <- function(log_file = file.choose(), log_date) {
  library(tidyverse)
  library(celestial)

  column_names <- c('Name', 'Alpha_h', 'Alpha_m', 'Alpha_s', 'Delta_d', 'Delta_m', 'Delta_s', 'Mtime', 'Stime', 'z', 'Focus')

  log_data <- read.table(log_file, col.names = column_names)

  log_data <- log_data %>% na.omit()

  BTA_latitude_rad <- 0.761894396137 # +43 deg 39' 12''

  log_data <- log_data %>%
    mutate(Alpha_h = as.integer(str_replace(Alpha_h, 'Alpha=', ''))) %>%
    mutate(Alpha = paste(Alpha_h, Alpha_m, Alpha_s, sep = ':')) %>%
    mutate(Alpha_rad = hms2deg(Alpha) * pi / 180) %>%

    mutate(Delta_d = str_replace(Delta_d, 'Delta=', '')) %>%
    mutate(Delta_rad = paste(Delta_d, Delta_m, Delta_s, sep = ':')) %>%
    mutate(Delta_rad = dms2deg(Delta_rad) * pi / 180) %>%

    mutate(Stime = str_replace(Stime, 'Stime=', '')) %>%
    mutate(Stime_s = as.integer(as.integer(str_sub(Stime, start = -1)) * 6)) %>%
    mutate(Stime = str_sub(Stime, start = 1, end = 5)) %>%
    mutate(Stime = paste(Stime, Stime_s, sep = ':')) %>%
    mutate(Stime_rad = hms2deg(Stime) * pi / 180) %>%

    mutate(Sdate = paste(log_date, Stime, sep = ' ')) %>%
    mutate(Sdate = lubridate::ymd_hms(Sdate)) %>%

    group_by(Name) %>% # vulnerable place!
    mutate(mean_Sdate = mean(Sdate)) %>%
    mutate(mean_Alpha_rad = mean(Alpha_rad)) %>%
    mutate(mean_Delta_rad = mean(Delta_rad)) %>%
    mutate(mean_Stime_rad = mean(Stime_rad)) %>%

    mutate(Hour_angle_rad = mean_Stime_rad - mean_Alpha_rad) %>%

    mutate(Q_degr = atan(sin(Hour_angle_rad) / (tan(BTA_latitude_rad) * cos(mean_Delta_rad) -
                                     sin(mean_Delta_rad) * cos(Hour_angle_rad))) * 180 / pi) %>%

    select(c(Name, Q_degr)) %>%
    distinct()

  return(log_data)
}
