
#--------------------------------------------------------------------------------
#' SPE to dat converter
#'
#' Convert SPE to dat (i.e. removing 4100 byte SPE header)
#'
#' @param SPE_file A string.
#' @return Save converted dat file on the disk in the same folder.
#' @examples
#' # spe2dat(file.choose())
#' @export
spe2dat <- function(SPE_file) {
  dat_file <- tools::file_path_sans_ext(SPE_file)
  dat_file <- paste(dat_file, 'dat', sep = '.')
  system(sprintf("dd if=%s of=%s bs=4100 skip=1", SPE_file, dat_file))
  print(dat_file)
}

#--------------------------------------------------------------------------------
#' N frames from file
#'
#' Take N frames from series of 512 x 512 speckle images
#' and write to separate binary file.
#'
#' @param n A number.
#' @param start A number.
#' @return The array of \code{n} frames.
#' @examples
#' # n_frames(50) # take first 50 frames
#' # n_frames(15, 10) or
#' @export
n_frames <- function(n, start = 1) {
  input_file <- file.choose() # choose .dat file
  output_file <- tools::file_path_sans_ext(input_file)
  output_file <- paste(output_file, '_',as.character(n), '_frames.dat', sep = '')
  system(sprintf("dd if=%s of=%s bs=524288 count=%d skip=%d",
                 input_file, output_file, n, (start - 1)))
  print(output_file)
}
