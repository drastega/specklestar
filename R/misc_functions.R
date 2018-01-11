#' @useDynLib specklestar
#' @importFrom Rcpp sourceCpp
NULL

#' @export
# SPE to dat conversion (i.e. removing 4100 byte SPE header)
spe2dat <- function() {
  input_file <- file.choose() # choose .SPE file
  output_file <- tools::file_path_sans_ext(input_file)
  output_file <- paste(output_file, 'dat', sep = '.')
  system(sprintf("dd if=%s of=%s bs=4100 skip=1", input_file, output_file))
  print(output_file)
}

#' TTT
#'
#' Take N frames from series of speckle images 512x512x2(bytes)
#'
#' @param n A number.
#' @param start A number.
#' @return The array of \code{n} frames.
#' @examples
#' n_frames(50) # take first 50 frames
#' n_frames(15, 10) or
#' @export
n_frames <- function(n, start = 1) {
  input_file <- file.choose() # choose .dat file
  output_file <- tools::file_path_sans_ext(input_file)
  output_file <- paste(output_file, '_',as.character(n), '_frames.dat', sep = '')
  system(sprintf("dd if=%s of=%s bs=524288 count=%d skip=%d",
                 input_file, output_file, n, (start - 1)))
  print(output_file)
}
