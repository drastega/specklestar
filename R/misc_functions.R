
#-------------------------------------------------------------------------------
#' SPE to dat converter
#'
#' Convert SPE to dat (i.e. removing 4100 byte SPE header) and save it on disk
#'
#' @param SPE_file a character string with the path name to SPE file.
#' @param dat_file a character string with the path name to output dat file.
#' @return Saved converted dat file on the disk.
#' @examples
#' \dontrun{
#' # On Unix-like operating systems only
#' # Convert choosed SPE file to dat and save it as your_file.dat
#' spe2dat(, "your_file.dat")
#' }
#' @export
spe2dat <- function(SPE_file = file.choose(), dat_file) {
  system(sprintf("dd if=%s of=%s bs=4100 skip=1", SPE_file, dat_file))
}

#-------------------------------------------------------------------------------
#' N frames from file
#'
#' Take N frames from series of 512 x 512 speckle images
#' and write them to separate binary file
#'
#' @param input_file a character string with the path name to input dat file.
#' @param output_file a character string with the path name to output dat file.
#' @param n a number of frames.
#' @param from a number of first frame.
#' @return The array of \code{n} frames.
#' @examples
#' \dontrun{
#' # On Unix-like operating systems only
#' # Write first 50 frames from choosed file to 50_frames.dat file
#' n_frames(, "50_frames.dat", 50)
#' }
#' @export
n_frames <- function(input_file = file.choose(), output_file, n, from = 1) {
  system(sprintf("dd if=%s of=%s bs=524288 count=%d skip=%d",
                 input_file, output_file, n, (from - 1)))
}
