#' Get selected speckle frame
#' Get specified speckle frame as matrix from file
#'
#' @param data_file a character string with the path name to a file.
#' @param frame an integer.
#' @param x_pix a number of x pixels (default is 512).
#' @param y_pix a number of y pixels (default is 512).
#' @param type a string 'int' or 'num'.
#' @param byte a number of bytes in pixel.
#' @return Matrix with given frame.
#' @examples
#' \dontrun{
#' # On Unix-like operating systems only
#' # Read frame number 2 from file to matrix
#' obj_filename <- system.file("extdata", "ads15182_550_2_frames.dat", package = "specklestar")
#' frame2 <- speckle_frame(obj_filename, 2)
#' }
#' @export
speckle_frame <- function(data_file = NULL, frame = 1, x_pix = 512, y_pix = 512, type = 'int', byte = 2) {
  if (is.null(data_file)) {
    data_file <- file.choose()
    print(data_file)
  }
  tmp_file <- paste(tempdir(), '/tmp.dat', sep = '')
  system(sprintf("dd if=%s of=%s bs=%d*%d*%d skip=%d count=1", data_file, tmp_file, x_pix, y_pix, byte, frame - 1),
         ignore.stderr = TRUE)

  file_connector = file(tmp_file, "rb")
  if (type == 'int') {
  frame <- readBin(file_connector, integer(), endian = "little", n = x_pix * y_pix, size = byte)
  } else {
    frame <- readBin(file_connector, numeric(), endian = "little", n = x_pix * y_pix, size = byte)
  }

  frame <- matrix(frame, x_pix, y_pix)

  close(file_connector)
  file.remove(tmp_file)

  return(frame)
}
