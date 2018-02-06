#' Get selected speckle frame
#'
#' Get specified speckle frame as matrix from file
#'
#' @param data_file A string.
#' @param frame An integer.
#' @return 512 x 512 matrix with given frame
#' @examples
#' ## On Unix-like operating systems only
#' ## Read frame number 3 from file
#' # obj_filename <- system.file("extdata", "ads15182_550_5_frames.dat", package = "specklestar")
#' # frame3 <- speckle_frame(obj_filename, 3)
#' @export
speckle_frame <- function(data_file = file.choose(), frame = 1) {
  tmp_file <- tools::file_path_sans_ext(data_file)
  tmp_file <- paste(tmp_file, '_frame_', as.character(frame), '.dat', sep = '')
  system(sprintf("dd if=%s of=%s bs=512*512*2 skip=%d count=1", data_file, tmp_file, frame - 1),
         ignore.stderr = TRUE)

  file_connector = file(tmp_file, "rb")
  frame <- readBin(file_connector, integer(), endian = "little", n = 512 * 512, size = 2)
  frame <- matrix(frame, 512, 512)

  close(file_connector)
  file.remove(tmp_file)

  return(frame)
}
