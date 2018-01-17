#' Plot selected speckle frame
#'
#' Ploting specified speckle frame from file
#'
#' @param filename A string.
#' @return NULL.
#' @examples
#' # Plot frame number 57 from chosen file
#' speckle_frame(file.choose(), 57)
#' @export
speckle_frame <- function(data_file = file.choose(), frame = 1) {
  tmp_file <- tools::file_path_sans_ext(data_file)
  tmp_file <- paste(tmp_file, '_frame_', as.character(frame), '.dat', sep = '')
  print(tmp_file)
  system(sprintf("dd if=%s of=%s bs=512*512*2 skip=%d", data_file, tmp_file, frame - 1))

  file_connector = file(tmp_file, "rb")
  frame <- readBin(file_connector, integer(), endian = "little", n = 512 * 512, size = 2)
  frame <- matrix(frame, 512, 512)
  par(mar = c(0, 0, 0, 0))
  plot(imager::as.cimg(frame^0.1), axes = FALSE)
  par(mar = c(5.1, 4.1, 4.1, 2.1)) # set standard margin parameters

  close(file_connector)
  file.remove(tmp_file)

  return(invisible())
}