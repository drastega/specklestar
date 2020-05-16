#' Visualization of speckle matrix
#'
#' @param speckle_matrix a speckle matrix to visualize (output of speckle_frame() function).
#' @return ggplot object.
#' @examples
#' \dontrun{
#' #' # On Unix-like operating systems only
#' # Read frame number 2 from file to matrix
#' obj_filename <- system.file("extdata", "ads15182_550_2_frames.dat", package = "specklestar")
#' frame2 <- speckle_frame(obj_filename, 2)
#' gg_speckle <- speckle_image(frame2)
#' }
#' @export
speckle_image <- function(speckle_matrix) {
  colnames(speckle_matrix) <- 1:512
  speckle_df <- as.data.frame(speckle_matrix)
  speckle_df$x <- 1:512
  speckle_df <- tidyr::pivot_longer(speckle_df, cols = -x, names_to = 'y')
  speckle_df <- dplyr::mutate(speckle_df, y = as.integer(y))

  gg_speckle <- ggplot2::ggplot(speckle_df, aes(x, y, fill = value)) +
    geom_raster()
  return(gg_speckle)
}

