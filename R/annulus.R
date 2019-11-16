#' Annulus
#'
#' Get points from annulus on matrix
#'
#' @param im matrix.
#' @param R radius of annulus.
#' @param dR annulus width in pixels (default is 2 px).
#' @param center point of center c(x, y) (default is center of matrix c(nrow(im) / 2, ncol(im) / 2)).
#' @return Vector with points of annulus.
#' @examples
#' m <- matrix(1:512, nrow = 512, ncol = 512)
#' annulus_points <- annulus(m, R = 57, dR = 1, center = c(100, 100))
#' @export
annulus <- function(im, R, dR = 2, center = c(nrow(im) / 2, ncol(im) / 2)) {

  melted_image <- reshape2::melt(im)
  x <- melted_image[['Var1']]
  y <- melted_image[['Var2']]
  z = melted_image[['value']]

  x_0 <- x - center[1]
  y_0 <- y - center[2]

  r <- sqrt(x_0 ^ 2 + y_0 ^ 2)
  phi <- atan2(y_0, x_0)

  im_polar <- cbind(r, phi, z)
  annulus_pts <- im_polar[(im_polar[, 'r'] < R + dR & im_polar[, 'r'] > R), ][, 'z']

  return(annulus_pts)
}
