#' Annulus
#'
#' Get points from annulus on matrix
#'
#' @param im matrix.
#' @param R radius of annulus.
#' @param dR annulus width in pixels (default is 2 px).
#' @param center point of center c(x, y) (default is center of matrix).
#' @return Vector with points of annulus.
#' @examples
#' m <- matrix(1:512, nrow = 512, ncol = 512)
#' annulus_points <- annulus(m, R = 57, dR = 1, center = c(100, 100))
#' @export
annulus <- function(im, R, dR = 2, center = NULL) {
  if(is.null(center)) {
    center <- nrow(im) / 2 # Check nrow and ncol
    center <- c(center, ncol(im) / 2)
  }

  xy <- select(reshape2::melt(im), x = Var1, y = Var2)
  xy <- as.matrix(xy)
  r <- sqrt((xy[ , 'x'] - center[1]) ^ 2 + (xy[ , 'y'] - center[2]) ^ 2)
  phi <- atan2((xy[ , 'y'] - center[2]), (xy[ , 'x'] - center[1]))

  im_polar <- cbind(r, phi, z = reshape2::melt(im)$value) %>% as.tibble()
  annulus_pts <- im_polar %>% filter(r < R + dR & r > R)

  return(annulus_pts)
}
