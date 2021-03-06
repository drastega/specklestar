% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/speckle_frame.R
\name{speckle_frame}
\alias{speckle_frame}
\title{Selected frame from binary file}
\usage{
speckle_frame(
  data_file = NULL,
  frame = 1,
  x_pix = 512,
  y_pix = 512,
  type = "int",
  byte = 2
)
}
\arguments{
\item{data_file}{a character string with the path name to a file.}

\item{frame}{an integer.}

\item{x_pix}{a number of x pixels (default is 512).}

\item{y_pix}{a number of y pixels (default is 512).}

\item{type}{a string 'int' or 'num' (default is 'int').}

\item{byte}{a number of bytes in pixel (default is 2).}
}
\value{
Matrix with given frame.
}
\description{
Get specified frame as matrix from binary file
}
\examples{
\dontrun{
# On Unix-like operating systems only
# Read frame number 2 from file to matrix
obj_filename <- system.file("extdata", "ads15182_550_2_frames.dat", package = "specklestar")
frame2 <- speckle_frame(obj_filename, 2)
}
}
