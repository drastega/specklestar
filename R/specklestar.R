#' specklestar: A package for reduction of speckle data.
#'
#' The specklestar package provides three categories of important functions:
#' foo, bar and baz.
#'
#' @section Foo functions:
#' The foo functions ...
#'
#' @docType package
#' @name specklestar
NULL

options(device = "quartz") # For OSX only
# options(device = "RStudioGD") # RStudio default graphics device

## Functions

#- n_frames(N) - take N frames from series of 512x512x2(bytes) speckle images
#- spe2dat(filename) - SPE to dat conversion (i.e. removing 4100 bytes SPE header)
#- middle_frame(filename) - calculate average image of the series of speckle images
#- ps(filename) - calculate power spectrum
#- speckle_generator()- generate speckle pattern of binary star
#- speckle_binary(filename) - Calculation rho, theta and dm for binary star
