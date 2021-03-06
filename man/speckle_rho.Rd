% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/speckle_rho.R
\name{speckle_rho}
\alias{speckle_rho}
\title{Rho from pixels to arcseconds}
\usage{
speckle_rho(rho_px = NULL, rho_px_err = NULL, band = NULL)
}
\arguments{
\item{rho_px}{rho in px.}

\item{rho_px_err}{error of rho in px.}

\item{band}{a character string with band ('550', 600', '676', '694', '800', '850').}
}
\value{
List with rho and it's error in arcseconds.
}
\description{
Convert rho from pixels to arcseconds for given band
}
\examples{
speckle_rho(rho_px = 12.1685, rho_px_err = 0.0865, band = '800')
}
