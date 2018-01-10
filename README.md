# specklestar

Overview
--------

R package specklestar is a set of R an C++ (via Rcpp) functions used in the [**Group of high-resolution methods in astronomy**](https://www.sao.ru/Doc-en/index.html) of the Special Astrophysical Observatory
of the Russian Academy of Science for reduction of speckle data obtained from BTA 6-m telescope.
Input data are series of 512x512 (x2 bytes) speckle images.

For reduction of speckle data of binary and multiple stars we use algorithm described in paper
["Differential photometry of speckle-interferometric binary and multiple stars"
Pluzhnik E.A., Astronomy and Astrophysics, v.431, p.587-596 (2005)](https://www.aanda.org/articles/aa/pdf/2005/08/aa1158.pdf).

## Installation
install.packages("devtools")

devtools::install_github("drastega/specklestar")

## Functions

- n_frames(N) - take N frames from series of 512x512x2(bytes) speckle images
- spe2dat() - function for SPE to dat conversion (i.e. removing 4100 bytes SPE header)
