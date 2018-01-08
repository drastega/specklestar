# specklestar

Overview
--------

R package specklestar is intended for reduction of speckle data obtained from optical telescopes.
Input data now is a set of 512x512 (x2 bytes) speckle images of binary and multiple stars.

speckle_binary.R - for reduction of speckle data of binary stars using algorithm described in paper
["Differential photometry of speckle-interferometric binary and multiple stars"
Pluzhnik E.A., Astronomy and Astrophysics, v.431, p.587-596 (2005)](https://www.aanda.org/articles/aa/pdf/2005/08/aa1158.pdf)

speckle_generator.R - for generation of model speckle data

middle_frame.R - for calculation of middle dark or flat field images.

Almost all programs are still far from finish.
By now the only working program is speckle_generator.R
