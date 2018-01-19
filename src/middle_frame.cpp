#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include "image_helper.h"
using namespace Rcpp;

//' Middle frame
//'
//' Average image of the series of speckle images
//'
//' @param filename A string.
//' @return The 512 x 512 matrix of middle speckle image.
//' @examples
//' mf <- middle_frame(file.choose())
//'
//' # Plot
//' library(imager)
//' plot(as.cimg(mf))
//' @export
// [[Rcpp::export]]
NumericMatrix middle_frame(String filename) {
  std::ifstream file(filename, std::ios::binary);

  file.seekg(0, std::ios::end);
  size_t file_length = file.tellg();
  int N_frame = file_length / (sizeof(unsigned short) * IMAGE_SIZE);

  unsigned short piData[IMAGE_SIZE];
  NumericMatrix meanData(512, 512);

  file.seekg(0, std::ios::beg);
  for(int f = 0; f < N_frame; f++) {
    file.read((char*)piData, IMAGE_SIZE * sizeof(unsigned short));
    if (IsOverThresholdFrame(piData)) continue;

    for(int i = 0; i < IMAGE_SIZE; i++) {
      meanData[i] += piData[i];
    }
  }
  file.close();
  return meanData;
}
