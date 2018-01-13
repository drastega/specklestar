#include <Rcpp.h>
#include <iostream>
#include <fstream>
#define IMAGE_SIZE (512 * 512 * 2)
using namespace Rcpp;

//' Middle frame
//'
//' Average image in the series of speckle images
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
  int N_frame = file_length / IMAGE_SIZE;

  char data[IMAGE_SIZE];
  unsigned short *piData = (unsigned short *)data;
  NumericVector dData(512*512);
  //NumericVector meanData(512*512);
  NumericMatrix meanData(512, 512);

  file.seekg(0, std::ios::beg);
  for(int f = 0; f < N_frame; f++) {
    file.read(data, IMAGE_SIZE);
    for(int i = 0; i < IMAGE_SIZE / sizeof(unsigned short); i++) {
      dData[i] = (double)piData[i];
      meanData[i] += dData[i];
    }
  }
  file.close();
  return meanData;
}
