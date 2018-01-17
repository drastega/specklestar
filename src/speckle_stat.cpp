#include <Rcpp.h>
#include <iostream>
#include <fstream>
#define IMAGE_SIZE (512 * 512)
using namespace Rcpp;

//' Statistics of speckles
//'
//' Calculate statistics of speckles in the series of speckle images and filter "bad" frames
//'
//' @param filename A string.
//' @return The double vector of speckle statistics.
//' @examples
//' spec_stat <- speckle_stat(file.choose())
//'
//' # Plot
//' plot(speckle_stat)
//' @export
// [[Rcpp::export]]
NumericVector speckle_stat(String filename) {
  std::ifstream file(filename, std::ios::binary);

  file.seekg(0, std::ios::end);
  size_t file_length = file.tellg();
  int N_frame = file_length / (sizeof(unsigned short) * IMAGE_SIZE);

  unsigned short piData[IMAGE_SIZE];
  NumericVector histData(65535);

  file.seekg(0, std::ios::beg);

  for(int f = 0; f < N_frame; f++) {
    file.read((char*)piData, IMAGE_SIZE * sizeof(unsigned short));
    for(int i = 0; i < IMAGE_SIZE; i++) {
      histData[ piData[i] ]++;
    }
  }
  file.close();

  return histData;
}
