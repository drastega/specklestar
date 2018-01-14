#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <math.h>
#define IMAGE_SIZE (512 * 512 * 2)
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
  file.seekg(0, std::ios::beg);

  int N_frame = file_length / IMAGE_SIZE;
  char data[IMAGE_SIZE];
  unsigned short *piData = (unsigned short *)data;
  std::vector<double> dData(512*512);
  std::vector<double> outData(512*257);

  file.close();

  return NumericVector(outData.begin(), outData.end());
}
