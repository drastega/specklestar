// We can now use the BH package
// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <gsl/gsl_histogram.h>
#include <boost/math/common_factor.hpp>
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
  int N_frame = file_length / IMAGE_SIZE;

  char data[IMAGE_SIZE];
  unsigned short *piData = (unsigned short *)data;
  NumericVector dData(512*512);
  NumericVector meanData(512*512);

  file.seekg(0, std::ios::beg);
  gsl_histogram * h = gsl_histogram_alloc (65536);

//  for(int f = 0; f < N_frame; f++) {
    file.read(data, IMAGE_SIZE);
    for(int i = 0; i < IMAGE_SIZE / sizeof(unsigned short); i++) {
      dData[i] = (double)piData[i];
      meanData[i] += dData[i];
    }
//  }
  gsl_histogram_free (h);
  file.close();

  return meanData;
}
