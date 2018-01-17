#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <vector>
#include "image_helper.h"
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
List speckle_stat(String filename, std::size_t threshold = 50000) {
  std::ifstream file(filename, std::ios::binary);

  file.seekg(0, std::ios::end);
  size_t file_length = file.tellg();
  int N_frame = file_length / (sizeof(unsigned short) * IMAGE_SIZE);

  unsigned short piData[IMAGE_SIZE];
  std::vector<unsigned short> badFrames;

  file.seekg(0, std::ios::beg);

  for(int f = 0; f < N_frame; f++) {
    file.read((char*)piData, IMAGE_SIZE * sizeof(unsigned short));
    if (IsOverThresholdFrame(piData))
        badFrames.push_back(f + 1);
  }
  file.close();

//  return NumericVector(badFrames.begin(), badFrames.end());
  return List::create(Named("badFrames") = badFrames,
                      Named("hist") = badFrames);
}
