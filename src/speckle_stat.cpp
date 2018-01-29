#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <vector>
#include "image_helper.h"
using namespace Rcpp;

//' Statistics of speckles
//'
//' Calculate statistics of speckles in the series of 512 x 512
//' speckle images and filter "bad" frames
//'
//' @param filename A string with name of file.
//' @param threshold Int.
//' @return The list with 2 elements 'badFrames' and 'hist': \cr
//' 1 number of bad frames, \cr
//' 2 double vector of speckle statistics.
//' @examples
//' spec_stat <- speckle_stat(file.choose())
//'
//' # Plot
//' plot(speckle_stat$hist, type = 'l')
//' @export
// [[Rcpp::export]]
List speckle_stat(String filename, std::size_t threshold = 50000) {
  std::ifstream file(filename, std::ios::binary);

  file.seekg(0, std::ios::end);
  size_t file_length = file.tellg();
  int N_frame = file_length / (sizeof(unsigned short) * IMAGE_SIZE);

  unsigned short piData[IMAGE_SIZE];
  std::vector<unsigned short> badFrames;
  std::vector<long> histData(65535);

  file.seekg(0, std::ios::beg);

  for(int f = 0; f < N_frame; f++) {
    file.read((char*)piData, IMAGE_SIZE * sizeof(unsigned short));
    for(int i = 0; i < IMAGE_SIZE; i++) {
      histData[piData[i]]++;
    }

    if (IsOverThresholdFrame(piData, threshold))
        badFrames.push_back(f + 1);
  }

  file.close();

  return List::create(Named("badFrames") = badFrames,
                      Named("hist") = histData);
}
