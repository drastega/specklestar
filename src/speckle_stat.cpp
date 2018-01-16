// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/density.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <stdio.h>
#include <cstdio>
#define IMAGE_SIZE (512 * 512)
using namespace Rcpp;

using namespace boost;
using namespace boost::accumulators;

typedef accumulator_set<unsigned short, features<tag::density> > acc;
typedef iterator_range<std::vector<std::pair<double, double> >::iterator > histogram_type;

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
  NumericMatrix histData(2, 65535);

  acc myAccumulator(tag::density::num_bins = 65535, tag::density::cache_size = 10);

  file.seekg(0, std::ios::beg);

  for(int f = 0; f < N_frame; f++) {
    file.read((char*)piData, IMAGE_SIZE * sizeof(unsigned short));
    for(int i = 0; i < IMAGE_SIZE; i++) {
      myAccumulator(piData[i]);
    }
  }
  file.close();

  histogram_type hist = density(myAccumulator);
  for(int i = 0; i < hist.size(); i++)
  {
    // std::cout << "Bin lower bound: " << hist[i].first << ", Value: " << hist[i].second << std::endl;
    histData[i*2 ] = hist[i].first;
    histData[i*2 + 1] = hist[i].second;
  }

  return histData;
}
