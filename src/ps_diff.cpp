#include <Rcpp.h>
#include <fftw3.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <math.h>
#include "image_helper.h"
using namespace Rcpp;

//' Power spectrum calculation
//'
//' Power spectrum of the difference of neighboring frames
//' in the series of speckle images
//'
//' @param filename A string.
//' @return The 257 x 512 double vector of power spectrum.
//' @examples
//' pow_spec_diff <- ps_diff(file.choose())
//'
//' # Plot
//' library(imager)
//' plot(as.cimg(pow_spec^0.01))
//' @export
// [[Rcpp::export]]
NumericVector ps_diff(String filename, std::size_t threshold = 50000) {

  std::ifstream file(filename, std::ios::binary);
  file.seekg(0, std::ios::end);
  size_t file_length = file.tellg();
  file.seekg(0, std::ios::beg);

  int N_frame = file_length / (IMAGE_SIZE * sizeof(unsigned short));
  unsigned short piData1[IMAGE_SIZE];
  unsigned short piData2[IMAGE_SIZE];
  std::vector<double> dData(512*512);
  std::vector<double> outData(512*257);

  fftw_complex *out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * 512 * (512 / 2 + 1));
  for(int j = 0; j < N_frame; j++){
    file.read((char*)piData1, IMAGE_SIZE * sizeof(unsigned short));
    if (IsOverThresholdFrame(piData1, threshold)) continue;
    file.read((char*)piData2, IMAGE_SIZE * sizeof(unsigned short));
    if (IsOverThresholdFrame(piData2, threshold)) continue;

    file.seekg(-IMAGE_SIZE * sizeof(unsigned short), std::ios_base::cur); // shift pointer one frame back

    for(int i = 0; i < IMAGE_SIZE / sizeof(unsigned short); i++) dData[i] = (double)piData2[i] - (double)piData1[i];

    fftw_plan p = fftw_plan_dft_r2c_2d(512, 512, dData.data(), out, FFTW_ESTIMATE);
    fftw_execute(p); /* repeat as needed */
    fftw_destroy_plan(p);
    for (int i = 0; i < 512 * 257; i++) outData[i] += out[i][0] * out[i][0] + out[i][1] * out[i][1];
  }
  fftw_free(out);
  file.close();

  return NumericVector(outData.begin(), outData.end());
}
