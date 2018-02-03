#include <fftw3.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <cstring>
#include <math.h>
#include <Rcpp.h>
#include "image_helper.h"
using namespace Rcpp;

//' Power spectrum calculation
//'
//' Power spectrum of the series of 512 x 512 speckle images
//'
//' @param filename A string.
//' @param dark 512 x 512 middle frame matrix.
//' @param flat 512 x 512 middle flat field matrix.
//' @param threshold An integer (default 50000).
//' @return The 513 x 1024 double vector of power spectrum.
//' @examples
//' ## Suppose we have midd_dark and midd_flat 512 x 512 matrices
//' # pow_spec <- ps(file.choose(), dark = midd_dark, flat = midd_flat)
//'
//' ## Plot
//' # library(imageviewer)
//' # imageviewer(log10(pow_spec))
//' @export
// [[Rcpp::export]]
NumericVector ps(String filename, NumericMatrix dark, NumericMatrix flat, std::size_t threshold = 50000) {

  std::ifstream file(filename.get_cstring(), std::ios::binary);
  file.seekg(0, std::ios::end);
  size_t file_length = file.tellg();
  file.seekg(0, std::ios::beg);

  int N_frame = file_length / (IMAGE_SIZE * sizeof(unsigned short));
  unsigned short piData[IMAGE_SIZE];
  memset(piData, 0, IMAGE_SIZE * sizeof(unsigned short));

  NumericMatrix outData(513, 1024);
  NumericMatrix big_dData(1024, 1024);

  fftw_complex *out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * 1024 * (1024 / 2 + 1));
  for(int j = 0; j < N_frame; j++) {
    file.read((char*)piData, IMAGE_SIZE * sizeof(unsigned short));
    if (IsOverThresholdFrame(piData, threshold)) continue;

    for(int i = 0; i < 512; i++) {
      for(int j = 0; j < 512; j++) {
        big_dData[j * 1024 + i] = ((double)piData[i * 512 + j] - dark[i * 512 + j]) / flat[i * 512 + j];
      }
    }

    fftw_plan p = fftw_plan_dft_r2c_2d(1024, 1024, big_dData.begin(), out, FFTW_ESTIMATE);
    fftw_execute(p);
    fftw_destroy_plan(p);
    for (int i = 0; i < 1024 * 513; i++) outData[i] += out[i][0] * out[i][0] + out[i][1] * out[i][1];
  }
  fftw_free(out);
  file.close();

  return outData;
}
