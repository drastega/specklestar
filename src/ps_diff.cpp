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
//' @return The 513 x 1024 double vector of power spectrum.
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
//  std::vector<double> big_dData(1024*1024);
  NumericMatrix big_dData(1024, 1024);
  NumericMatrix outData(513, 1024);

  fftw_complex *out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * 1024 * (1024 / 2 + 1));
  for(int j = 0; j < N_frame; j++){
    file.read((char*)piData1, IMAGE_SIZE * sizeof(unsigned short));
    if (IsOverThresholdFrame(piData1, threshold)) continue;
    file.read((char*)piData2, IMAGE_SIZE * sizeof(unsigned short));
    if (IsOverThresholdFrame(piData2, threshold)) continue;

    for(int i = 0; i < 512; i++) {
      for(int j = 0; j < 512; j++) {
        big_dData[j * 1024 + i] = (double)piData2[i * 512 + j] - (double)piData1[i * 512 + j];
      }
    }

    file.seekg(-IMAGE_SIZE * sizeof(unsigned short), std::ios_base::cur); // shift pointer one frame back

    fftw_plan p = fftw_plan_dft_r2c_2d(1024, 1024, big_dData.begin(), out, FFTW_ESTIMATE);
//    fftw_plan p = fftw_plan_dft_r2c_2d(1024, 1024, big_dData.data(), out, FFTW_ESTIMATE);
    fftw_execute(p); /* repeat as needed */
    fftw_destroy_plan(p);
    for (int i = 0; i < 1024 * 513; i++) outData[i] += out[i][0] * out[i][0] + out[i][1] * out[i][1];
  }
  fftw_free(out);
  file.close();

//  return NumericVector(big_dData.begin(), big_dData.end());
  return outData;
//  return big_dData;
}
