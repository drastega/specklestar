#include <Rcpp.h>
#include <fftw3.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <math.h>
#define IMAGE_SIZE (512 * 512 * 2)
using namespace Rcpp;

//' Power Spectrum calculation
//'
//' Power Spectrum of the series of speckle images
//'
//' @param filename A string.
//' @return The 512 x 512 double vector of Power Spectrum.
//' @examples
//' pow_spec <- ps(file.choose())
//'
//' # Plot
//' library(imager)
//' plot(as.cimg(pow_spec^0.01))
//' @export
// [[Rcpp::export]]
NumericVector ps(String filename) {

  std::ifstream file(filename, std::ios::binary);
  file.seekg(0, std::ios::end);
  size_t file_length = file.tellg();
  file.seekg(0, std::ios::beg);

  int N_frame = file_length / IMAGE_SIZE;
  char data[IMAGE_SIZE];
  unsigned short *piData = (unsigned short *)data;
  std::vector<double> dData(512*512);
  std::vector<double> outData(512*257);

  fftw_complex *out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * 512 * (512 / 2 + 1));
  for(int j = 0; j < N_frame; j++){
    file.read(data, IMAGE_SIZE);
    for(int i = 0; i < IMAGE_SIZE / sizeof(unsigned short); i++) dData[i] = (double)piData[i];

    fftw_plan p = fftw_plan_dft_r2c_2d(512, 512, dData.data(), out, FFTW_ESTIMATE);
    fftw_execute(p); /* repeat as needed */
    fftw_destroy_plan(p);
    for (int i = 0; i < 512 * 257; i++) outData[i] += sqrt(out[i][0] * out[i][0] + out[i][1] * out[i][1]);
  }
  fftw_free(out);
  file.close();

  return NumericVector(outData.begin(), outData.end());
}
