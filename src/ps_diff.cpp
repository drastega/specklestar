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
  size_t frameSize = IMAGE_SIZE * sizeof(unsigned short);

  std::ifstream file(filename, std::ios::binary);
  file.seekg(0, std::ios::end);
  size_t file_length = file.tellg();
  file.seekg(0, std::ios::beg);

  NumericMatrix outData(513, 1024);
  NumericMatrix big_dData(1024, 1024);

  int N_frame = file_length / frameSize;
  unsigned short piData1[IMAGE_SIZE]
               , piData2[IMAGE_SIZE]
               ;

  fftw_complex *out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * 1024 * (1024 / 2 + 1));

  // int state = 0;
  bool state = true;
  int j = 0;
  while( file.read((char*)piData1, frameSize) ) {
    if( !file ) break;
    j++;
    if (IsOverThresholdFrame(piData1, threshold)) continue;
    break;
  }
  for(; file && j < N_frame; j++) {
    file.read((char*)(state ? piData1 : piData2), frameSize);
    if( !file ) break;
    if (IsOverThresholdFrame((state ? piData1 : piData2), threshold)) continue;

    for(int i = 0; i < 512; i++) {
      for(int j = 0; j < 512; j++) {
        big_dData[j * 1024 + i] = (double)(state ? piData1 : piData2)[i * 512 + j]
                                - (double)(state ? piData2 : piData1)[i * 512 + j];
      }
    }
    fftw_plan p = fftw_plan_dft_r2c_2d(1024, 1024, big_dData.begin(), out, FFTW_ESTIMATE);
    fftw_execute(p); // repeat as needed
    fftw_destroy_plan(p);
    for (int i = 0; i < 1024 * 513; i++) outData[i] += out[i][0] * out[i][0] + out[i][1] * out[i][1];
    // state = 0 == state ? 1 : 0;
    state = !state;
  }
  fftw_free(out);
  file.close();

  Rcout << j << " averaged frames";

  return outData;
}
