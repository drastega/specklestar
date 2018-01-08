#include <Rcpp.h>
#include <fftw3.h>
#include <iostream>  
#include <fstream>
#define IMAGE_SIZE (512 * 512 * 2)
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector ps(String filename) {
std::ifstream file(filename, std::ios::binary);

file.seekg(0, std::ios::end);
size_t file_length = file.tellg();
int N_frame = file_length / IMAGE_SIZE;

char data[IMAGE_SIZE];
//char *pData = new char[IMAGE_SIZE];
file.seekg(0, std::ios::beg);
file.read(data, IMAGE_SIZE);
file.close();
unsigned short *piData = (unsigned short *)data;
//double dData[512*512]; 
NumericVector dData(512*512);

  for(int i = 0; i < IMAGE_SIZE / sizeof(unsigned short); i++) {
    dData[i] = (double)piData[i];
//    std::cout << i << ": " << dData[i] << " "; 
  }

//  fftw_complex *out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * 512 * (512 / 2 + 1));
//  fftw_plan p;
//  p = fftw_plan_dft_r2c_2d(512, 512, dData, out, FFTW_ESTIMATE);
//  fftw_execute(p); /* repeat as needed */
//  fftw_destroy_plan(p);
//  fftw_free(out);

  // delete [] pData;
  return dData;
}