#include <Rcpp.h>
#include <math.h>
#include <complex>
#include <fftw3.h>
using namespace Rcpp;

//' Autocorrelation function calculation
//'
//' Autocorrelation function of power spectrum
//'
//' @param ps 513 x 1024 power spectrum double matrix.
//' @return The 513 x 1024 double matrix of ACF.
//' @examples
//' acf <- speckle_acf(ps)
//'
//' # Plot
//' library(imageviewer)
//' imageviewer(log10(acf))
//' @export
// [[Rcpp::export]]
NumericVector speckle_acf(NumericMatrix ps) {

  NumericMatrix acf(1024, 1024);
  std::vector<std::complex<double>> data(513 * 1024);

  for( int j = 0; j < 1024; j++ ){
    for( int i = 0; i < 513; i++ ){
      data[513 * j + i].real( ps(i, j) / (1024 * 1024) );
      data[513 * j + i].imag( 0 );
    }
  }

  fftw_complex *ps_fftw_cmplx = reinterpret_cast<fftw_complex*>(data.data());

    double *out = (double*) fftw_malloc(sizeof(double) * 1024 * 1024);

    fftw_plan p = fftw_plan_dft_c2r_2d(1024, 1024, ps_fftw_cmplx, out, FFTW_ESTIMATE);
    fftw_execute(p);
    fftw_destroy_plan(p);

    for (int i = 0; i < 1024 * 1024; i++) acf[i] = out[i];

    fftw_free(out);

  return acf;
}
