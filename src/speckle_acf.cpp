#include <Rcpp.h>
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
  NumericMatrix big_ps(1024, 1024);
  NumericMatrix acf(513, 1024);

  for(int i = 0; i < 1024; i++) {
    for(int j = 511; j < 1024; j++) {
      big_ps[j * 1024 + i] = ps[i * 513 + j];
    }
  }

  fftw_complex *out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * 1024 * (1024 / 2 + 1));

  fftw_plan p = fftw_plan_dft_r2c_2d(1024, 1024, big_ps.begin(), out, FFTW_BACKWARD);
  fftw_execute(p); /* repeat as needed */
  fftw_destroy_plan(p);

  for (int i = 0; i < 1024 * 513; i++) acf[i] += out[i][0] * out[i][0] + out[i][1] * out[i][1];

  fftw_free(out);

//  return acf;
  return big_ps;
}
