#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector speckle_gen(double seeing, double speckle_sigma, double m1, double m2, double rho_x, double rho_y, double wind) {
  int N_speckle = 300;
  int n_x = 512;
  int n_y = n_x;
  
  double stellar_center_x = R::rnorm(n_x / 2, wind);
  double stellar_center_y = R::rnorm(n_y / 2, wind);
  double x0;
  double y0;
  NumericVector speckle_field(262144);
  NumericVector gaussian_2d(262144);
  
  for (int i = 0; i < N_speckle; i++) {
    x0 = R::rnorm(stellar_center_x, seeing) - rho_x / 2;
    y0 = R::rnorm(stellar_center_y, seeing) - rho_y / 2;
    for (int x = 0;  x < n_x; x++) {
      for (int y = 0; y < n_y; y++) {
        gaussian_2d[x * n_y + y] = m1 * exp(-(pow((x - x0), 2)/(2 * pow(speckle_sigma, 2)) + pow((y - y0), 2)/(2 * pow(speckle_sigma, 2)))) + \
          m2 * exp(-(pow((x - x0 - rho_x), 2)/(2 * pow(speckle_sigma, 2)) + pow((y - y0 - rho_y), 2)/(2 * pow(speckle_sigma, 2))));
      }
    }
    for (int c = 0 ; c < 262144 ; c++) {
      speckle_field[c] = speckle_field[c] + gaussian_2d[c];
    }
  }
    return speckle_field;
}
