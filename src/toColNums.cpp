// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
#include <math.h>
using namespace Rcpp;
typedef Eigen::SparseVector<double> SpVec;
typedef SpVec::InnerIterator InIterVec;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::interfaces(r, cpp)]]


// [[Rcpp::export]]
std::vector<double> toColNums(Eigen::SparseMatrix<double> data) {
  std::vector<double> tmp( data.nonZeros() );
  int iter=0;
  int id = 0;
  for ( int k=0; k < data.outerSize(); ++k){
	  id ++;
	  for (Eigen::SparseMatrix<double>::InnerIterator it(data, k); it; ++it){
	  	  tmp[iter++] = id;
	  }
  }
  return tmp;
}
