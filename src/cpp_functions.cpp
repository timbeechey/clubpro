// clubpro, an R package for classification using binary procrustes rotation.
// Copyright (C) 2022  Timothy Beechey (tim.beechey@protonmail.com)
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include <Rcpp.h>
#include <cpp11.hpp>
using namespace Rcpp;
using namespace cpp11;

[[cpp11::register]]
void fun() {}

// [[Rcpp::export]]
NumericMatrix c_to_indicator_matrix(NumericVector v) {
  // if there are any values < 1, shift all values up to align with column indices
  // i.e. so the minimum value is 1
  NumericVector shifted_vec(v.length());
  if (min(v) < 1) {
    shifted_vec = v + (1 - min(v));
  } else {
    shifted_vec = v; // no shift
  }
  long n {shifted_vec.length()};
  double m {max(v)};
  NumericMatrix indmat(n, m);
  for (unsigned int i = 0; i < n; i++) {
    for (unsigned int j = 0; j < m; j++) {
      if (j == shifted_vec[i]-1.0) {
        indmat(i, j) = 1.0;
      }
    }
  }
  return indmat;
}

// [[Rcpp::export]]
NumericMatrix c_normalise_matrix(NumericMatrix A) {
  for (unsigned int j = 0; j < A.ncol(); j++) {
    float colfactor = sqrt(sum(pow(A(_,j),2)));
    for (unsigned int i = 0; i < A.nrow(); i++) {
      if (colfactor == 0.0) {
        A(i,j) = 0.0;
      } else {
        A(i,j) = A(i,j) / colfactor;
      }
    }
  }
  for (unsigned int i = 0; i < A.nrow(); i++) {
    float rowfactor = sqrt(sum(pow(A(i,_),2)));
    for (unsigned int j = 0; j < A.ncol(); j++) {
      if (rowfactor == 0.0) {
        A(i,j) = 0.0;
      } else {
        A(i,j) = A(i,j) / rowfactor;
      }
    }
  }
  return A;
}

// [[Rcpp::export]]
NumericMatrix c_dichotemise_matrix(NumericMatrix A) {
  for (int i = 0; i < A.nrow(); i++) {
    double m = max(A(i,_));
    for(int j = 0; j < A.ncol(); j++) {
      if (A(i,j) == m) {
        A(i,j) = 1.0;
      } else {
        A(i,j) = 0.0;
      }
    }
  }
  return A;
}

// [[Rcpp::export]]
NumericMatrix c_binary_procrustes_rotation(NumericVector x, NumericVector y) {
  // access R base functions for matrix multiplication and cross product
  Environment::base_namespace();
  Function crossprod("crossprod");
  Function matmult("%*%");
  NumericMatrix X = c_to_indicator_matrix(x);
  NumericMatrix Y = c_to_indicator_matrix(y);
  NumericMatrix T = crossprod(X, Y);
  NumericMatrix Tn = c_normalise_matrix(T);
  NumericMatrix Z = matmult(X, Tn);
  return Z;
}

// [[Rcpp::export]]
List c_classify(NumericVector obs, NumericVector target) {

  IntegerVector matches(obs.length());
  CharacterVector unique_group_names = target.attr("levels");
  StringVector predicted_classification(obs.length());
  StringVector classification_result(obs.length());
  CharacterVector all_group_names(target.length());

  NumericMatrix conformed_mat = c_binary_procrustes_rotation(obs, target);
  NumericMatrix target_indicator_mat = c_to_indicator_matrix(target);
  NumericMatrix binary_matrix = c_dichotemise_matrix(conformed_mat);

  for (int i = 0; i < all_group_names.length(); i++) {
    all_group_names[i] = unique_group_names[target[i]-1];
  }

  for (int i = 0; i < binary_matrix.nrow(); i++) {
    // is_true() converts SingleLogicalResult returned by all() to bool
    if (is_true(all(binary_matrix(i,_) == target_indicator_mat(i,_)))) {
      matches[i] = 1;
    } else{
      matches[i] = 0;
    }
    for (int j = 0; j < binary_matrix.ncol(); j++) {
      if (binary_matrix(i, j) == 1.0) {
        CharacterVector x {predicted_classification[i], unique_group_names[j]};
        predicted_classification[i] = collapse(x);
        if (predicted_classification[i] == all_group_names[i]) {
          classification_result[i] = "correct";
        } else if (sum(binary_matrix(i,_)) > 1.0) {
          classification_result[i] = "ambiguous";
        } else{
          classification_result[i] = "incorrect";
        }
      }
    }
  }

  double pcc = mean(matches) * 100;

  List out = List::create(Named("predicted_classification") = predicted_classification, _["classification_result"] = classification_result, _["pcc"] = pcc);
  return out;
}

// [[Rcpp::export]]
NumericVector c_rand_pccs(NumericVector obs, NumericVector target, int nreps) {
  NumericVector pccs(nreps); // preallocate vector
  for (int i = 0; i < nreps; i++) {
    pccs[i] = c_classify(sample(obs, obs.length()), target)["pcc"];
  }
  return pccs;
}
