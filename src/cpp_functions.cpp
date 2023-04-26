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

#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat to_indicator_matrix(arma::vec v) {
    auto n {v.n_elem};
    auto m {v.max()};
    arma::mat indmat(n, m);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            if (j == v[i] - 1.0) {
                indmat(i, j) = 1.0;
            }
        }
    }
    return indmat;
}

// [[Rcpp::export]]
arma::mat normalise_matrix_columns(arma::mat A) {
    for (int j = 0; j < A.n_cols; j++) {
        double colfactor = sqrt(accu(pow(A.col(j),2)));
        for (int i = 0; i < A.n_rows; i++) {
            if (colfactor == 0.0) {
                A(i,j) = 0.0;
            } else {
                A(i,j) /= colfactor;
            }
        }
    }
    return A;
}

// [[Rcpp::export]]
arma::mat normalise_matrix_rows(arma::mat A) {
    for (int i = 0; i < A.n_rows; i++) {
        float rowfactor = sqrt(accu(pow(A.row(i),2)));
        for (int j = 0; j < A.n_cols; j++) {
            if (rowfactor == 0.0) {
                A(i,j) = 0.0;
            } else {
                A(i,j) /= rowfactor;
            }
        }
    }
    return A;
}

// [[Rcpp::export]]
arma::mat dichotemise_matrix(arma::mat A) {
    for (int i = 0; i < A.n_rows; i++) {
        auto m = max(A.row(i));
        for(int j = 0; j < A.n_cols; j++) {
            if ((A(i,j) == m) & (m > 0)) {
                A(i,j) = 1.0;
            } else {
                A(i,j) = 0.0;
            }
        }
    }
    return A;
}

// [[Rcpp::export]]
arma::mat binary_procrustes_rotation(arma::vec x, arma::vec y, bool normalise_cols) {
    arma::mat T = trans(to_indicator_matrix(x)) * to_indicator_matrix(y);
    arma::mat Z;
    if (normalise_cols) {
        Z = to_indicator_matrix(x) * normalise_matrix_rows(normalise_matrix_columns(T));
    } else {
        Z = to_indicator_matrix(x) * normalise_matrix_rows(T);
    }
    return Z;
}

// [[Rcpp::export]]
double c_pcc(arma::vec obs, arma::vec target, int imprecision, bool normalise_cols) {
    arma::mat target_indicator_mat = to_indicator_matrix(target);
    arma::mat binary_matrix = dichotemise_matrix(binary_procrustes_rotation(obs, target, normalise_cols));
    int matches{};
    for (int i = 0; i < binary_matrix.n_rows; i++) {
        if (accu(binary_matrix.row(i)) == 1.0) {
            if (std::abs(int(binary_matrix.row(i).index_max()) - int(target_indicator_mat.row(i).index_max())) <= imprecision) {
                matches += 1;
            } 
        }
    }
    return ((double)matches / obs.n_elem) * 100;
}

// [[Rcpp::export]]
arma::vec shuffle_obs_pccs(arma::vec obs, arma::vec target, int imprecision, int nreps, bool normalise_cols) {
    arma::vec pccs(nreps);
    for (int i = 0; i < nreps; i++) {
        pccs(i) = c_pcc(shuffle(obs), target, imprecision, normalise_cols);
    }
    return pccs;
}

// [[Rcpp::export]]
arma::vec random_dat_pccs(arma::vec obs, arma::vec target, int imprecision, int nreps, bool normalise_cols) {
    arma::vec pccs(nreps);
    arma::vec obs_range = linspace(min(obs), max(obs), (max(obs)-min(obs))+1);
    for (int i = 0; i < nreps; i++) {
        pccs(i) = c_pcc(Rcpp::RcppArmadillo::sample(obs_range, obs.n_elem, true), target, imprecision, normalise_cols);
    }
    return pccs;
}
