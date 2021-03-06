# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

c_to_indicator_matrix <- function(v) {
    .Call(`_clubpro_c_to_indicator_matrix`, v)
}

c_normalise_matrix <- function(A) {
    .Call(`_clubpro_c_normalise_matrix`, A)
}

c_dichotemise_matrix <- function(A) {
    .Call(`_clubpro_c_dichotemise_matrix`, A)
}

c_binary_procrustes_rotation <- function(x, y) {
    .Call(`_clubpro_c_binary_procrustes_rotation`, x, y)
}

c_classify <- function(obs, target, imprecision) {
    .Call(`_clubpro_c_classify`, obs, target, imprecision)
}

c_rand_pccs <- function(obs, target, imprecision, nreps) {
    .Call(`_clubpro_c_rand_pccs`, obs, target, imprecision, nreps)
}

