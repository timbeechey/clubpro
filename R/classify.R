# clubpro, an R package for classification using binary procrustes rotation.
# Copyright (C) 2022  Timothy Beechey (tim.beechey@protonmail.com)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Classify observations
#'
#' \code{classify()} is used to classify obervations using binary procrustes
#' rotation.
#' @param y, a numeric vector of observations.
#' @param x, a factor vector.
#' @param nreps, the number of replicates to use in the randomisation test.
#' @return an object of class "clubprofit" is a list containing the folllowing
#' components:
#' \describe{
#'   \item{prediction}{a character vector of predicted classifications.}
#'   \item{accuracy}{a character vector indicating whether each classification
#'   is "correct", "incorrect", or "ambiguous".}
#'   \item{pcc}{the percentage of correct classifications.}
#'   \item{cval}{the chance of randomly reordered data producing a PCC >= the
#'   observed PCC.}
#'   \item{pcc_replicates}{a vector of PCCs generated from randomly reordered
#'   data used to calculate \code{cval}.}
#'   }
#' @export
classify <- function(y, x, nreps = 1000) {
  assertthat::assert_that(is.factor(x), msg = "The target vector must be a factor")
  assertthat::assert_that(assertthat::are_equal(length(x), length(y)),
                          msg = "The vectors passed to classify() must be of the same length")
  obs_pcc <- c_classify(y, x) # calls Rcpp classification function
  rand_pccs <- c_rand_pccs(y, x, nreps)
  cval <- length(rand_pccs[rand_pccs >= obs_pcc$pcc])/nreps
  return(
    structure(
      list(
        prediction = obs_pcc$predicted_classification,
        accuracy = obs_pcc$classification_result,
        pcc = obs_pcc$pcc,
        cval = cval,
        pcc_replicates = rand_pccs
      ),
      class = "clubprofit"
    )
  )
}
