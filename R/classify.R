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
#' @param y a factor vector of observations.
#' @param x a factor vector of target groups.
#' @param imprecision a number indicting the margin of imprecision allowed in classification.
#' @param nreps the number of replicates to use in the randomisation test.
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
#'   \item{call}{the matched call.}
#'   }
#' @examples
#' a <- sample(1:5, 20, replace = TRUE)
#' b <- rep(c("group1", "group2"), each = 10)
#' b <- factor(b)
#' mod <- classify(a, b)
#' mod <- classify(a, b, nreps = 200L)
#' @export
classify <- function(y, x, imprecision = 0, nreps = 10000L) {

  stopifnot("The second argument to classify() must be a vector"=is.null(dim(x))) # is not not a df or matrix
  stopifnot("The second argument to classify() must be a vector, not a list"=is.recursive(x) == FALSE) # x is not a list
  stopifnot("The first argument to classify() must be a vector, not a list"=is.recursive(y) == FALSE) # y is not a list
  stopifnot("The first argument to classify() cannot be a factor"=!is.factor(y))
  stopifnot("The second argument to classify() must be a factor"=is.factor(x))
  stopifnot("length of vectors passed to classify() are not equal"=length(x) == length(y))
  stopifnot("nreps must be a number"=is.numeric(nreps)) # TRUE for int or double
  stopifnot("nreps must be a positive number"=nreps >= 1) # nreps must be a positve number
  stopifnot("nreps must be a single number"=length(nreps) == 1) # nreps is a single value
  stopifnot("nreps must be a whole number"=nreps %% 1 == 0)

  if (is.character(y)) {
    if (any(is.na(y))) {
      obs_num <- as.integer(addNA(y))
    } else {
      obs_num <- as.integer(factor(y))
    }
  } else if (is.numeric(y)) {
    if (any(is.na(y))) {
      obs_num <- as.integer(addNA(y))
    } else {
       obs_num <- y
    }
  }

  obs_pcc <- c_classify(obs_num, x, imprecision) # calls Rcpp classification function
  correct_classifications <- length(obs_pcc$classification_result[obs_pcc$classification_result == "correct"])
  ambiguous_classifications <- length(obs_pcc$classification_result[obs_pcc$classification_result == "ambiguous"])
  incorrect_classifications <- length(obs_pcc$classification_result[obs_pcc$classification_result == "incorrect"])
  rand_pccs <- c_rand_pccs(obs_num, x, imprecision, nreps)
  cval <- length(rand_pccs[rand_pccs >= obs_pcc$pcc])/nreps
  return(
    structure(
      list(
        prediction = obs_pcc$predicted_classification,
        accuracy = obs_pcc$classification_result,
        pcc = obs_pcc$pcc,
        correct_classifications = correct_classifications,
        ambiguous_classifications = ambiguous_classifications,
        incorrect_classifications = incorrect_classifications,
        cval = cval,
        pcc_replicates = rand_pccs,
        y = y,
        x = x,
        y_num = obs_num,
        nreps = nreps,
        call = match.call()
      ),
      class = "clubprofit"
    )
  )
}
