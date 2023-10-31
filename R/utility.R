# clubpro, an R package for classification using binary procrustes rotation.
# Copyright (C) 2023  Timothy Beechey (tim.beechey@protonmail.com)
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

#' Print the model call.
#' @param x an object of class "clubprofit"
#' @param ... ignored
#' @return No return value, called for side effects.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' print(mod)
#' @export
print.clubprofit <- function(x, ...) {
  print(x$call)
}

#' Print a summary of results from a fitted clubpro model.
#' @param object an object of class "clubprofit".
#' @param digits an integer used for rounding numeric values in the output.
#' @param ... ignored
#' @return No return value, called for side effects.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' summary(mod)
#' @export
summary.clubprofit <- function(object, ..., digits = 2L) {

  # check function arguments
  stopifnot("digits must be a number"= is.numeric(digits))
  stopifnot("digits cannot be negative"= digits >= 0)
  stopifnot("digits must be a single number"= length(digits) == 1)

  cat("********** Classification Results **********\n")
  cat("Observations:", length(object$y), "\n")
  cat("Missing observations:", sum(is.na(object$y)), "\n")
  cat("Target groups:", nlevels(object$x), "\n")
  cat("Correctly classified observations:", object$correct_classifications, "\n")
  cat("Incorrectly classified observations:", object$incorrect_classifications, "\n")
  cat("Ambiguously classified observations:", object$ambiguous_classifications, "\n")
  cat("PCC:", round(object$pcc, digits), "\n")
  cat("Median classification strength index:", round(object$median_csi, digits), "\n\n")
  cat("********** Randomisation Test **********\n")
  cat("Random reorderings:", object$nreps, "\n")
  cat("Minimum random PCC:", round(min(object$pcc_replicates), digits), "\n")
  cat("Maximum random PCC:", round(max(object$pcc_replicates), digits), "\n")
  cat("Chance-value:", round(object$cval, 2), "\n\n")
  print(individual_results(object))
}

#' Individual level classification results.
#'
#' @details
#' Returns a data.frame containing predicted classifications and classification
#' accuracy for each individual observation.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @param digits an integer
#' @return a data.frame containing a columns of predictions and prediction
#' accuracy
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' individual_results(mod)
#' @export
individual_results <- function(m, digits) {
  UseMethod("individual_results")
}

#' @export
individual_results.default <- function(m, digits) .NotYetImplemented()

#' @export
individual_results.clubprofit <- function(m, digits = 2L) {
  df <- data.frame(individual = 1:length(m$y),
                   observation = m$y,
                   target = m$x,
                   prediction = m$prediction,
                   accuracy = m$accuracy,
                   csi = round(m$csi, 2))
  df$target <- factor(df$target, levels = levels(m$x))
  df$accuracy <- factor(df$accuracy, levels = c("correct", "incorrect", "ambiguous"))
  df
}


#' Predicted category for each observation.
#'
#' @details
#' Returns a character vector containing the name of the predicted category
#' for each observed value.
#' @param object an object of class "clubprofit" produced by \code{club()}
#' @param ... ignored
#' @return a character vector.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' predict(mod)
#' @export
predict.clubprofit <- function(object, ...) {
  object$prediction
}


#' Classification accuracy for each observation.
#'
#' @details
#' Returns a character vector containing a string corresponding to each
#' observation indicating whether classification of that observation was
#' "correct", "incorrect", or "ambigous".
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return a character vector.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' accuracy(mod)
#' @export
accuracy <- function(m) {
  UseMethod("accuracy")
}

#' @export
accuracy.default <- function(m) .NotYetImplemented()

#' @export
accuracy.clubprofit <- function(m) {
  m$accuracy
}


#' Percentage of correct classifications.
#'
#' @details
#' Returns the percentage of correctly classified observations.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return a numeric value.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' pcc(mod)
#' @export
pcc <- function(m) {
  UseMethod("pcc")
}


#' @export
pcc.default <- function(m) .NotYetImplemented()


#' @export
pcc.clubprofit <- function(m) {
  m$pcc
}


#' Chance value.
#'
#' @details
#' Compute the chance that randomly reordered data results in a
#' percentage of correctly classified observations at least as high as
#' the observed data.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return a numeric value.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' cval(mod)
#' @export
cval <- function(m) {
  UseMethod("cval")
}


#' @export
cval.default <- function(m) .NotYetImplemented()


#' @export
cval.clubprofit <- function(m) {
  m$cval
}


#' Number of correct classifications.
#'
#' @details
#' Returns the number of observations which were
#' classified correctly by the model.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return an integer.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' n_correct(mod)
#' @export
n_correct <- function(m) {
  UseMethod("n_correct")
}


#' @export
n_correct.default <- function(m) .NotYetImplemented()


#' @export
n_correct.clubprofit <- function(m) {
  m$correct_classifications
}


#' Number of incorrect classifications.
#'
#' @details
#' Returns the number of observations which were
#' classified incorrectly by the model.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return an integer.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' n_incorrect(mod)
#' @export
n_incorrect <- function(m) {
  UseMethod("n_incorrect")
}


#' @export
n_incorrect.default <- function(m) .NotYetImplemented()


#' @export
n_incorrect.clubprofit <- function(m) {
  m$incorrect_classifications
}


#' Number of ambiguous classifications.
#'
#' @details
#' Returns the number of observations which were
#' classified ambiguously by the model.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return an integer.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' n_ambiguous(mod)
#' @export
n_ambiguous <- function(m) {
  UseMethod("n_ambiguous")
}


#' @export
n_ambiguous.default <- function(m) .NotYetImplemented()


#' @export
n_ambiguous.clubprofit <- function(m) {
  m$ambiguous_classifications
}


#' Classification strength indices.
#'
#' @details
#' Returns a vector containing the classification strength
#' index for each observation.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return a numeric vector.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' csi(mod)
#' @export
csi <- function(m) {
  UseMethod("csi")
}


#' @export
csi.default <- function(m) .NotYetImplemented()


#' @export
csi.clubprofit <- function(m) {
  m$csi
}


#' Median classification strength index.
#'
#' @details
#' Returns the median classification strength index.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return a numeric vector.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' median_csi(mod)
#' @export
median_csi <- function(m) {
  UseMethod("median_csi")
}


#' @export
median_csi.default <- function(m) .NotYetImplemented()


#' @export
median_csi.clubprofit <- function(m) {
  m$median_csi
}


#' PCC replicates.
#'
#' @details
#' Returns an object containing a vector of PCC replicates used to
#' calculate the chance-value.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return an object of class clubprorand.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' head(pcc_replicates(mod))
#' @export
pcc_replicates <- function(m) {
  UseMethod("pcc_replicates")
}


#' @export
pcc_replicates.default <- function(m) .NotYetImplemented()


#' @export
pcc_replicates.clubprofit <- function(m) {
  pcc_replicates <- m$pcc_replicates
  attr(pcc_replicates, "observed_pcc") <- m$pcc
  class(pcc_replicates) <- "clubprorand"
  pcc_replicates
}


#' Plot PCC replicates.
#'
#' @details
#' Plot a histogram of PCCs computed from randomly reordered data
#' used to calculate the chance-value.
#' @param x an object of class "clubprofit" produced by \code{club()}
#' @param ... ignored
#' @return no return value, called for side effects only.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' plot(pcc_replicates(mod))
#' @export
plot.clubprorand <- function(x, ...) {
  lattice::histogram(unclass(x), type = "count", xlab = "PCC", ylab = "Count", col = "#56B4E9",
                     panel = function(...) {
                       panel.histogram(...)
                       panel.abline(v = attr(x, "observed_pcc"), 
                                    col = "red", 
                                    lty = 2)
                     })
}


#' Plot classification accuracy.
#'
#' @details
#' Produces bar plot showing counts of individuals against observed values within
#' each target grouping. Fill colours indicate whether each individual was
#' classified correctly, incorrectly or ambiguously.
#' 
#' @param x an object of class "clubprofit" produced by \code{club()}
#' @param colors a character vector of length 3 specifying colors to use in plot
#' @param ... ignored
#' @return called for side-effects only
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' plot(mod)
#' @export
plot.clubprofit <- function(x, colors = c("#56B4E9", "#E69F00", "#999999"), ...) {
  accuracy <- observation <- target <- NULL
  z <- individual_results(x)
  xlabs <- levels(addNA(z$observation))
  xlabs[is.na(xlabs)] <- "NA"

  if (any(is.na(z$target))) {
    z$target <- addNA(z$target)
    npanels <- nlevels(z$target)
  } else {
    z$target <- factor(z$target)
    npanels <- nlevels(z$target)
  }

  if (any(is.na(z$observation))) {
    z$observation <- addNA(z$observation)
  } else {
    z$observation <- factor(z$observation)
  }


  histogram(~ observation | target,
    data = z,
    type = "count",
    xlim = c(0:(nlevels(z$observation) + 1)),
    scales = list(x = list(at = 1:nlevels(z$observation), labels = xlabs)),
    groups = factor(accuracy, levels = c("correct", "incorrect", "ambiguous")),
    panel = function(...) {
                           panel.superpose(..., 
                                           panel.groups = panel.histogram,
                                           col = colors, 
                                           border = "black", 
                                           alpha = 1.0)},
    xlab = "Observed Value", ylab = "Count",
    layout = c(1, npanels),
    par.settings = list(superpose.polygon = list(col = colors, 
                                                 border = "black", 
                                                 alpha = 1.0)),
    auto.key = list(space = "top", rectangles = TRUE, columns = 3)
  )
}
