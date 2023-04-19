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

#' Print the model call.
#' @param x an object of class "clubprofit"
#' @param ... ignored
#' @return No return value, called for side effects.
#' @examples
#' a <- sample(1:5, 20, replace = TRUE)
#' b <- rep(c("group1", "group2"), each = 10)
#' b <- factor(b)
#' mod <- classify(a, b)
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
#' a <- sample(1:5, 20, replace = TRUE)
#' b <- rep(c("group1", "group2"), each = 10)
#' b <- factor(b)
#' mod <- classify(a, b)
#' summary(mod)
#' summary(mod, digits = 3)
#' @export
summary.clubprofit <- function(object, ..., digits = 2L) {

  # check function arguments
  stopifnot("digits must be a number"= is.numeric(digits))
  stopifnot("digits cannot be negative"= digits >= 0)
  stopifnot("digits must be a single number"= length(digits) == 1)

  cat("*****Classification Results*****\n")
  cat("Observations:", length(object$y), "\n")
  cat("Missing observations:", sum(is.na(object$y)), "\n")
  cat("Target groups:", nlevels(object$x), "\n")
  cat("Correctly classified observations:", object$correct_classifications, "\n")
  cat("Incorrectly classified observations:", object$incorrect_classifications, "\n")
  cat("Ambiguously classified observations:", object$ambiguous_classifications, "\n")
  cat("PCC:", round(object$pcc, digits), "\n\n")
  cat("*****Randomisation Test*****\n")
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
#' @param m an object of class "clubprofit" produced by \code{classify()}
#' @param digits an integer
#' @return a data.frame containing a columns of predictions and prediction
#' accuracy
#' @examples
#' a <- sample(1:5, 20, replace = TRUE)
#' b <- rep(c("group1", "group2"), each = 10)
#' b <- factor(b)
#' mod <- classify(a, b)
#' individual_results(mod)
#' individual_results(mod, digits = 3)
#' @export
individual_results <- function(m, digits) {
  UseMethod("individual_results")
}

#' @rdname individual_results
#' @export
individual_results.default <- function(m, digits) .NotYetImplemented()

#' @rdname individual_results
#' @export
individual_results.clubprofit <- function(m, digits = 2L) {
  df <- data.frame(individual = 1:length(m$y),
                   observation = m$y,
                   target = m$x,
                   prediction = m$prediction,
                   accuracy = m$accuracy)
  df$target <- factor(df$target, levels = levels(m$x))
  df$accuracy <- factor(df$accuracy, levels = c("correct", "incorrect", "ambiguous"))
  df
}

#' Plot classification accuracy.
#'
#' @details
#' Produces a strip plot showing counts of individuals against observed values within
#' each target grouping. Point fill colours indicate whether
#' individuals were classified correctly, incorrectly or ambiguously.
#' @param m an object of class "clubprofit" produced by \code{classify()}
#' @param abline_offset a number indicating vertical adjustment of horizontal ablines
#' @param point_offset a number indicating amount of vertical separation between points
#' @param ... ignored
#' @return called for side-effects only
#' @examples
#' a <- sample(1:5, 20, replace = TRUE)
#' b <- rep(c("group1", "group2"), each = 10)
#' b <- factor(b)
#' mod <- classify(a, b)
#' plot_classification(mod)
#' @export
plot_classification <- function(m, ...) {
  UseMethod("plot_classification")
}

#' @rdname plot_classification
#' @export
plot_classification.default <- function(m, ...) .NotYetImplemented()

#' @rdname plot_classification
#' @export
plot_classification.clubprofit <- function(m, abline_offset = -0.1, point_offset = 0.4, ...) {

  dat_vals <- m$y_num
  dat_labels <- m$y
  dat_labels[is.na(dat_labels)] <- "NA"
  dat_labels <- mixedsort(unique(dat_labels))
  target_labels <- as.character(m$x)
  target_labels[is.na(target_labels)] <- "NA"
  target_labels <- factor(target_labels)

  if (is.numeric(m$y)) {
    x_at <- min(dat_vals, na.rm=TRUE):max(dat_vals, na.rm=TRUE)
    x_labs <- dat_labels
  } else {
    x_at <- 1:length(dat_labels)
    x_labs <- dat_labels
  }

  stripchart(dat_vals ~ target_labels, method="stack", offset = point_offset, 
             at = 1:nlevels(target_labels), pch = 21, col = "#0072B2", 
             bg = "#0072B260", cex=1.5, group.names = levels(target_labels),
             subset = m$accuracy == "correct", las = 1, xaxt = "n", yaxt = "n",
             xlab = "Observed Value", ylab = "Observed Category", ...)
  axis(1, at = x_at, labels = x_labs)
  axis(2, at = 1:nlevels(target_labels), labels = levels(target_labels), las = 1)
  stripchart(dat_vals ~ target_labels, method="stack", offset = point_offset, 
             at = 1:nlevels(target_labels), pch = 21,
             col = "#D55E00", bg = "#D55E0060",
             cex=1.5, subset = m$accuracy == "incorrect", add = TRUE)
  stripchart(dat_vals ~ target_labels, method="stack", offset = point_offset, 
             at = 1:nlevels(target_labels), pch = 21,
             col = "#999999", bg = "#99999960",
             cex=1.5, subset = m$accuracy == "ambiguous", add = TRUE)
  for (i in 2:nlevels(target_labels)) {
    abline(h = i + abline_offset)
  }
  if ("ambiguous" %in% m$accuracy) {
      legend("top", legend = c("Correct", "Incorrect", "Ambiguous"), pt.cex=1.5,
             pch=21, col=c("#0072B2", "#D55E00", "#999999"), horiz = TRUE, bty = "n",
             pt.bg=c("#0072B260", "#D55E0060", "#99999960"), xpd = TRUE, inset = c(0, -0.15))
  } else {
      legend("top", legend = c("Correct", "Incorrect"), pt.cex=1.5,
             pch=21, col=c("#0072B2", "#D55E00"), horiz = TRUE, bty = "n",
             pt.bg=c("#0072B260", "#D55E0060"), xpd = TRUE, inset = c(0, -0.15))
  }
}

#' Plot predicted classification results.
#'
#' @details
#' Produces a strip plot showing counts of individuals for each observed value by
#' predicted classification grouping.
#' @param m an object of class "clubprofit" produced by \code{classify()}
#' @param ... ignored
#' @return called for side-effects only
#' @examples
#' a <- sample(1:5, 20, replace = TRUE)
#' b <- rep(c("group1", "group2"), each = 10)
#' b <- factor(b)
#' mod <- classify(a, b)
#' plot_prediction(mod)
#' @export
plot_prediction <- function(m, ...) {
  UseMethod("plot_prediction")
}

#' @rdname plot_prediction
#' @export
plot_prediction.default <- function(m, ...) .NotYetImplemented()

#' @rdname plot_prediction
#' @export
plot_prediction <- function(m, ...) {
    
  dat_vals <- m$y_num
  dat_labels <- unique(m$y)

  if (is.numeric(m$y)) {
    x_at <- min(dat_vals):max(dat_vals)
    x_labs <- min(dat_vals):max(dat_vals)
  } else {
    x_at <- 1:length(dat_labels)
    x_labs <- dat_labels
  }
    
  stripchart(dat_vals ~ m$prediction, method="stack", offset = 0.4, 
             at = 1:nlevels(m$x), pch = 21, col = "#0072B2", 
             bg = "#0072B260", cex=1.5, group.names = levels(m$x),
             las = 1, xaxt="n",
             xlab = "Observed Value", ylab = "Predicted Category", ...)
  axis(1, at = x_at, labels = x_labs)
  for (i in 1:nlevels(m$x)) {
       d <- density(dat_vals[m$prediction == levels(m$x)[i]], bw=0.5, na.rm = TRUE)
       d$y = d$y + as.numeric(i)
      lines(d, col = "#0072B2", lw = 2)
  }
}
