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

#' @export
print.clubprofit <- function(x, ...) {
  print(x$call)
}

#' @export
summary.clubprofit <- function(object, ..., digits = 2L) {
  cat("Classification analysis of", length(object$prediction), "observations.\n\n")
  cat("PCC:", round(object$pcc, digits), "\n")
  cat("c-value:", round(object$cval, 2), "\n\n")
  print(individual_results(object))
}

#' Individual level classification results.
#'
#' @details
#' Returns a data.frame containing predicted classifications and classification
#' accuracy for each individual observation.
#' @param m an object of class "clubprofit" produced by \code{classify()}
#' @param digits an integer
#' @return a data.frame containing a columns of predictions and prediction accuracy
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

#' Plots individual-level classification results.
#' @param x an object of class "clubprofit" produced by \code{classify()}
#' @param ... ignored
#' @return an object of class "ggplot"
#' @export
plot.clubprofit <- function(x, ...) {

  df <- individual_results(x)

  # find the largest count in a single group, used to set axis labels
  max_count <- max(table(df$observation, df$target))

  ggplot(df, aes(x=observation)) +
    geom_bar(stat="count", colour = "black", aes(fill = accuracy), alpha = 0.9) +
    scale_fill_manual(values = c("#7aa457", "#cb6751", "#9e6ebd")) +
    scale_x_reverse(breaks = min(z$observation):max(z$observation)) +
    scale_y_continuous(breaks = 0:max_count, labels = 0:max_count) +
    coord_flip() +
    facet_wrap(~ target) +
    theme_bw() +
    theme(panel.grid = element_blank(), legend.position = "bottom", legend.title = element_blank())
}
