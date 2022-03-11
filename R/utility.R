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

  # bind variables to function
  observation <- accuracy <- NULL

  df <- individual_results(x)

  # find the largest count in a single group, used to set axis labels
  max_count <- max(table(df$observation, df$target))

  ggplot2::ggplot(df, ggplot2::aes(x=observation)) +
    ggplot2::geom_bar(stat="count", colour = "black",
                      ggplot2::aes(fill = accuracy), alpha = 0.9) +
    ggplot2::scale_fill_manual(values = c("#7aa457", "#cb6751", "#9e6ebd")) +
    ggplot2::scale_x_reverse(breaks = min(df$observation):max(df$observation)) +
    ggplot2::scale_y_continuous(breaks = 0:max_count, labels = 0:max_count) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ target, nrow = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank())
}

#' Plot observed and random order generated PCCs.
#'
#' Produces a plot which shows the proportion of random reorderings of the
#' observed data which produce PCCs at least as high as the observed PCC.
#' @param m an object of class "clubprofit" produced by \code{classify()}.
#' @return an object of class "ggplot".
#' @export
plot_cvalue <- function(m) {
  UseMethod("plot_cvalue")
}

#' @rdname plot_cvalue
#' @export
plot_cvalue.default <- function(m) .NotYetImplemented()

#' @rdname plot_cvalue
#' @export
plot_cvalue.clubprofit <- function(m) {

  #bind variables to the function
  n <- PCC <- NULL

  df <- data.frame(n = 1:length(m$pcc_replicates),
                   PCC = m$pcc_replicates)

  ggplot2::ggplot(df, ggplot2::aes(x = PCC)) +
    ggplot2::geom_histogram(colour = "black", fill = "#7aa457", alpha=0.7, bins=10) +
    ggplot2::geom_vline(xintercept = m$pcc, linetype=2) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}
