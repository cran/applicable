#' Print number of predictors and principal components used.
#'
#' @param x A `apd_pca` object.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @return None
#'
#' @examples
#'
#' model <- apd_pca(~ Sepal.Length + Sepal.Width, iris)
#' print(model)
#' @export
print.apd_pca <- function(x, ...) {
  predictors_count <- ncol(x$blueprint$ptypes$predictors)
  percentage <- x$threshold * 100
  num_comp <- x$num_comp
  wording <- "components were"

  if (num_comp == 1) {
    wording <- "component was"
  }

  print_output <- glue::glue(
    "# Predictors:
      {predictors_count}
   # Principal Components:
      {num_comp} {wording} needed
      to capture at least {percentage}% of the
      total variation in the predictors."
  )

  cat(print_output)

  invisible(x)
}

#' Print number of predictors and principal components used.
#'
#' @param x A `apd_hat_values` object.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @return None
#'
#' @examples
#'
#' model <- apd_hat_values(~ Sepal.Length + Sepal.Width, iris)
#' print(model)
#' @export
print.apd_hat_values <- function(x, ...) {
  predictors_count <- ncol(x$blueprint$ptypes$predictors)

  print_output <- glue::glue(
    "# Predictors:
        {predictors_count}"
  )
  cat(print_output)

  invisible(x)
}

#' Print number of predictors and principal components used.
#'
#' @param x A `apd_similarity` object.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @return None
#'
#' @examples
#'
#' set.seed(535)
#' tr_x <- matrix(
#'   sample(0:1, size = 20 * 50, prob = rep(.5, 2), replace = TRUE),
#'   ncol = 20
#'  )
#' model <- apd_similarity(tr_x)
#' print(model)
#' @export
print.apd_similarity <- function(x, ...) {
  cat("Applicability domain via similarity\n")
  cat(
    "Reference data were", ncol(x$ref_data), "variables collected on",
    nrow(x$ref_data), "data points.\n"
  )
  if (!is.na(x$quantile)) {
    cat(
      "New data summarized using the ", round(x$quantile * 100, 1),
      "th percentile.\n",
      sep = ""
    )
  } else {
    cat("New data summarized using the mean.\n", sep = "")
  }
  invisible(x)
}
