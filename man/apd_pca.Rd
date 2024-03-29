% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca-fit.R
\name{apd_pca}
\alias{apd_pca}
\alias{apd_pca.default}
\alias{apd_pca.data.frame}
\alias{apd_pca.matrix}
\alias{apd_pca.formula}
\alias{apd_pca.recipe}
\title{Fit a \code{apd_pca}}
\usage{
apd_pca(x, ...)

\method{apd_pca}{default}(x, ...)

\method{apd_pca}{data.frame}(x, threshold = 0.95, ...)

\method{apd_pca}{matrix}(x, threshold = 0.95, ...)

\method{apd_pca}{formula}(formula, data, threshold = 0.95, ...)

\method{apd_pca}{recipe}(x, data, threshold = 0.95, ...)
}
\arguments{
\item{x}{Depending on the context:
\itemize{
\item A \strong{data frame} of predictors.
\item A \strong{matrix} of predictors.
\item A \strong{recipe} specifying a set of preprocessing steps
created from \code{\link[recipes:recipe]{recipes::recipe()}}.
}}

\item{...}{Not currently used, but required for extensibility.}

\item{threshold}{A number indicating the percentage of variance desired from
the principal components. It must be a number greater than 0 and less or
equal than 1.}

\item{formula}{A formula specifying the predictor terms on the right-hand
side. No outcome should be specified.}

\item{data}{When a \strong{recipe} or \strong{formula} is used, \code{data} is specified as:
\itemize{
\item A \strong{data frame} containing the predictors.
}}
}
\value{
A \code{apd_pca} object.
}
\description{
\code{apd_pca()} fits a model.
}
\details{
The function computes the principal components that account for
up to either 95\% or the provided \code{threshold} of variability. It also
computes the percentiles of the absolute value of the principal components.
Additionally, it calculates the mean of each principal component.
}
\examples{
predictors <- mtcars[, -1]

# Data frame interface
mod <- apd_pca(predictors)

# Formula interface
mod2 <- apd_pca(mpg ~ ., mtcars)

# Recipes interface
library(recipes)
rec <- recipe(mpg ~ ., mtcars)
rec <- step_log(rec, disp)
mod3 <- apd_pca(rec, mtcars)
}
