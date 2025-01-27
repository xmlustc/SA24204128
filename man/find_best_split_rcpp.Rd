% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{find_best_split_rcpp}
\alias{find_best_split_rcpp}
\title{Find the Best Split Using Rcpp}
\usage{
find_best_split_rcpp(data, target)
}
\arguments{
\item{data}{A numeric matrix where rows represent observations and columns represent features.}

\item{target}{An integer vector containing the target variable (class labels) corresponding to the rows of \code{data}.}
}
\value{
A list containing:
\itemize{
  \item \code{feature}: The index of the best feature to split on.
  \item \code{threshold}: The threshold value of the best split for the feature.
}
}
\description{
This function finds the best feature and threshold to split data using Gini impurity, implemented with Rcpp for speed.
}
\examples{
\dontrun{
data <- matrix(runif(100), nrow = 10, ncol = 10)
target <- sample(0:1, 10, replace = TRUE)
best_split <- find_best_split_rcpp(data, target)
print(best_split)
}
}
