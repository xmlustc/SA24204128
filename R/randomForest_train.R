#' @title Random Forest Training
#' @description Trains a random forest using a collection of decision trees, 
#' each trained on a bootstrapped sample of the data with random feature selection.
#' @param data A data frame containing the input features and target variable.
#' @param target A string specifying the name of the target variable in the data.
#' @param n_trees The number of decision trees to include in the forest. Default is 100.
#' @param max_depth The maximum depth of each decision tree. Default is 10.
#' @return A list containing all trained trees and their respective features.
#' @examples
#' \dontrun{
#'     data(iris)
#'     forest <- randomForest_train(iris, target = "Species", n_trees = 100, max_depth = 10)
#'     str(forest)
#' }
#' @export
randomForest_train <- function(data, target, n_trees = 100, max_depth = 10) {
  forest <- list()
  for (i in 1:n_trees) {
    bootstrapped_data <- data[sample(1:nrow(data), replace = TRUE), ]
    forest[[i]] <- decision_tree_train(bootstrapped_data, target, max_depth)
  }
  return(forest)
}
