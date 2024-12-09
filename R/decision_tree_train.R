#' @useDynLib SA24204128
NULL

#' @title Majority Class
#' @description Returns the majority class in a given vector.
#' @param target A vector of class labels.
#' @return The majority class in the vector.
#' 
#' @examples
#' \dontrun{
#'     target <- c("setosa", "setosa", "versicolor", "setosa")
#'     majority <- majority_class(target)
#'     print(majority)  # Should print "setosa"
#' }
#' @export
majority_class <- function(target) {
  class_counts <- table(target)
  return(names(which.max(class_counts)))
}

#' @title Decision Tree Training
#' @description This function trains a decision tree recursively using a greedy algorithm. 
#' It selects the best feature and split point at each node to maximize information gain, 
#' and continues until a maximum depth is reached or the data at a node is homogeneous.
#' @param data A data frame containing the input features and target variable.
#' @param target A string specifying the name of the target variable in the data.
#' @param max_depth The maximum depth of the decision tree. Default is 10.
#' @param current_depth The current depth of the tree during the recursive training process. Default is 1.
#' @return A list representing the decision tree.
#' @examples
#' \dontrun{
#'     data(iris)
#'     tree <- decision_tree_train(iris, target = "Species", max_depth = 5)
#'     str(tree)
#' }
#' @export
decision_tree_train <- function(data, target, max_depth = 10, current_depth = 1) {
  if (current_depth >= max_depth || length(unique(data[[target]])) == 1) {
    return(majority_class(data[[target]]))
  }
  
  data_matrix <- as.matrix(data[, colnames(data) != target])
  target_vector <- as.integer(as.factor(data[[target]]))
  
  split <- find_best_split_rcpp(data_matrix, target_vector)
  
  if (is.null(split$feature) || split$feature == -1) {
    return(majority_class(data[[target]]))
  }
  
  feature <- colnames(data)[split$feature + 1]
  threshold <- split$threshold
  
  left_data <- data[data[[feature]] <= threshold, , drop = FALSE]
  right_data <- data[data[[feature]] > threshold, , drop = FALSE]
  
  if (nrow(left_data) == 0 || nrow(right_data) == 0) {
    return(majority_class(data[[target]]))
  }
  
  return(list(
    feature = feature,
    threshold = threshold,
    left = decision_tree_train(left_data, target, max_depth, current_depth + 1),
    right = decision_tree_train(right_data, target, max_depth, current_depth + 1)
  ))
}
