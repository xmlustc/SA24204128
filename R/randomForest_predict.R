#' @title Random Forest Prediction
#' @description Predicts the target variable for a new dataset using a trained random forest model. 
#' The final prediction is determined by majority voting across all trees.
#' @param forest A list representing the trained random forest.
#' @param new_data A data frame containing the input features for prediction. The features must match 
#' those used during training.
#' @return A vector of predicted class labels.
#' @examples
#' \dontrun{
#'     data(iris)
#'     new_data <- iris[1:10, -5]  # Remove target variable
#'     predictions <- randomForest_predict(forest, new_data)
#'     print(predictions)
#' }
#' @export
randomForest_predict <- function(forest, new_data) {
  predict_tree <- function(tree, data) {
    if (is.character(tree)) {
      return(rep(tree, nrow(data)))  # Leaf node returns majority class
    }
    
    feature <- tree$feature
    threshold <- tree$threshold
    
    left_indices <- which(data[[feature]] <= threshold)
    right_indices <- which(data[[feature]] > threshold)
    
    predictions <- rep(NA, nrow(data))
    if (length(left_indices) > 0) {
      predictions[left_indices] <- predict_tree(tree$left, data[left_indices, , drop = FALSE])
    }
    if (length(right_indices) > 0) {
      predictions[right_indices] <- predict_tree(tree$right, data[right_indices, , drop = FALSE])
    }
    
    return(predictions)
  }
  
  # Aggregate votes
  votes <- sapply(forest, function(tree_info) {
    predict_tree(tree_info, new_data)
  })
  
  # Majority voting for final prediction
  final_prediction <- apply(votes, 1, function(x) {
    names(sort(table(x), decreasing = TRUE))[1]
  })
  
  return(final_prediction)
}
