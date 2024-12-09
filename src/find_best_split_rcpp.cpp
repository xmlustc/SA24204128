#include <Rcpp.h>
using namespace Rcpp;
//' @name find_best_split_rcpp
//' @title Find the Best Split Using Rcpp
//' @description This function finds the best feature and threshold to split data using Gini impurity, implemented with Rcpp for speed.
//' @param data A numeric matrix where rows represent observations and columns represent features.
//' @param target An integer vector containing the target variable (class labels) corresponding to the rows of \code{data}.
//' @return A list containing:
//' \itemize{
//'   \item \code{feature}: The index of the best feature to split on.
//'   \item \code{threshold}: The threshold value of the best split for the feature.
//' }
//' @examples
//' \dontrun{
//' data <- matrix(runif(100), nrow = 10, ncol = 10)
//' target <- sample(0:1, 10, replace = TRUE)
//' best_split <- find_best_split_rcpp(data, target)
//' print(best_split)
//' }
//' @export
// [[Rcpp::export]]
 List find_best_split_rcpp(NumericMatrix data, IntegerVector target) {
   
   int n_rows = data.nrow();
   int n_features = data.ncol();
   double best_gini = R_PosInf;  // Start with the best Gini impurity as infinity
   int best_feature = -1;
   double best_threshold = 0.0;
   
   // Function to compute Gini impurity for a given set of class labels
   auto compute_gini = [](IntegerVector target_subset) {
     int n = target_subset.size();
     if (n == 0) return 1.0;  // If the subset is empty, return max Gini
     
     // Count class frequencies manually to avoid using table()
     IntegerVector unique_classes = unique(target_subset);
     double gini = 1.0;
     
     for (int i = 0; i < unique_classes.size(); i++) {
       int class_count = sum(target_subset == unique_classes[i]);
       double p = class_count / double(n);
       gini -= p * p;
     }
     return gini;
   };
   
   // Iterate over each feature
   for (int feature = 0; feature < n_features; feature++) {
     NumericVector column = data(_, feature);  // Feature column
     
     // Get unique sorted threshold values
     NumericVector thresholds = unique(column);
     
     // Iterate over each possible threshold value
     for (int i = 0; i < thresholds.size(); i++) {
       double threshold = thresholds[i];
       
       // Split the data into two groups based on the threshold
       IntegerVector left_indices, right_indices;
       
       for (int j = 0; j < n_rows; j++) {
         if (column[j] <= threshold) {
           left_indices.push_back(j);
         } else {
           right_indices.push_back(j);
         }
       }
       
       // Skip if one of the subsets is empty
       if (left_indices.size() == 0 || right_indices.size() == 0) continue;
       
       // Extract the corresponding target values for the left and right subsets
       IntegerVector left_target = target[left_indices];
       IntegerVector right_target = target[right_indices];
       
       // Compute Gini impurity for the left and right subsets
       double gini_left = compute_gini(left_target);
       double gini_right = compute_gini(right_target);
       
       // Compute the weighted average Gini impurity for this split
       double weighted_gini = (left_target.size() * gini_left + right_target.size() * gini_right) / n_rows;
       
       // If this split is better (lower Gini impurity), update the best split
       if (weighted_gini < best_gini) {
         best_gini = weighted_gini;
         best_feature = feature;
         best_threshold = threshold;
       }
     }
   }
   
   return List::create(
     Named("feature") = best_feature,
     Named("threshold") = best_threshold
   );
 }
