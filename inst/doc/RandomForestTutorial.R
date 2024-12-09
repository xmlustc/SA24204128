## -----------------------------------------------------------------------------
library(SA24204128)
#install.packages("SA24204128")

## -----------------------------------------------------------------------------
# 加载 iris 数据集
data(iris)

# 训练一棵决策树
tree <- decision_tree_train(iris, target = "Species", max_depth = 5)

# 打印决策树结构
print(tree)

## -----------------------------------------------------------------------------
# 训练一个包含 100 棵树的随机森林
forest <- randomForest_train(iris, target = "Species", n_trees = 100, max_depth = 5)

for (i in 1:min(3, length(forest))) {  # 打印最多前 3 棵树
  cat("Tree", i, "Root Node:\n")
  print(forest[[i]])  # 打印根节点
  cat("\n")
}

## -----------------------------------------------------------------------------
# 加载必要包
library(randomForest)  # 或者 library(ranger)

# 加载 iris 数据集
data(iris)

# 数据集划分
set.seed(14634)  # 设置随机种子以保证可重复性
train_indices <- sample(1:nrow(iris), 0.7 * nrow(iris))  # 70% 用于训练
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

# 1. 使用SA24204128
# 测量训练时间
time_custom_train <- system.time({
  forest_custom <- randomForest_train(train_data, target = "Species", n_trees = 100, max_depth = 10)
})

# 测量预测时间
time_custom_predict <- system.time({
  predictions_custom <- randomForest_predict(forest_custom, test_data[, -5])
})

# 计算准确性
accuracy_custom <- mean(predictions_custom == test_data$Species)

# 2. 使用 randomForest 包
# 测量训练时间
time_rf_train <- system.time({
  forest_rf <- randomForest(Species ~ ., data = train_data, ntree = 100, maxnodes = 10)
})

# 测量预测时间
time_rf_predict <- system.time({
  predictions_rf <- predict(forest_rf, test_data[, -5])
})

# 计算准确性
accuracy_rf <- mean(predictions_rf == test_data$Species)

# 3. 输出对比结果
results <- data.frame(
  Method = c("Custom Random Forest", "randomForest Package"),
  Training_Time = c(time_custom_train["elapsed"], time_rf_train["elapsed"]),
  Prediction_Time = c(time_custom_predict["elapsed"], time_rf_predict["elapsed"]),
  Accuracy = c(accuracy_custom, accuracy_rf)
)

print(results)


