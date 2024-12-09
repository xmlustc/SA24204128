## -----------------------------------------------------------------------------
set.seed(123)
data <- rnorm(1000, mean = 0, sd = 1)
hist(data, breaks = 30, main = "正态分布直方图", xlab = "值", ylab = "频数")

## -----------------------------------------------------------------------------
set.seed(1213)
group_a <- rnorm(50, mean = 5, sd = 2)
group_b <- rnorm(50, mean = 10, sd = 3)
data_summary <- data.frame(
  Group = c("A", "B"),
  Mean = c(mean(group_a), mean(group_b)),
  SD = c(sd(group_a), sd(group_b)),
  Median = c(median(group_a), median(group_b)),
  Min = c(min(group_a), min(group_b)),
  Max = c(max(group_a), max(group_b))
)
knitr::kable(data_summary, caption = "组A和组B的描述性统计量")

## -----------------------------------------------------------------------------
set.seed(123)
x <- rnorm(100, mean = 5, sd = 2)
y <- 2 + 1.5 * x + rnorm(100)
plot(x, y, main = "散点图与回归直线", xlab = "X", ylab = "Y")
model <- lm(y ~ x)
abline(model, col = "red")
summary(model)

## -----------------------------------------------------------------------------
set.seed(1212)   #设置随机种子

# Rayleigh分布随机数生成函数
generate_rayleigh <- function(n, sigma) {
  u <- runif(n)  # 生成n个(0,1)之间的均匀分布随机数
  x <- sigma * sqrt(-2 * log(1 - u))
  return(x)
}

# 设置不同的sigma值
sigma_values <- c(1, 2, 3)
par(mfrow = c(1, length(sigma_values)))  # 设置多图排列

# 对每个sigma值生成样本并绘制直方图
for (sigma in sigma_values) {
  samples <- generate_rayleigh(1000, sigma)
  
  # 绘制样本的直方图，并标出理论峰值sigma
  hist(samples, breaks = 30, main = paste("Rayleigh分布 (σ =", sigma, ")"),
       xlab = "值", ylab = "频率/组距", probability = TRUE)
  abline(v = sigma, col = "red", lwd = 2, lty = 2)  # 理论峰值
  
  # 叠加真实的Rayleigh分布密度函数
  curve((x / sigma^2) * exp(-x^2 / (2 * sigma^2)), 
        from = 0, to = max(samples), add = TRUE, col = "blue", lwd = 2)
}

par(mfrow = c(1, 1))  # 恢复图形布局

## ----fig.width=7, fig.height=5------------------------------------------------
set.seed(1212)   #设置随机种子

# 正态混合分布样本生成函数
generate_mixture <- function(n, p1) {
  # 生成1000个0和1的样本，以概率p1生成0，其余生成1
  component <- rbinom(n, 1, p1)
  # 根据component选择分布N(0,1)或N(3,1)
  sample <- ifelse(component == 0, rnorm(n, 0, 1), rnorm(n, 3, 1))
  return(sample)
}

# 设置初始的p1值
p1_values <- c(0.25, 0.4, 0.5, 0.6, 0.75)
par(mfrow = c(1, length(p1_values)))  # 设置多图排列

# 对不同的p1值生成样本并绘制直方图
for (p1 in p1_values) {
  samples <- generate_mixture(1000, p1)
  
  # 绘制样本直方图
  hist(samples, breaks = 30, probability = TRUE, 
       main = paste("Normal Mixture (p1 =", p1, ")"),
       xlab = "值", ylab = "频率/组距")
  
}

par(mfrow = c(1, 1))  # 恢复图形布局

## -----------------------------------------------------------------------------
set.seed(1213)  # 设置随机种子

# 复合泊松-伽马过程模拟函数
simulate_compound_poisson_gamma <- function(lambda, alpha, beta, t, num_simulations) {
  results <- numeric(num_simulations)
  
  for (i in 1:num_simulations) {
    # 生成泊松过程的事件数量
    N_t <- rpois(1, lambda * t)
    
    # 如果N_t > 0，则生成N_t个Gamma(alpha, beta)随机变量并求和
    if (N_t > 0) {
      Y <- rgamma(N_t, shape = alpha, scale = beta)
      results[i] <- sum(Y)
    } else {
      results[i] <- 0  # 如果N_t = 0，则X(t) = 0
    }
  }
  
  return(results)
}

# 参数设置
parameter_sets <- list(
  list(lambda = 2, alpha = 3, beta = 1.5),
  list(lambda = 3, alpha = 2, beta = 2.0),
  list(lambda = 1, alpha = 4, beta = 0.8)
)

# 模拟参数
t <- 10
num_simulations <- 10000
results <- data.frame()

# 对每一组参数进行模拟和理论计算
for (params in parameter_sets) {
  lambda <- params$lambda
  alpha <- params$alpha
  beta <- params$beta
  
  # 模拟复合泊松过程
  simulated_values <- simulate_compound_poisson_gamma(lambda, alpha, beta, t, num_simulations)
  
  # 计算模拟均值和方差
  simulated_mean <- mean(simulated_values)
  simulated_variance <- var(simulated_values)
  
  # 计算理论均值和方差
  theoretical_mean <- lambda * t * alpha * beta
  theoretical_variance <- lambda * t * (alpha * beta^2 + (alpha * beta)^2)
  
  # 将结果添加到数据框
  results <- rbind(results, data.frame(
    Lambda = lambda,
    Alpha = alpha,
    Beta = beta,
    Simulated_Mean = simulated_mean,
    Theoretical_Mean = theoretical_mean,
    Simulated_Variance = simulated_variance,
    Theoretical_Variance = theoretical_variance
  ))
}

# 打印结果表格
print(results)

## -----------------------------------------------------------------------------
# 设置随机种子以便结果可重现
set.seed(1212)
# 设置参数
m <- 1e5
alpha <- 3
beta <- 3
r <- gamma(alpha + beta) / (gamma(alpha) * gamma(beta))

# 定义 f 函数
f <- function(x, y) {
  r * x * (y^(alpha - 1) * (1 - y)^(beta - 1))
}

# 要计算的 x 值
x_values <- seq(0.1, 0.9, by = 0.1)

# 生成随机样本
y_values <- lapply(x_values, function(x) runif(m, min = 0, max = x))

# 计算每个 x 的蒙特卡洛估计
monte_carlo_estimates <- sapply(1:length(x_values), function(i) {
  mean(f(x_values[i], y_values[[i]]))
})

# 计算实际的 CDF 值
actual_cdf_values <- pbeta(x_values, shape1 = alpha, shape2 = beta)

# 合并结果到数据框
results <- data.frame(
  x = x_values,
  monte_carlo_estimate = monte_carlo_estimates,
  actual_cdf_value = actual_cdf_values
)

# 输出结果
print(results)

## -----------------------------------------------------------------------------
# 设置随机种子以便结果可重现
set.seed(1212)
# 生成 Rayleigh 分布样本的函数
rayleigh_sample <- function(sigma, size) {
  U <- runif(size)  # 生成 U(0,1) 的随机数
  return(sigma * sqrt(-2 * log(U)))  # 通过逆变换法生成 Rayleigh 样本
}

# 使用对偶变量生成 Rayleigh 样本
rayleigh_antithetic <- function(sigma, size) {
  U <- runif(size)  # 生成 U(0,1) 的随机数
  X <- sigma * sqrt(-2 * log(U))  # 原始 Rayleigh 样本
  X_prime <- sigma * sqrt(-2 * log(1 - U))  # 对偶 Rayleigh 样本
  return(list(X = X, X_prime = X_prime))  # 返回一对对偶变量
}
# 计算方差减小率
variance_reduction <- function(sigma, size) {
  # 生成独立的样本
  X1 <- rayleigh_sample(sigma, size)
  X2 <- rayleigh_sample(sigma, size)
  independent_mean <- (X1 + X2) / 2  # 计算独立样本的平均值
  
  # 生成对偶变量样本
  antithetic_samples <- rayleigh_antithetic(sigma, size)
  X <- antithetic_samples$X
  X_prime <- antithetic_samples$X_prime
  antithetic_mean <- (X + X_prime) / 2  # 计算对偶变量的平均值
  
  # 计算方差
  var_independent <- var(independent_mean)
  var_antithetic <- var(antithetic_mean)
  
  # 计算方差减小率
  reduction <- 100 * (var_independent - var_antithetic) / var_independent
  return(list(reduction = reduction, var_independent = var_independent, var_antithetic = var_antithetic))
}

# 运行示例
sigma <- 1
size <- 1e5
result <- variance_reduction(sigma, size)

cat("独立样本的方差:", result$var_independent, "\n")
cat("对偶样本的方差:", result$var_antithetic, "\n")
cat("方差减小率:", result$reduction, "%\n")


## -----------------------------------------------------------------------------
# 设置随机种子以便结果可重现
set.seed(1212)

# 定义 g(x)
g <- function(x) {
  ifelse(x < 1, 0, (x^2 / sqrt(2 * pi)) * exp(-x^2 / 2))
}

# 定义 f1(x) 和 f2(x)
f1 <- function(x) {
  (1 / sqrt(2 * pi)) * exp(-x^2 / 2)
}

f2 <- function(x) {
  x * exp(-x^2 / 2)
}

# 生成 Rayleigh 分布样本的函数
rayleigh_sample <- function(sigma, size) {
  U <- runif(size)  # 生成 U(0,1) 的随机数
  return(sigma * sqrt(-2 * log(U)))  # 通过逆变换法生成 Rayleigh 样本
}

# 从 f1 和 f2 中生成样本
sample_from_f1 <- function(n) {
  rnorm(n, mean = 0, sd = 1)  # 从正态分布中生成样本
}

sample_from_f2 <- function(n) {
  rayleigh_sample(sigma = 1, size = n)  # 使用自定义函数生成Rayleigh样本
}

# 进行重要性采样
n <- 100000  # 样本大小
num_runs <- 100  # 重复次数
II1_values <- numeric(num_runs)
II2_values <- numeric(num_runs)

for (i in 1:num_runs) {
  tt1 <- sample_from_f1(n)
  I1 <- g(tt1) / f1(tt1)
  II1_values[i] <- mean(I1)
  
  tt2 <- sample_from_f2(n)
  I2 <- g(tt2) / f2(tt2)
  II2_values[i] <- mean(I2)
}

# 计算方差
var_II1 <- var(II1_values)
var_II2 <- var(II2_values)

# 输出结果
cat("Estimate using f1:", mean(II1_values), "\n")
cat("Variance using f1:", var_II1, "\n")
cat("Estimate using f2:", mean(II2_values), "\n")
cat("Variance using f2:", var_II2, "\n")

# 真实的积分值（可以通过数值积分计算）
true_value <- integrate(g, lower = 1, upper = Inf)$value
cat("True value of the integral:", true_value, "\n")


## -----------------------------------------------------------------------------
# 加载必要的库
library(ggplot2)
# 设置随机种子以便结果可重现
set.seed(1212)
# 快速排序算法
quicksort <- function(arr) {
  if(length(arr) <= 1) {
    return(arr)
  }
  
  pivot <- arr[sample(length(arr), 1)]  # 随机选择一个枢轴
  left <- arr[arr < pivot]
  right <- arr[arr > pivot]
  
  return(c(quicksort(left), pivot, quicksort(right)))
}

# 运行蒙特卡洛实验的函数
run_experiment <- function(n, simulations = 100) {
  times <- numeric(simulations)
  
  for (i in 1:simulations) {
    permuted_numbers <- sample(1:n, n)  # 生成随机排列
    start_time <- Sys.time()  # 开始计时
    quicksort(permuted_numbers)  # 排序
    end_time <- Sys.time()  # 结束计时
    times[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
  }
  
  return(mean(times))  # 返回平均时间
}

# 测试的 n 值
n_values <- c(10^4, 2 * 10^4, 4 * 10^4, 6 * 10^4, 8 * 10^4)
results <- data.frame(n = n_values, a_n = NA)

# 对每个 n 运行实验
for (n in n_values) {
  results$a_n[results$n == n] <- run_experiment(n)
}

# 计算 t_n = n * log(n)
results$t_n <- results$n * log(results$n)

# 线性回归
model <- lm(a_n ~ t_n, data = results)

# 回归模型的摘要
summary(model)

# 绘制结果
ggplot(results, aes(x = t_n, y = a_n)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "平均计算时间与 t_n 的关系",
       x = "t_n = n log(n)",
       y = "平均计算时间（秒）") +
  theme_minimal()

## -----------------------------------------------------------------------------
# 加载必要的库
library(moments)

# 设置参数
n <- 10000  # 每组样本大小
num_groups <- 10000  # 组数

# 存储偏度值
skewness_values <- numeric(num_groups)

# 生成 10000 组标准正态分布数据并计算样本偏度
set.seed(1212)  # 设置随机种子以保证结果可重复
for (i in 1:num_groups) {
  sample_data <- rnorm(n)  # 从标准正态分布中生成样本
  skewness_values[i] <- skewness(sample_data)  # 计算偏度
}

# 计算样本偏度的分位数
quantiles <- quantile(skewness_values, probs = c(0.025, 0.05, 0.95, 0.975))
print("样本偏度的分位数:")
print(quantiles)

# 计算分位数的标准误差
# 先计算偏度分布的密度函数值
density_values <- dnorm(quantiles)  # 计算分位数的密度值

# 计算标准误差
standard_errors <- sqrt((c(0.025, 0.05, 0.95, 0.975) * (1 - c(0.025, 0.05, 0.95, 0.975))) / (num_groups * density_values^2))
cat("分位数的标准误差:\n")
print(standard_errors)

# 计算近似分布 N(0, 6/n) 的分位数
standard_deviation <- sqrt(6 / n)
approx_quantiles <- qnorm(c(0.025, 0.05, 0.95, 0.975), mean = 0, sd = standard_deviation)
print("近似分布 N(0, 6/n) 的分位数:")
print(approx_quantiles)

# 比较结果
comparison <- data.frame(
  Quantiles = c(0.025, 0.05, 0.95, 0.975),
  Sample_Quantiles = quantiles,
  Approx_Quantiles = approx_quantiles
)
print("分位数比较:")
print(comparison)

## -----------------------------------------------------------------------------
# 设置参数
set.seed(1212)  # 设置随机种子以保证可重复性
n <- 100  # 每次模拟的样本大小
simulations <- 10000  # 总模拟次数
alpha <- 0.05  # 显著性水平

# 存储功效的计数
pearson_power <- 0
spearman_power <- 0
kendall_power <- 0

# 进行模拟
for (i in 1:simulations) {
  # 生成数据
  X <- rexp(n)  # 生成一个有序的序列
  Y <- X^0.4 + rt(n, 10) # Y 是 X 的对数加上一定的噪声

  # 对于每种检验，进行假设检验
  pearson_test <- cor.test(X, Y)
  spearman_test <- cor.test(X, Y, method = "spearman")
  kendall_test <- cor.test(X, Y, method = "kendall")

  # 检查功效
  if (pearson_test$p.value < alpha) {
    pearson_power <- pearson_power + 1
  }
  if (spearman_test$p.value < alpha) {
    spearman_power <- spearman_power + 1
  }
  if (kendall_test$p.value < alpha) {
    kendall_power <- kendall_power + 1
  }
}

# 计算功效
pearson_effectiveness <- pearson_power / simulations
spearman_effectiveness <- spearman_power / simulations
kendall_effectiveness <- kendall_power / simulations

# 输出结果
cat("皮尔逊检验的功效:", pearson_effectiveness, "\n")
cat("斯皮尔曼检验的功效:", spearman_effectiveness, "\n")
cat("肯德尔检验的功效:", kendall_effectiveness, "\n")


## -----------------------------------------------------------------------------
# 设置参数
p1 <- 0.651  # 第一种方法的功效
p2 <- 0.676  # 第二种方法的功效
n1 <- 10000  # 第一种方法的实验次数
n2 <- 10000  # 第二种方法的实验次数

# 计算成功次数
X1 <- p1 * n1
X2 <- p2 * n2

# 计算合并比例
p <- (X1 + X2) / (n1 + n2)

# 计算Z统计量
Z <- (p1 - p2) / sqrt(p * (1 - p) * (1/n1 + 1/n2))

# 计算p值
p_value <- 2 * (1 - pnorm(abs(Z)))  # 双尾检验

# 输出结果
cat("Z统计量:", Z, "\n")
cat("p值:", p_value, "\n")

# 判断是否拒绝零假设
if (p_value < 0.05) {
  cat("拒绝零假设：两种方法的功效有显著差异。\n")
} else {
  cat("未拒绝零假设：两种方法的功效没有显著差异。\n")
}

## ----cars---------------------------------------------------------------------
# 设置随机种子以便结果可重现
set.seed(1212)
# 设置参数
N <- 1000  # 总的假设个数
n_null <- 950  # 真假设的数量
n_alt <- 50  # 假假设的数量
alpha <- 0.1  # 显著性水平
m <- 10000  # 模拟次数

# Bonferroni校正函数
bonferroni_correction <- function(p_values, alpha) {
  return(p.adjust(p_values, method = "bonferroni"))
}

# BH校正函数
bh_correction <- function(p_values, alpha) {
  return(p.adjust(p_values, method = "BH"))
}

# 进行一次假设检验的模拟
simulate_hypothesis_test <- function() {
  # 真假设的 p 值（均匀分布）
  p_null <- runif(n_null)
  
  # 假假设的 p 值（Beta 分布）
  p_alt <- rbeta(n_alt, 0.1, 1)
  
  # 合并 p 值
  p_values <- c(p_null, p_alt)
  
  # Bonferroni 校正
  bonferroni_p <- bonferroni_correction(p_values, alpha)
  
  # BH 校正
  bh_p <- bh_correction(p_values, alpha)
  
  return(list(p_values = p_values, bonferroni_p = bonferroni_p, bh_p = bh_p))
}

# 初始化结果储存变量
bonferroni_results <- data.frame(FWER = 0, FDR = 0, TPR = 0)
bh_results <- data.frame(FWER = 0, FDR = 0, TPR = 0)

# 多次模拟实验
for (i in 1:m) {
  result <- simulate_hypothesis_test()
  
  # Bonferroni校正结果
  bonferroni_reject <- result$bonferroni_p <= alpha
  bonferroni_FWER <- any(bonferroni_reject[1:n_null])
  bonferroni_FDR <- sum(bonferroni_reject[1:n_null]) / max(1, sum(bonferroni_reject))
  bonferroni_TPR <- sum(bonferroni_reject[(n_null + 1):N]) / n_alt
  
  bonferroni_results$FWER <- bonferroni_results$FWER + bonferroni_FWER
  bonferroni_results$FDR <- bonferroni_results$FDR + bonferroni_FDR
  bonferroni_results$TPR <- bonferroni_results$TPR + bonferroni_TPR
  
  # BH校正结果
  bh_reject <- result$bh_p <= alpha
  bh_FWER <- any(bh_reject[1:n_null])
  bh_FDR <- sum(bh_reject[1:n_null]) / max(1, sum(bh_reject))
  bh_TPR <- sum(bh_reject[(n_null + 1):N]) / n_alt
  
  bh_results$FWER <- bh_results$FWER + bh_FWER
  bh_results$FDR <- bh_results$FDR + bh_FDR
  bh_results$TPR <- bh_results$TPR + bh_TPR
}

# 计算平均值
bonferroni_results <- bonferroni_results / m
bh_results <- bh_results / m

# 创建结果表格
results_table <- data.frame(
  "Bonferroni correction" = c(bonferroni_results$FWER, bonferroni_results$FDR, bonferroni_results$TPR),
  "B-H correction" = c(bh_results$FWER, bh_results$FDR, bh_results$TPR)
)
rownames(results_table) <- c("FWER", "FDR", "TPR")

# 输出结果表格
print(results_table)

## -----------------------------------------------------------------------------
# 加载数据和库
library(boot)

# 设置随机种子以便结果可重现
set.seed(1212)

# 空调设备故障时间间隔数据
data <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)

# 计算 MLE（最大似然估计）
lambda_mle <- 1 / mean(data)
cat("MLE of lambda:", lambda_mle, "\n")

# 定义 Bootstrap 函数
bootstrap_function <- function(data, indices) {
  resample_data <- data[indices]
  lambda_hat <- 1 / mean(resample_data)
  return(lambda_hat)
}

# 设置 Bootstrap 迭代次数
B <- 1000

# 使用 boot 函数进行 Bootstrap
boot_results <- boot(data, statistic = bootstrap_function, R = B)

# 计算偏差和标准误差
bootstrap_mean <- mean(boot_results$t)
bias <- bootstrap_mean - lambda_mle
se <- sd(boot_results$t)

# 输出结果
cat("Bootstrap Bias:", bias, "\n")
cat("Bootstrap Standard Error:", se, "\n")

# 查看 Bootstrap 结果
boot.ci(boot_results, type = "basic")

## -----------------------------------------------------------------------------
# 加载必要库
library(boot)

# 空调设备故障时间间隔数据
data <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)

# MLE of lambda (风险率)
lambda_mle <- 1 / mean(data)
mean_failure_time <- 1 / lambda_mle

# 定义 Bootstrap 函数，用于估计1/λ（故障时间的均值）
bootstrap_function <- function(data, indices) {
  resample_data <- data[indices]
  lambda_hat <- 1 / mean(resample_data)
  return(1 / lambda_hat)  # 返回故障时间的均值
}

# 设置 Bootstrap 迭代次数
B <- 1000

# 使用 boot 函数进行 Bootstrap
boot_results <- boot(data, statistic = bootstrap_function, R = B)

# 计算标准正态法置信区间
z_star <- qnorm(0.975)
se <- sd(boot_results$t)
ci_standard_normal <- c(mean_failure_time - z_star * se, mean_failure_time + z_star * se)

# 计算基础法置信区间
ci_basic <- boot.ci(boot_results, type = "basic")

# 计算百分位法置信区间
ci_percentile <- boot.ci(boot_results, type = "perc")

# 计算 BCa 法置信区间
ci_bca <- boot.ci(boot_results, type = "bca")

# 输出所有置信区间
cat("Standard Normal CI: ", ci_standard_normal, "\n")
cat("Basic CI: ", ci_basic$basic[4:5], "\n")
cat("Percentile CI: ", ci_percentile$percent[4:5], "\n")
cat("BCa CI: ", ci_bca$bca[4:5], "\n")

## -----------------------------------------------------------------------------
rm(list = ls())    # 删除所有对象
# 加载必要的库
library(bootstrap)
data1<-data(scor)

# 计算样本协方差矩阵
sample_cov <- cov(scor)

# 计算协方差矩阵的特征值
eigenvalues <- eigen(sample_cov)$values

# 计算 theta 的估计值
theta_hat <- eigenvalues[1] / sum(eigenvalues)

# Jackknife方法计算偏差和标准误差
n <- nrow(scor)  # 样本量
theta_jackknife <- numeric(n)

# 对每个样本点，计算去掉该点后的 theta
for (i in 1:n) {
  # 去掉第 i 个学生的数据
  scor_jack <- scor[-i, ]
  # 计算新的协方差矩阵
  cov_jack <- cov(scor_jack)
  # 计算新的特征值
  eigenvalues_jack <- eigen(cov_jack)$values
  # 计算新的 theta
  theta_jackknife[i] <- eigenvalues_jack[1] / sum(eigenvalues_jack)
}

# 计算 Jackknife 偏差
theta_mean_jack <- mean(theta_jackknife)
jackknife_bias <- (n - 1) * (theta_mean_jack - theta_hat)

# 计算 Jackknife 标准误差
jackknife_se <- sqrt((n - 1) * mean((theta_jackknife - theta_mean_jack)^2))

# 输出结果
cat("Theta estimate (theta_hat):", theta_hat, "\n")
cat("Jackknife Bias estimate:", jackknife_bias, "\n")
cat("Jackknife Standard Error estimate:", jackknife_se, "\n")


## -----------------------------------------------------------------------------
library(DAAG)
attach(ironslag)
n <- length(magnetic) #in DAAG ironslag
e1 <- e2 <- e3 <- e4 <- numeric(n)
# for n-fold cross validation
# fit models on leave-one-out samples
for (k in 1:n) {
y <- magnetic[-k]
x <- chemical[-k]

J1 <- lm(y ~ x)
yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
e1[k] <- magnetic[k]- yhat1

J2 <- lm(y ~ x + I(x^2))
yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
J2$coef[3] * chemical[k]^2
e2[k] <- magnetic[k]- yhat2

J3 <- lm(log(y) ~ x)
logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
yhat3 <- exp(logyhat3)
e3[k] <- magnetic[k]- yhat3

J4 <- lm(y ~ x + I(x^2) + I(x^3))
yhat4 <- J4$coef[1] + J4$coef[2] * chemical[k] +
J4$coef[3] * chemical[k]^2+J4$coef[4] * chemical[k]^3
e4[k] <- magnetic[k]- yhat4
}
c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))
y <- magnetic
x <- chemical
J1 <- lm(y ~ x)
J2 <- lm(y ~ x + I(x^2))
J3 <- lm(log(y) ~ x)
J4 <- lm(y ~ x + I(x^2) + I(x^3))
summary(J1)$adj.r.squared
summary(J2)$adj.r.squared
summary(J3)$adj.r.squared
summary(J4)$adj.r.squared

## -----------------------------------------------------------------------------
rm(list = ls())    # 删除所有对象

# 加载数据
data(chickwts)

# 提取soybean和linseed组的重量数据
sample.x <- sort(as.vector(chickwts$weight[chickwts$feed == "soybean"]))
sample.y <- sort(as.vector(chickwts$weight[chickwts$feed == "linseed"]))

# 定义Cramér-von Mises统计量计算函数
cvm_statistic <- function(x, y) {
  n <- length(x)
  m <- length(y)
  # combined <- c(x, y)
  ecdf_x <- ecdf(x)
  ecdf_y <- ecdf(y)
  
  # 计算Cramér-von Mises统计量
  cvm <- (m*n)/(m+n)^2 * (sum((ecdf_x(x) - ecdf_y(x))^2)+sum((ecdf_x(y) - ecdf_y(y))^2))
  return(cvm)
}

# 计算原始样本的Cramér-von Mises统计量
original_cvm <- cvm_statistic(sample.x, sample.y)

# 定义置换检验次数
n_permutations <- 10000
perm_cvm <- numeric(n_permutations)

# 进行置换检验
set.seed(1213)
for (i in 1:n_permutations) {
  combined <- sample(c(sample.x, sample.y))
  x_perm <- combined[1:length(sample.x)]
  y_perm <- combined[(length(sample.x) + 1):length(combined)]
  
  perm_cvm[i] <- cvm_statistic(x_perm, y_perm)
}

# 计算p值
p_value <- mean(abs(perm_cvm) >= abs(original_cvm))

# 输出结果
cat("原始样本的Cramér-von Mises统计量:", original_cvm, "\n")
cat("置换检验的p值:", p_value, "\n")



## -----------------------------------------------------------------------------
# 加载数据
data(chickwts)

# 使用R介绍cor.test函数的样例数据
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)


# 使用 cor.test 函数计算 Spearman 相关系数的 p 值
spearman_result <- cor.test(x, y, method = "spearman")
#cat("Spearman 相关系数:", spearman_result$estimate, "\n")
cat("cor.test 给出的 p 值:", spearman_result$p.value, "\n")

# 定义 Spearman 相关系数计算函数
spearman_stat <- function(x, y) {
  cor(x, y, method = "spearman")
}

# 计算原始样本的 Spearman 相关系数
original_spearman <- spearman_stat(x, y)

# 定义置换检验的次数
n_permutations <- 10000
perm_spearman <- numeric(n_permutations)

# 进行置换检验
set.seed(1212)
for (i in 1:n_permutations) {
  y_perm <- sample(y)  # 随机打乱 y 的顺序
  perm_spearman[i] <- spearman_stat(x, y_perm)
}

# 计算置换检验的 p 值
p_value_permutation <- mean(abs(perm_spearman) >= abs(original_spearman))

# 输出结果
#cat("原始样本的 Spearman 相关系数:", original_spearman, "\n")
cat("置换检验的 p 值:", p_value_permutation, "\n")


## -----------------------------------------------------------------------------
set.seed(1213)  # 固定随机种子

# Metropolis-Hastings采样器
metropolis_cauchy <- function(n, burn_in = 1000) {
  # 初始值
  x <- numeric(n + burn_in)
  x[1] <- 0
  
  for (i in 2:(n + burn_in)) {
    # 从提议分布N(x[i-1], 1)采样候选点
    proposal <- rnorm(1, mean = x[i - 1], sd = 1)
    
    # 计算接受概率
    alpha <- dcauchy(proposal) / dcauchy(x[i - 1])
    if (runif(1) < alpha) {
      x[i] <- proposal
    } else {
      x[i] <- x[i - 1]
    }
  }
  
  # 丢弃前burn_in个样本
  return(x[(burn_in + 1):(n + burn_in)])
}

# 参数设置
n <- 10000  # 所需样本数量
samples <- metropolis_cauchy(n)

# 计算生成样本的分位数
sample_deciles <- quantile(samples, probs = seq(0.1, 1, by = 0.1))

# 计算标准柯西分布的理论分位数
theoretical_deciles <- qcauchy(seq(0.1, 1, by = 0.1))

# 比较生成样本分位数与理论分位数
result <- data.frame(
  Decile = seq(0.1, 1, by = 0.1),
  Simulated = sample_deciles,
  Theoretical = theoretical_deciles
)

print(result)

## -----------------------------------------------------------------------------
set.seed(1212)  # 固定随机种子

# Gibbs采样器函数
gibbs_sampler <- function(a, b, n, num_samples) {
  # 初始化链
  x_chain <- numeric(num_samples)
  y_chain <- numeric(num_samples)
  
  # 初始值
  x_chain[1] <- 0
  y_chain[1] <- 0.5
  
  for (i in 2:num_samples) {
    # 给定当前y，采样x | y ~ Binomial(n, y)
    y <- y_chain[i - 1]
    x_chain[i] <- rbinom(1, n, y)
    
    # 给定当前x，采样y | x ~ Beta(x + a, n - x + b)
    x <- x_chain[i]
    y_chain[i] <- rbeta(1, x + a, n - x + b)
  }
  
  # 返回生成的链
  return(data.frame(x = x_chain, y = y_chain))
}

# 参数设置
a <- 4
b <- 2
n <- 10
num_samples <- 10000

# 运行Gibbs采样器
samples <- gibbs_sampler(a, b, n, num_samples)

# 丢弃前1000个样本
burn_in <- 1000
samples <- samples[-(1:burn_in), ]

# 显示前几组生成的样本
head(samples)
plot(samples$x,type='l')
plot(samples$y,type='l')
hist(samples$x)
hist(samples$y)

## -----------------------------------------------------------------------------
library(coda)
set.seed(1212)  # 固定随机种子
# Metropolis-Hastings 采样器函数
metropolis_cauchy <- function(n, burn_in = 1000, init = 0) {
  x <- numeric(n + burn_in)
  x[1] <- init
  
  for (i in 2:(n + burn_in)) {
    proposal <- rnorm(1, mean = x[i - 1], sd = 1)
    alpha <- dcauchy(proposal) / dcauchy(x[i - 1])
    if (runif(1) < alpha) {
      x[i] <- proposal
    } else {
      x[i] <- x[i - 1]
    }
  }
  return(x[(burn_in + 1):(n + burn_in)])
}

# 多条链的 Metropolis-Hastings 采样
num_chains <- 4
chain_length <- 10000
chains <- lapply(1:num_chains, function(i) metropolis_cauchy(chain_length, init = runif(1, -10, 10)))

# 将结果转换为 mcmc 列表
mh_mcmc <- mcmc.list(lapply(chains, mcmc))

# 计算 Gelman-Rubin 诊断
print(gelman.diag(mh_mcmc)$psrf[, "Point est."])

## -----------------------------------------------------------------------------
set.seed(1212)  # 固定随机种子
# Gibbs 采样器函数
gibbs_sampler <- function(a, b, n, num_samples, init_x = 0, init_y = 0.5) {
  x_chain <- numeric(num_samples)
  y_chain <- numeric(num_samples)
  x_chain[1] <- init_x
  y_chain[1] <- init_y
  
  for (i in 2:num_samples) {
    y <- y_chain[i - 1]
    x_chain[i] <- rbinom(1, n, y)
    
    x <- x_chain[i]
    y_chain[i] <- rbeta(1, x + a, n - x + b)
  }
  
  return(data.frame(x = x_chain, y = y_chain))
}

# 设置参数和链数量
a <- 2
b <- 2
n <- 10
num_samples <- 10000
num_chains <- 4

# 运行多条 Gibbs 采样链
gibbs_chains <- lapply(1:num_chains, function(i) gibbs_sampler(a, b, n, num_samples, init_x = sample(0:n, 1), init_y = runif(1)))

# 提取 x 和 y 链，计算 Gelman-Rubin 诊断
x_chains <- mcmc.list(lapply(gibbs_chains, function(chain) mcmc(chain$x)))
y_chains <- mcmc.list(lapply(gibbs_chains, function(chain) mcmc(chain$y)))

# 计算 x 和 y 的 Gelman-Rubin 诊断
gelman_diag_x <- gelman.diag(x_chains)
gelman_diag_y <- gelman.diag(y_chains)

print(gelman_diag_x$psrf[, "Point est."])
print(gelman_diag_y$psrf[, "Point est."])

## -----------------------------------------------------------------------------
# 定义函数计算向量的欧几里得范数
euclidean_norm <- function(a) {
  sqrt(sum(a^2))
}

# 定义第 k 项的计算函数
k_term <- function(k, a, d) {
  norm_a <- euclidean_norm(a)
  coef1 <- (-1)^k / (factorial(k) * 2^k)
  coef2 <- norm_a^(2 * k + 2) / ((2 * k + 1) * (2 * k + 2))
  gamma_part <- gamma((d + 1) / 2) * gamma(k + 3 / 2) / gamma(k + d / 2 + 1)
  term_k <- coef1 * coef2 * gamma_part
  return(term_k)
}

# 定义计算级数和的函数
series_sum <- function(a, d, tol = 1e-10, max_iter = 1000) {
  sum <- 0
  k <- 0
  repeat {
    term <- k_term(k, a, d)
    sum <- sum + term
    if (abs(term) < tol) { # 收敛判断
      break
    }
    k <- k + 1
  }
  return(sum)
}

# 计算当 a = (1, 2)^T, d = 2 时的级数和
a <- c(1, 2)
d <- 2
result <- series_sum(a, d)
cat("当 a = (1, 2)^T, d = 2 时的级数和为:", result, "\n")

## -----------------------------------------------------------------------------
# 加载必要的库
library(stats)

# 定义常量 k 值
k_values <- c(4:25, 100, 500, 1000)

# 定义 c_k 的计算函数
c_k <- function(a, k) {
  sqrt((a^2 * k) / (k + 1 - a^2))
}

# 计算 S_{k-1}(a) 和 S_k(a) 的函数
S_k <- function(a, k) {
  critical_value <- c_k(a,k)
  pt(-critical_value, df = k, lower.tail = FALSE)
}

# 定义找到 A(k) 的函数
find_A_k <- function(k) {
  target_func <- function(a) S_k(a, k-1) - S_k(a, k)
  result <- uniroot(target_func, lower = 0.001, upper = 2)$root
  return(result)
}

# 计算 A(k) 值
A_k_values <- sapply(k_values, find_A_k)

# 定义积分部分用于解方程的函数
left_integral <- function(c_k1, k) {
  integrate(function(u) (1 + u^2 / (k - 1))^(-k / 2), 0, c_k1)$value
}

right_integral <- function(c_k2, k) {
  integrate(function(u) (1 + u^2 / k)^(-(k + 1) / 2), 0, c_k2)$value
}

# 定义方程求解函数，找到满足方程的a值
solve_for_a <- function(k) {
  target_func <- function(a) {
    # 计算 c_{k-1} 和 c_k
    c_k1 <- c_k(a, k - 1)
    c_k2 <- c_k(a, k)
    
    # 计算方程的左侧和右侧
    
    left_side <- exp(lgamma(k / 2)-lgamma((k - 1) / 2))*(2 ) / (sqrt(pi * (k - 1))) * left_integral(c_k1, k)
    right_side <- exp(lgamma((k + 1) / 2)-lgamma((k) / 2))*(2) / (sqrt(pi * k)) * right_integral(c_k2, k)
    
    # 返回左右差值
    left_side - right_side
  }
  
  # 使用 uniroot 求解方程，找到 a 的值
  result <- uniroot(target_func,c(0.0001,2))$root
  return(result)
}

# 计算解方程得到的 a 值
a_values <- sapply(k_values, solve_for_a)

# 打印和比较 A(k) 与求解方程得到的 a 值
comparison <- data.frame(k = k_values, A_k = A_k_values, a_solved = a_values)
print(comparison)

## -----------------------------------------------------------------------------
# 初始化观测数据和参数
Y <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)
tau <- 1
n <- length(Y)

# 初始lambda的估计值（可以随机或使用观测值的均值）
lambda_est <- mean(Y)

# 设置迭代参数
tolerance <- 1e-6
max_iter <- 1000
iter <- 0
lambda_old <- 0
need.lambda <- c()

# EM算法迭代
while (abs(lambda_est - lambda_old) > tolerance && iter < max_iter) {
  iter <- iter + 1
  lambda_old <- lambda_est
  
  # E步：计算填补后的 T_i 的期望值
  T_filled <- sapply(Y, function(y) {
    if (y < tau) {
      return(y) # 未截尾的值直接取观测值
    } else {
      return(tau + lambda_est) # 截尾的情况
    }
  })
  
  # M步：更新lambda的估计值
  lambda_est <- mean(T_filled)
  need.lambda <- c(need.lambda,lambda_est)
}

cat("EM算法估计的lambda:", lambda_est, "\n")

# 观测数据的 MLE 估计（仅用未截尾的数据）
lambda_mle <- sum(Y)/7
cat("观测数据 MLE 估计的lambda:", lambda_mle, "\n")

## -----------------------------------------------------------------------------
# 加载线性规划包
library(lpSolve)

# 定义目标函数系数
obj <- c(4, 2, 9)

# 定义约束条件矩阵
const_mat <- matrix(c(
  2, 1, 1,   # 2x + y + z <= 2
  1, -1, 3   # x - y + 3z <= 3
), nrow = 2, byrow = TRUE)

# 定义约束条件的右侧值
const_rhs <- c(2, 3)

# 定义约束方向
const_dir <- c("<=", "<=")

# 求解线性规划
result <- lp("min", obj, const_mat, const_dir, const_rhs, all.int = FALSE)

# 输出结果
list(
  Optimal_Value = result$objval,
  Solution = result$solution
)

## -----------------------------------------------------------------------------
# 定义公式列表
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

# 加载数据集
data(mtcars)
# 初始化存储模型结果的列表
models_for <- list()

# 使用 for 循环拟合线性模型
for (i in seq_along(formulas)) {
  models_for[[i]] <- lm(formulas[[i]], data = mtcars)
}

# 使用 lapply 拟合线性模型
models_lapply <- lapply(formulas, function(f) lm(f, data = mtcars))


# 使用 for 循环查看系数
for (i in seq_along(models_for)) {
  cat("Model", i, "coefficients:\n")
  print(coef(models_for[[i]]))
  cat("\n")
}

# 使用 lapply 查看系数
lapply(models_lapply, coef)

## -----------------------------------------------------------------------------
# 创建引导样本列表
set.seed(123)  # 设置随机种子以便结果可重复
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), replace = TRUE)
  mtcars[rows, ]
})
# 初始化存储模型的列表
models_for <- list()

# 使用 for 循环对每个引导样本拟合模型
for (i in seq_along(bootstraps)) {
  models_for[[i]] <- lm(mpg ~ disp, data = bootstraps[[i]])
}

# 定义一个专用的拟合函数
fit_model <- lm  # 因为公式固定为 mpg ~ disp，因此可以直接传递公式

# 使用 lapply 对引导样本拟合模型
models_lapply <- lapply(bootstraps, fit_model, formula = mpg ~ disp)

# 使用 for 循环查看系数
for (i in seq_along(models_for)) {
  cat("Model", i, "coefficients:\n")
  print(coef(models_for[[i]]))
  cat("\n")
}

# 使用 lapply 查看系数
lapply(models_lapply, coef)


## -----------------------------------------------------------------------------
# 定义 rsq 函数，用于提取模型的 R^2
rsq <- function(mod) {
  summary(mod)$r.squared
}

# 提取 for 循环中每个模型的 R^2
r_squared_for <- numeric(length(models_for))  # 初始化存储 R^2 的向量
for (i in seq_along(models_for)) {
  r_squared_for[i] <- rsq(models_for[[i]])
}

# 查看结果
r_squared_for

# 使用 lapply 提取每个模型的 R^2
r_squared_lapply <- lapply(models_lapply, rsq)  

# 查看结果
r_squared_lapply

# 比较两种方法的结果是否一致
#all.equal(r_squared_for, r_squared_lapply)


## -----------------------------------------------------------------------------
trials <- replicate(
 100,
 t.test(rpois(10, 10), rpois(7, 10)),
 simplify = FALSE
 )
# 使用 sapply 和匿名函数提取 p-value
p_values_anonymous <- sapply(trials, function(trial) trial$p.value)

# 查看前几个 p-value
head(p_values_anonymous)

# 使用 [[ 直接提取 p-value
p_values_direct <- sapply(trials, `[[`, "p.value")

# 查看前几个 p-value
head(p_values_direct)

# 验证结果是否一致
all.equal(p_values_anonymous, p_values_direct)

## -----------------------------------------------------------------------------
my_lapply_variant <- function(FUN, ..., FUN.VALUE, USE.NAMES = TRUE) {
  # 参数说明：
  # FUN: 应用于每组输入的函数
  # ...: 多个并行输入
  # FUN.VALUE: 输出的预期类型或结构，用于 vapply 验证
  # USE.NAMES: 是否保留输出的名称
  
  # 使用 Map 在并行输入上迭代
  mapped_results <- Map(FUN, ...)
  
  # 使用 vapply 对每个结果进行验证并返回向量/矩阵
  result <- vapply(mapped_results, identity, FUN.VALUE, USE.NAMES = USE.NAMES)
  
  return(result)
}

# 定义并行输入，需要对它们的每组元素求和
x <- 1:5
y <- 6:10

# 应用函数：对 x 和 y 中的每组值求和
result <- my_lapply_variant(
  FUN = function(a, b) a + b,  # 自定义函数：求和
  x, y,                       # 并行输入
  FUN.VALUE = numeric(1),     # 输出为标量
  USE.NAMES = TRUE           # 不保留名称
)

print(result)  # 输出结果


## -----------------------------------------------------------------------------
fast_chisq_test <- function(observed, expected) {
  # 参数说明：
  # observed: 观察值的数值向量
  # expected: 期望值的数值向量
  
  # 检查输入向量是否长度一致
  if (length(observed) != length(expected)) {
    stop("observed 和 expected 的长度必须相同")
  }
  
  # 检查是否有非正值
  if (any(observed <= 0) || any(expected <= 0)) {
    stop("observed 和 expected 必须全为正数")
  }
  
  # 按卡方公式计算统计量
  chi_square_stat <- sum((observed - expected)^2 / expected)
  
  # 自由度
  df <- length(observed) - 1
  
  # 计算 p 值
  p_value <- pchisq(chi_square_stat, df, lower.tail = FALSE)
  
  # 返回结果
  return(list(
    statistic = chi_square_stat,
    df = df,
    p_value = p_value
  ))
}
# 定义观察值和期望值
observed <- c(10, 20, 30)
expected <- c(15, 25, 20)

# 调用快速卡方检验函数
result <- fast_chisq_test(observed, expected)

# 查看结果
print(result)

result_original <- chisq.test(x = observed, p = expected / sum(expected), rescale.p = TRUE)
result_original$statistic
result_original$p.value


## -----------------------------------------------------------------------------
# 自定义一个快速的 table 函数
fast_table <- function(x, y) {
  # 确保输入是整数向量
  if (!is.integer(x) || !is.integer(y)) {
    stop("Inputs must be integer vectors.")
  }
  # 获取 x 和 y 的唯一值范围
  ux <- sort(unique(x))
  uy <- sort(unique(y))
  # 初始化计数矩阵
  result <- matrix(0L, nrow = length(ux), ncol = length(uy))
  # 使用双向索引快速填充计数矩阵
  for (i in seq_along(x)) {
    result[match(x[i], ux), match(y[i], uy)] <- result[match(x[i], ux), match(y[i], uy)] + 1L
  }
  # 返回结果，带上行列名
  dimnames(result) <- list(ux, uy)
  return(result)
}

# 用于加速卡方检验的实现，包含 P 值计算
fast_chisq_test <- function(x, y) {
  # 确保输入是整数向量
  if (!is.integer(x) || !is.integer(y)) {
    stop("Inputs must be integer vectors.")
  }
  # 生成快速的列联表
  observed <- fast_table(x, y)
  # 计算期望频数
  row_sums <- rowSums(observed)
  col_sums <- colSums(observed)
  total <- sum(observed)
  expected <- outer(row_sums, col_sums) / total
  # 计算卡方统计量
  chisq_stat <- sum((observed - expected)^2 / expected)
  # 计算自由度
  df <- (nrow(observed) - 1) * (ncol(observed) - 1)
  # 计算 P 值
  p_value <- pchisq(chisq_stat, df, lower.tail = FALSE)
  # 返回结果，包括统计量和 P 值
  return(list(
    chisq_stat = chisq_stat,
    df = df,
    p_value = p_value
  ))
}

# 示例用法
x <- as.integer(c(1, 2, 1, 2, 1, 3, 2, 3, 3))
y <- as.integer(c(1, 2, 1, 1, 2, 3, 3, 1, 2))

# 快速计算列联表
cat("Fast Table Result:\n")
print(fast_table(x, y))

# 快速计算卡方检验统计量和 P 值
fast_result <- fast_chisq_test(x, y)

# 打印结果
cat("\nFast Chi-Square Test Result:\n")
cat("Chi-Square Statistic:", fast_result$chisq_stat, "\n")
cat("Degrees of Freedom:", fast_result$df, "\n")
cat("P-Value:", fast_result$p_value, "\n")

# 验证与标准 chisq.test 的结果是否一致
cat("\nStandard chisq.test Result:\n")
chisq_test_result <- chisq.test(fast_table(x, y))
print(chisq_test_result)


## -----------------------------------------------------------------------------
set.seed(1212)  # 确保可重复性
library(Rcpp)

cppFunction('
#include <Rcpp.h>
using namespace Rcpp;

// Gibbs 采样器
// [[Rcpp::export]]
List gibbs_sampler(int n, double a, double b, int num_iter, double y_init) {
  // 存储采样结果
  NumericVector x_samples(num_iter);
  NumericVector y_samples(num_iter);
  
  // 初始化变量
  double y = y_init;
  int x = 0;
  
  for (int i = 0; i < num_iter; i++) {
    // 从 Binomial(n, y) 中采样 x | y
    x = R::rbinom(n, y);
    
    // 从 Beta(x + a, n - x + b) 中采样 y | x
    y = R::rbeta(x + a, n - x + b);
    
    // 保存采样结果
    x_samples[i] = x;
    y_samples[i] = y;
  }
  
  return List::create(
    Named("x_samples") = x_samples,
    Named("y_samples") = y_samples
  );
}
')

# 运行示例
n <- 10
a <- 2
b <- 3
num_iter <- 1000
y_init <- 0.5

result <- gibbs_sampler(n, a, b, num_iter, y_init)

# 提取采样结果
x_samples <- result$x_samples
y_samples <- result$y_samples

# 绘制采样轨迹
plot(x_samples, type = "l", col = "blue", main = "X 的采样轨迹")
plot(y_samples, type = "l", col = "red", main = "Y 的采样轨迹")


## -----------------------------------------------------------------------------

gibbs_sampler_r <- function(n, a, b, num_iter, y_init) {
  x_samples <- numeric(num_iter)
  y_samples <- numeric(num_iter)
  
  y <- y_init
  for (i in 1:num_iter) {
    # 从 Binomial(n, y) 中采样 x | y
    x <- rbinom(1, n, y)
    # 从 Beta(x + a, n - x + b) 中采样 y | x
    y <- rbeta(1, x + a, n - x + b)
    
    x_samples[i] <- x
    y_samples[i] <- y
  }
  
  list(x_samples = x_samples, y_samples = y_samples)
}

# 参数设置
set.seed(1212)  # 确保可重复性
n <- 10
a <- 2
b <- 3
num_iter <- 1000
y_init <- 0.5

# 调用 C++ 实现的采样器
cpp_result <- gibbs_sampler(n, a, b, num_iter, y_init)
cpp_x_samples <- cpp_result$x_samples
cpp_y_samples <- cpp_result$y_samples

# 调用 R 实现的采样器
r_result <- gibbs_sampler_r(n, a, b, num_iter, y_init)
r_x_samples <- r_result$x_samples
r_y_samples <- r_result$y_samples

# 比较 x_samples
qqplot(r_x_samples, cpp_x_samples, main = "QQ Plot of x_samples (R vs C++)", xlab = "R Samples", ylab = "C++ Samples")
abline(0, 1, col = "red")

# 比较 y_samples
qqplot(r_y_samples, cpp_y_samples, main = "QQ Plot of y_samples (R vs C++)", xlab = "R Samples", ylab = "C++ Samples")
abline(0, 1, col = "blue")

## -----------------------------------------------------------------------------
# 加载必要的包
library(microbenchmark)


# 使用 microbenchmark 比较计算时间
benchmark_result <- microbenchmark(
  Cpp = gibbs_sampler(n, a, b, num_iter, y_init),
  R = gibbs_sampler_r(n, a, b, num_iter, y_init),
  times = 10
)


# 显示运行时间结果
print(benchmark_result)

# 绘制时间比较图
boxplot(benchmark_result, main = "Gibbs Sampler Performance (C++ vs R)", 
        ylab = "Execution Time (ms)", col = c("lightblue", "lightgreen"))

