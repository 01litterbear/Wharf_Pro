
library(dplyr)
library(hrbrthemes)
library(ggplot2)
library(ggExtra)
yj1 = read.csv('pile_1_top_Ux.csv')
cp <- yj1[, c(1, 6)] 
data <- cp

# 计算均值向量
mu <- colMeans(data)
# 计算协方差矩阵
S <- cov(data)
# 计算协方差矩阵的逆矩阵
S_inv <- solve(S)
# 计算每个数据点的马氏距离
mahalanobis_distances <- numeric(nrow(data))
for (i in 1:nrow(data)) {
  diff <- data[i, ] - mu
  vc <- as.matrix(diff)
  vvc <- vc %*% S_inv
  mahalanobis_distances[i] <- sqrt( vvc %*% t(diff))
}
View(mahalanobis_distances)


p <- ggplot(data, aes(x = cref, y = Ux)) +
  geom_point()
p_marginal <- ggMarginal(p, type = "histogram")
print(p_marginal)

# 离群点可以通过多种方法识别。以下是一个简单的例子，基于 Z 分数来标记离群点：
# 定义离群点：绝对 Z 分数大于 2 的点
data$z_x <- (data$x - mean(data$x)) / sd(data$x)
data$z_y <- (data$y - mean(data$y)) / sd(data$y)
data$outlier <- abs(data$z_x) > 2 | abs(data$z_y) > 2

# 基于卡方分布的阈值
threshold_chi2 = qchisq(0.95, df = ncol(data))

# 基于统计特性的阈值
mean_distance = mean(mahalanobis_distances)
std_distance = sd(mahalanobis_distances)
threshold_stat = mean_distance + 2 * std_distance # k 取 3

# 基于百分位数的阈值
threshold_percentile = quantile(mahalanobis_distances, 0.95)

# 查找离群点
outliers_chi2 = which(mahalanobis_distances > threshold_chi2)
outliers_stat = which(mahalanobis_distances > threshold_stat)
outliers_percentile = which(mahalanobis_distances > threshold_percentile)

data$mahalanobis_distances <- mahalanobis_distances
data$outlier <- mahalanobis_distances > threshold_chi2
View(outliers_chi2)

p1 <- ggplot(data, aes(x = cref, y = Ux)) +
  geom_point(aes(color = outlier),size=2,alpha = 8/10) +
  scale_color_manual(values = c("blue", "red"), labels = c("FALSE", "TRUE")) +
  theme_minimal() + 
  theme(legend.position =  c(0.15,0.7),
        
        legend.background = element_rect(
          # fill = "lightblue", # 填充色
          colour = "grey", # 框线色
          size = 0.5 ) ) # 线条宽度 # 自定义图例位置

# 使用 ggMarginal 添加边际分布
p_marginal <- ggMarginal(p1, type = "histogram",
                         xparams = list(fill ="orange"),
                         yparams = list(fill ="skyblue")
                         )
print(p_marginal)





