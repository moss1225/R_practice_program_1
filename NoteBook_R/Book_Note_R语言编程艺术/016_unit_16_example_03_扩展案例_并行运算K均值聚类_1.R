# 数据是MASS包自带的Boston数据集
# K均值函数使用stats包中的kmeans()
library(MASS)
result <- kmeans(Boston , 4 , nstart = 100)

# 在使用并行计算前，首先看看使用lapply函数的运算情况
results <- lapply(rep(24 , 4) , function(nstart) kmeans(Boston , 4 , nstart = nstart))
i <- sapply(results , function(result) result$tot.withinss)
result <- results[[which.min(i)]]

# 使用clusterApply并行运算
library( snow )
cl <- makeCluster(4 , type="SOCK")
ignore <- clusterEvalQ(cl , {library(MASS);NULL})
# 用clusterEvalQ初始化内核，在每一个内核中载入包
results <- clusterApply(cl , rep(24 , 4) , function(nstart) kmeans(Boston , 4 , nstart = nstart))
i <- sapply(results , function(result) result$tot.withinss)
result<- results[[which.min(i)]]

