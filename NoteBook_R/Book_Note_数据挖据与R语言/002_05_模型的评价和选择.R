# 平均值误差MAE

# *清理数据
library(rpart)
library(DMwR) # 载入包含基础数据和数据预处理函数的包
data(algae) # 载入数据
algae <- algae[-manyNAs(algae), ] # 去除缺失值很多的数据行
clean.algae <- knnImputation(algae , k=10) # 利用knnImputation，去除少部分NA值

# *创建模型
# 模型一：线性模型
lm.a1 <- lm(a1 ~ . , data = clean.algae[ , 1:12]) # 创建线性方程
final.lm <- step(lm.a1) # 利用step函数，对lm.a1进行自动优化
# 模型二：回归树模型
library(rpart) # 载入回归树所需的包
rt.a1 <- rpart( a1 ~ . , data = algae[ , 1:12]) # 创建回归树模型

# *预测数据
lm.prediction.a1 <- predict(final.lm , clean.algae) # 生产线性方程的预测数据
rt.prediction.a1 <- predict(rt.a1 , algae) # 生成回归树的预测数据

# *计算误差
# 计算一：计算平均绝对误差MAE
(mae.a1.lm <- mean(abs(lm.prediction.a1 - algae[ , "a1"] ))) # 计算预测值与真实值的误差的绝对值的平均值，此处是线性方程的预测值误差
(mae.a1.rt <- mean(abs(rt.prediction.a1 - algae[ , "a1"] ))) # 计算预测值与真实值的误差的绝对值的平均值，此处是回归树的预测值误差
# 计算二：计算均方误差MSE
(mse.a1.lm <- mean((lm.prediction.a1 - algae[ , "a1"])^2)) # 计算预测值与真实值的差值的平方，此行是线性方程
(mse.a1.rt<-mean((rt.prediction.a1 - algae[ , "a1"])^2)) # 计算预测值与真实值的差值的平方，此行是回归树
# 计算三：计算绝对标准化的误差NMSE
(mse.a1.lm <- mean((lm.prediction.a1 - algae[ , "a1"])^2))/mean(mean(algae[ ,'a1'] )-algae[ ,'a1'] ^2) # 计算预测值与真实值的差值的平方，此行是线性方程
(mse.a1.rt<-mean((rt.prediction.a1 - algae[ , "a1"])^2))/mean(mean(algae[ ,'a1'] )-algae[ ,'a1'] ^2)  # 计算预测值与真实值的差值的平方，此行是回归树
# 说明：NMSE是一个壁纸，其取值范围通常是0～1。如果模型表现优于值这个非常简单的基准模型预测，
#             那么NMSE应该明显小于1。NMSE的值越小，模型的性能就越好。NMSE的值大于1，意味着
#             模型预测还不如间的的把所有个案的平均值作为预测值。

# *快速方法
# 在本书提供的R添加包中，函数regr.eval()用来计算线性回归模型的性能度量指标。
regr.eval(algae[ , "a1"] , rt.prediction.a1 , train.y = algae[ , "a1"])

# *可视化分析
#可视化地产看模型的预测值更加有趣，一种方式是绘制散点图。
old.par <-par(mfrow = c(1 , 2))
plot(lm.prediction.a1 , algae[ , 'a1'] , main="Linear Model" , xlab="predictions" , ylab="True Values")
abline(0 , 1 , lty=2)
plot(rt.prediction.a1 , algae[ , 'a1'] , main="Regression Tree" , xlab="predictions" , ylab="True Values")
abline(0 , 1 , lty=2)
par(old.par)
# *评价模型