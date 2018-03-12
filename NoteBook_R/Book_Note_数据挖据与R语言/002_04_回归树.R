# 本节描述了如何通过建立回归树来预测海藻a1出现的频率。
# 由于这类模型多能够处理缺失值，所以这里只需要如前面所述移除62号和192号数据
# 建立回归树模型代码如下：
# 引入rpart包，引入回归模型
library(rpart)
data(algae)
algae <- algae[-mangyNAs(algae) , ]
rt.a1 <- rpart( a1 ~ . , data = algae[ , 1:12])
