# 本节描述了如何通过建立回归树来预测海藻a1出现的频率。
# 由于这类模型多能够处理缺失值，所以这里只需要如前面所述移除62号和192号数据
# 建立回归树模型代码如下：
# 引入rpart包，引入回归模型
library(rpart)
library(DMwR)
data(algae)
algae <- algae[-manyNAs(algae) , ]
rt.a1 <- rpart( a1 ~ . , data = algae[ , 1:12])
first.tree <- rt.a1
prettyTree(rt.a1)
# 使用rpart()函数构建书，在构建树的过程中，当给定条件满足时构建过程就停止。
# 1）偏差的减少小于摸一个给定限制时，称mp；
# 2）当结点中的样本数量小于某个给定界限时，称minisplit；
# 3）当树的深度大于一个给定界限值，称maxdepth。
first.tree <- snip.rpart(first.tree )
prettyTree(first.tree)