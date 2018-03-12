# 多元线性回归
# 获取数据，消除缺失值过多的数据
library("DMwR")
# 载入包
data(algae)
# 载入数据
algae<- algae[-manyNAs(algae) , ]
# 消除缺失值较多的数据
clean.algae <- knnImputation(algae , k = 10)
# 对缺失值进行预测填补，采用DMwr包中的knnImputation函数，方法时，找到与数据差异最小的10条数据行，
# 10条数据对应原数据缺失值部分平均数，进行数据补充。
# 在运行上面的代码后，得到的数据框clean.algae将不含有缺失值
# 接下来，将建立一个由于预测海藻的线性回归模型：
lm.a1 <- lm(a1 ~ . , data = clean.algae[ , 1:12])
# 查看多元线性模型信息
summary(lm.a1)
# 名称注释：
# Intercept：残差
# Estimate：估计值
# std.error：标准误差
# t value：t值
# Pr(>|t|): p值，越小于好，用于检测任何解释变量与目标变量没有依赖关系这一原假设，p为0.0001表示有99.99%的置信区间却淡定原假设是错误的。
# Residual standard error：拟合优度（越小越好）
# Adjusted R-squard：修正的拟合优度（范围：0-1，越大拟合度越好）
# Multiple R-squard：拟合优度（越大越好）
# F-statistic：F检验 小于0.05，表示在P=0.05的水平上还是通过显著性检测的
# R中提供Q-Q图检测时候满足正态分布

# 目标消元法
# 首先用函数anova()来精简线性模型。
anova(lm.a1)
# 显示season字段的p值最大，故先去掉season的线性关系
# 从lm.a1中除去season 
lm2.a1 <- update(lm.a1 , . ~ . , -season)
# 查看lm2.a1中的模型信息
summary(lm2.a2) 
# 比较lm.a1和lm2.a1两个模型
anova(lm.a1 , lm2.a1)
# Sum of Sq:误差平方和-488，平方和减少-488，表示拟合度上升
# Pr(>F):0.9671表示两个模型不同的可能为30%
# R中包含一步跳过所有anova和update步骤的函数step
final.lm <- step(lm.a1)