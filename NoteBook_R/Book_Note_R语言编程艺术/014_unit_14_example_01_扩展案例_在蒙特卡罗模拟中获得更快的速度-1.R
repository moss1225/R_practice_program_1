#         在一些应用中，进行统计模拟的代码可能需要运行几个小时、几天、甚至几个月，因此
# 提升的方法是非常重要的。因此，我们将考察一下两个模拟的例子。
# 		  首先，我们先考虑下述来自于第8.6节的代码：
sum <- 0
nreps <- 100000
for(i in 1: nreps){
	xy <- rnorm(2) # generat 2 N(0,1)s
	sum <- sum + max(xy)
}
print(sum/nreps)
# 程序说明：
# 求解E[max(xy)]，即服从标准正态分布N(0,1)的两个相互独立随机，
# 变量X和Y的最大值的期望值，然后求这些最大值就得到待估的期望值。
# 简而言之，求出两个独立正态分布随机数中最大值的期望。

# 下面是修改后的代码
nreps <- 100000
xymat <- matrix( rnorm(2 * nreps) , ncol = 2) 
# 求出2*nreps个随机正态分布变量
# 分成两列醉成向量，每一行包含两个随机变量，默认为一组测试
maxs <- pmax(xymat[,1],xymat[,2])
# 求出一组数据中的最大值，pmax()为内置函数。
print(mean(maxs))

# 分别将修改前后的代码，分别存在MaxNorm1.R和MaxNorm2.R中。
# 通过system.time("MaxNorm1.R")和system.time("MaxNorm2.R")的方式检验运行速度
# 我们获得更快速度，但是消耗了更大内存。