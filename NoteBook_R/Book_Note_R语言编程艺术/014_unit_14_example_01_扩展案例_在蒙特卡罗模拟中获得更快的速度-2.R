# 相对较为复杂的案例
#         下一个例子是初等概率课程中的一个经典习题，刚1包含10颗蓝色大理石和8颗黄色
# 大理石。缸2是6颗蓝色的和6颗黄色的。我们从缸1中随机取出一颗石头，然后转移到缸2
# 中，然后再从缸2中随机找出一个石头。那么，第二颗石头是蓝色的概率是多少？这个问题
# 很容易地得出解析解，但在此我们利用模拟来完成。下面是一种直接的方法：
# performnreps reptitions of the marble experiment , to estimate
# P(pick blue from 2)
sim1 <- function(nreps){
	nb1 <- 10 # blue marbles in Urn1
	n1 <- 18 # number of marbles in Urn1 at 1st pick
	n2 <- 13 # number of marbles in Urn2 at 2st pick
	count <- 0 # number of repetitions in which get blue from Urn2
	for(i in 1:nreps){
		nb2 <- 6 # 6 blue marbles orig.in Urn2
		# pick from Urn 1 and put in Urn 2 ; is it blue?
		if ( runif(1) < nb1/n1) nb2 <- nb2 + 1 # 从1包中取出的是否是蓝球
		# pick form Urn2 ; is it blue
		if ( runif(1) < nb2/n2) count <- count + 1# 从2包中取出的是否是蓝球
	}
	return(count/nreps) # est.P(pick blue from Urn2)
}

# 改写程序，以下我们用apply()来避免循环：
sim2 <-function(nreps){
	nb1=10
	nb2=6
	n1=18
	n2=13
	# pre-generate all our random numbers, one row per repetition
	u <- matrix(c(runif( 2*nreps )) , nrow=nreps , ncol=2)
	# define simfun for use in apply() ; similates on repetition
	simfun <- function(rw){
		# rw("row")is a pair of random numbers
		# choose from Urn 1
		if(rw[1] < nb1/n1) nb2 <- nb2 + 1
		# choose from Urn 2 , and return boolean on choosing blue
		return (rw[2] < nb2/n2) # 返回的是布尔类型
	}
	z <- apply(u , 1 , simfun)
	# z is a vector of booleans but they can be treated as 1s , 0s
	return(mean(z))
}
# 上述改写程序说明：
#         在此，我们创建了一个两列的矩阵u，其中的元素是U(0 , 1)随机变量。矩阵的第一列用于
# 模拟从缸1中取出石头，第二列用于模拟缸2.按照这种方式，我们这种方式，我们将所有的随机数一次性生
# 成，这可以节省一些时间，但主要的改进在于apply()的使用。朝着这个目标，函数simdun()
# 应用于实验的每次重复中，即应用于u的每一行。我们将这个函数传递给apply()，将其应用
# 与所有的nreps次重复中。
#          需要注意的是，由于函数simfun()是在sim2()内部定义的，所以sin2()的局部变量
# n1、n2、nb1和nb2，在simfun()中相当于全局变量。此外，由于布尔向量在R中会被自动
# 处变成1和0，我们可以通过简单地调用mean()来计算向量中TRUE的比例。

# 两种方法性能测试
syste.time(print(sim1(100000)))
# 0.5086 # 运算结果
# 运行时长 2.465
syste.time(print(sim2(100000)))
# 0.5031 # 运算结果
# 运行时长 2.936

#         尽管函数式编程有许多优点，但在这里，使用apply()并没有发挥作用；相反，事情变得
# 更糟了。由于这可能是因为随便抽取的数字会有所不同，所以我将代码重新运行了若干次，
# 然而结果都是类似的。
#         因此，我们将模拟的过程向量化。

# 改写程序，让模拟的过程向量化
sim3 <- function(nreps){
	nb1=10
	nb2=6
	n1=18
	n2=13
	u <- matrix(c(runif( 2*nreps )) , nrow=nreps , ncol=2)
	# set up the condition vector
	cndth <- ( ( u[ , 1] <= nb1/n1 ) & ( u[,2] <= ( nb2+1 )/n2 ) )| 
			( u[ , 1] > nb1/n1 ) & ( u[ ,2] <= nb2/n2 )
	return(mean(cndth))
}
#         记住<=和&都是函数；事实上，它们都是向量化函数，因此其速度应该很快。毫无疑问，
# 这段代码可以带来巨大的速度进步。
syste.time(print(sim2(100000)))
# 0.4987 # 运算结果
# 运行时长 0.060

# 总结：
#         从原则上说。上面我们采取的加速代码的方式还可以应用到许多其他的蒙特卡罗模拟
# 中。然而很显然，计算cndtn的语句可能会变得很长复杂，即使是对看上去很简单的问题。
#         此外，这种方法不能应用与"无限阶段"的情形，即在时间上有无限的步数。在此，我
# 们将取石头的例子看作是两阶段的，它对应于矩阵u的两列。