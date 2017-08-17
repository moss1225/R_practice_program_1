# 考虑如下的概率问题
#          从20个学生中选出人数分为3、4、5的三个委员会。A和B被选入同一个委员会的概
# 率为多少？
#          这个问题不难得到解析解，不过有时会希望使用模拟来验证结果，并且不管怎么说，编
# 写代码将展示R的集合运算在涉及组合的问题时如何排上用场。

# 主函数
sim <- function(nreps){ # nreps 是模拟执行的次数 
	commdata <- list() # 创建列表，储存变量
	# will store all our info about the 3 committees
	commdata$countabsamecomm <- 0 # 创建对象，储存A和B出现在同一委员会的次数
	for(i in 1:nreps){ # 主循环
		commdata$whosleft <- 1:20 # 备选学生名单，备选20人，假设A、B同学编号为1、2
		# who`s left to chose from
		commdata$numabchosen <- 0
		# number among A , B chosen so far
		# choose committee 1 , and check for A , B serving together
		commdata <- choosecomm(commdata , 5)
		# if A or B already chosen ,no need to look at the other comms .
		if( commdata$numabchosen > 0) next # 如果A和B出现在同一委员会，直接跳到下次循环
		# chose committee 2 and check
		commdata <- choosecomm(commdata , 4)
		if( commdata$numabchosen > 0) next
		# chose committee 3 and check
		commdata <- choosecomm(commdata , 3)
	}
	print(commdata$countabsamecomm/nreps)
}

# 功能函数
choosecomm <- function(comdat , comsize){
	# choose commitee
	committee <- sample(comdat$whosleft , comsize) # 在comdat$whosleft中随机抽取comsize人，当作委员会成员
	# count how many of A and B were chosen
	comdat$numabchosen <- length(intersect(1:2 , committee)) # 鉴定编号1、2的学生为待定的学生，检测1、2是否在委员会成员的名单里，intersect()交集
	# print(committee) # 调试函数
	# print(comdat$numabchosen) # # 调试函数
	if(comdat$numabchosen == 2){ 
		comdat$countabsamecomm <- comdat$countabsamecomm + 1
	}
	# delete chosen committee from the set of people we now have to choose from
	comdat$whosleft <- setdiff(comdat$whosleft , committee)
	# print(comdat$whosleft) # 调试函数
	return (comdat)
}

# 调用主函数
sim ( 100 )