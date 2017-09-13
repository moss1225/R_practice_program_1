# 假设我们要创建5个文件，从q1.pdf到q5.pdf，依次为服从正态分布N(0,t^2)的100随机变量
# 的直方图。执行下面代码：
# 方法一
for(i in 1:5){
	fname <- paste("q" , i , ".pdf" , sep="")
	# frame <-  sprintf("q%d.pdf" , i)
	pdf( fname )
	hist(rnorm(100 , sd = i))
	dev.off()
}