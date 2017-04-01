# TODO: Add comment
# 模拟产生统计专业同学的名单（学号区分），
#记录数学分析，线性代数，概率论三科成绩，
#然后进行一些统计分析。
# Author: Moss
###############################################################################
num<-seq(10378001,10378100)
			#↑等比数列构建数组
#  print(num)
			#↑输出num
x1=round(runif(100,min=80,max=100))
			#↑创建x1，round()四舍五入取整，runif(个数，最小值，最大值)随机分配
x2=round(rnorm(100,mean=80,sd=7))
			#↑创建x2，rnorm(个数，平均值，标准差)正态分布
x3=round(rnorm(100,mean=83,sd=18))
			#↑创建x3，rnorm(个数，平均值，标准差)正态分布
x3[which(x3>100)]=100
			#↑将x3中大于100的数赋值为100
x=data.frame(num,x1,x2,x3)
			#↑将数据合并成数据框
write.table(x,file="C:\\Users\\Administrator\\Desktop\\a.txt",col.names=F,row.names=T,sep=" ")
			#↑讲数据写入a.txt，
mean(x)
			#↑求平均值
colMeans(x)
			#↑按列求平均值
colMeans(x)[c("x1","x2","x3")]
			#↑"x1","x2","x3"列求平均值
apply(x,2,mean)
			#↑按列求平均值,apply(对象,1行2列,方法)
apply(x,2,max)
			#↑按列求最大值
apply(x[c("x1","x2","x3")],2,sum)
			#↑按列求和
which.max(apply(x[c("x1","x2","x3")],2,sum))
			#↑求出总分最大同学分的编号
x$num[which.max(apply(x[c("x1","x2","x3")],2,sum))]
			#↑求出总分最大同学分的学号