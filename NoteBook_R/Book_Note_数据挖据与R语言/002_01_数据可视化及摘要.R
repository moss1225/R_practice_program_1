# 1、加载数据
library(DMwR)

# 2、查看数据集前6行数据
head(algae)

# 3、读取Analysis.txt数据
# 用if的方式进行多行注释
if(F){ 
algae <- read.table('Analysis.txt' , 
	head=F ,
	dec='.' ,
	col.names=c('season' , 'size' , 'speed' ,  'mxPH' , 'mnO2' , 'C1' ,
	'NO3' , 'NH4' , 'oP04' , 'Ch1a' , 'a1' , 'a2' , 'a3' , 'a4' ,
	'a5' , 'a6' , 'a7'),
	na.strings=c('XXXXXXX'))
}# if end

# 4、数据可视化和摘要
# 获取数据统计特性的一个方法是获取数据的如下描述性统计摘要。
summary( algae )

# 5、绘制直方图
hist( algae$mxPH , prob = T)

# 6、通过柱状图、箱图进行分析
library( car )
par( afrow = c(1 , 2) )
hist( algae$mxPH , prob = T , xlab="" ,
		main = "Histogram of maximum pH value" , ylim = 0:1 )
lines(density(algae$mxPH , na.rm=T))
# 设置参数"na.rm=T" ，是说明在函数的疾患中不考虑NA值
rug(jitter(algae$mxPH))
# 轴须图，在轴旁边出现一些小线段，jitter是加噪函数
# rug()增加绘图下方的图像描述
# 重点！！！
# jetter()对要绘制的原始值略微进行随机排列，这就避免了两个值相等的可能性
qq.plot(algae$mxPH , main = 'Normal QQ plot of maximum pH')
par( mfrow=c(1 ,1))
boxplot( algae$oPO4 , ylab = "Orthophosphate(oPO4)")
# 绘制箱图
rug(jitter(algae$oPO4) , side =2)
abline(h= mean(algae$oPO4 , na.rm =T ) , lty = 2)
# 在均值部分画一条线

# 7、研究数据突出的离散值
plot(algae$NH4 , xlab = "")
abline(h = mean(algae$NH4 , na.rm=T) , lty =1 )
abline(h = mean(algae$NH4 , na.rm=T) + sd(algae$NH4 , na.rm=T) , lty = 2)
abline(h = median(algae$NH4 , na.rm=T) , lty =3)
identify(algae$NH4)

# 8、绘制条件箱图
library(lattice)
bwplot(size ~ a1 , data = algae , ylab = 'River Size' , xlab = 'Algal A1')

# 9、分位箱图
library(Hmisc)
bwplot(size ~ a1 , data = algae , panel = panel.bpplot ,
		probs=seq(.01 , .49 , by = .01) , datadensty=TRUE ,
		ylab='River Size' , xlab='Algal A1')

# 10、连续数值离散化的分位箱图
minO2 <- equal.count(na.omit(algae$mnO2) ,
		number = 4 , overlap = 1/5)
# equal.count()对连续变量mnO2离散化，把怪变量转换为因子类型。
# number()设置区间个数 ，overlap()设置两个区间之间的靠近边界的重合。
# 意味着某些观测值将被分配到相邻的区间中，每个区间的观测值的个数相等。
# na.omit()可以用来剔除向量中任意NA值
stripplot(season~a3|minO2 , 
		data=algae[!is.na(algae$mnO2) , ])
# 调用stripplot()函数，该函数是lattiece包中的一个绘图函数，它根据另一个变量（这里是season）
# 把变量的实际值绘制到不同的图形中。