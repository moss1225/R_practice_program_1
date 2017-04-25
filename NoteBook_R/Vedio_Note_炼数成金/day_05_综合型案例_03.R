#6.19 P390
#         为研究高压电线对牲畜的影响，R.Norell研究晓得电流对农场动物的影
#响。他在试验中，选择了7头，6种电击强度，0,1,2,3,4,5毫安。每头牛被电
#击30下，每种强度5下，按随机的次序进行。然后重复整个实验，每头牛总共
#被电击60下。对每次电击，响应变量—嘴巴运动，或者出现，或者未出现。表
#6.13中的数据给出每种电击强度70次试验中相应的总次数。试分析电击对牛
#的影响。

#用数据框形式输入数据，在构造矩阵。
norell<-data.frame(
		x=0:5,
			#↑电流强度
		n=rep(70,6),
			#↑实验次数和实验分组数
		success=c(0,9,21,47,60,63)
			#↑成功次数
		)
	#↑构造数据框

norell$Ymat<-cbind(norell$success, norell$n-norell$success)
	#↑ 创建矩阵 第一列为norell$success为电击有反应的次数 
	#↑ 第二列为norell$n-norell$success为电击没反应的次数 
	#↑ 函数cbind()那列创建矩阵

glm.sol<-glm(Ymat~x,family=binomial,data=norell)
	#↑创建广义线性模型，family=binomia，选择公式类型

summary(glm.sol)
	#↑创建广义线性模型信息
	#↑ 得到B0=-3.3010,B1=1.2459
	#↑P=[exp(B0+B1*X)]/[1+exp(B0+B1*X)]
