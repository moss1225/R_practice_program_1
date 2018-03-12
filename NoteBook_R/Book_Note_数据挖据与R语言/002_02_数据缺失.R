# 当我们处理含有缺失的数据时，可以运用以下几种常见的策略：
# 1、将含有缺失值的案例剔除
# 2、根据变量之间的相关关系填补缺失值
# 3、根据案例之间的相似性填补缺失值
# 4、使用能够处理缺失值数据的工具

# 读取数据
library(DMwR)
data(algae)

# 将缺失部分剔除
algae[!complete.cases(algae) , ]
# complete.cases()产生一个布尔向量，该向量的元素个数与algae数据框中的行数相
# 同，如果数据框的对应行中不含NA值（即为一个完整的观测值），函数返回值就是TRUE。前
# 面提到过"!"运算符，它是取逻辑否，因此上述指令显示了含有缺失值的水样记录。

# 去除某行数据
algae <- algae[-c(62 , 199) , ]

# apply()函数 ，应用apply函数，计算对象x中NA的数量。在R中逻辑值TRUE数值为1，逻辑值
# FLASE的数值为0
apply(algae , 1 , function(x) sum(is.na( x )))

# 函数manyNAs()的功能是找出缺失值个数大于列数20%的行。在第二个参数中可以设置一个精确
# 的列数作为界限。
algae <- algae[ -manyNAs(algae) ]

# 用最高频率值来填补缺失值
# 用平均数、众数、中位数填缺失值
# 举例，使用平均数填写缺失值
algae[ 48 , "mxPH" ] <-mean(algae$mxPH , na.rm = T)
# 举例，使用中位数填写缺失值
algae[is.na( algae$Chla) , "Chla"] <- median( algae$Chla , na.rm = T)

# 通过变量相关关系来填补缺失值
cor(algae[ , 4:18] , use = "complete.obs")
# 函数cor()的功能是产生变量之间的相关值矩阵（因为钱3个变量是名义变量，所以计算相关值时不考虑他们）
# 使用参数 use= "complete.obs"时，R在计算相关值时忽略含有NA的记录。相关值是在1与-1周围表示两个变量之间有强线性关系。

# 对于P04列数据确实问题，用线性关系进行拟合，P04=42.897 + 1.293×oP
data(algeae)
algae <- algae[-manyNAs(algae) , ]
fillP04 <- function(op){
	if(is.na(oP)){
		return(NA)
	}else{
		return(42.897 + 1.293*oP )
	}
}
algae[is.na(algae$P04) , "P04"] <- sapply(algae[is.na(algae$P04) , "op04"] , fillP04)
# 函数sapply()的一个参数是一个向量，第二个参数为一个函数，第二个元素函数分次应用到一个元素向量中每一个参数。

# 绘制按季节分类的关于mxPH的直方图
library(lattice)
histogram( ~mxPH | season , data = algae )

# 修改排列顺序
algae$season <- factor(algae$season , levels=c("spring" , "summer" , "autumn" , "winter"))

# 复合型分类直方图
histogram( ~mxPH | size * speed , data = algae )
stripplot(size ~ mxPH | speed ,daya =algae ,  jitter = T)

# 通过探索案例之间的相似性来填补缺失值
data(algae)
algae <- algae[-manyNAs(algae) , ]

# 通过探索案例之间的相似性来填补缺失值
data(algae)
algae <-algae[-manyNAs(algae) , ]
# 添加包中的函数knnImputation()函数，这个函数用一个欧式距离的变量来找到距任何个案最近的k个邻居
algae<- knnImputation(algae , k =10)