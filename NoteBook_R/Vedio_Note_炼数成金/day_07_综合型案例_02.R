# 案例：多分类问题的距离判别
#         对于距离判别，很容易将两分类判别方法推广到多分类问题，事实上，距离
# 判别的本质就是计算Mahalanobis距离，待测样本距哪个总体距离近，就认为
# 它属于哪一类。
distinguish.distance <- function(TrnX , TrnG , TstX=NULL , var.equal = FALSE){
	# TrnX表示数据，TrnG表示分类
	if(is.factor(TrnG)==FALSE){
		# is.factor(TrnG) 检测TrnG是不是因子，即包含的是离散值，不包含连续值。
		mx<-nrow(TrnX) 
		mg<-nrow(TrnG)
		TrnX <-rbind(TrnX , TrnG)
		TrnG <-factor(rep(1:2) , c(mx , mg) )
		# 方式兼容day_07_综合型案例_01.R 程序
	}
	if(is.null(TstX)==TRUE) TstX <- TrnX
	# 如果测试集为空，把训练集当作测试集
	if(is.vector(TstX)==TRUE) TstX<-t(as.matrix(TstX))
	else if( is.matrix(TstX) != TRUE) TstX<-as.martrix(TstX)
	if(is.matrix(TrnX)!=TRUE) TrnX <- as.matrix(TrnX)
	
	nx <- nrow(TstX)
	# 获取测试集的行数，即有多少条需要测试的数据。
	belong  <- matrix(rep(0 , nx), nrow=1 , dimnames=list("blong" , 1:nx ))
	# belong 存储分类结果。
	g <- length(levels(TrnG))
	# g记录一共有多少种分类，levels返回因子个数。
	mu <- matrix(0 , nrow=g , ncol=ncol(TrnX))
	# 生成一个列平均值的矩阵
	for(i in 1:g){ # i 表示循环值，部分位置也表示种类的分类，例如：分类1 ，分类2。
		mu[i , ] <- colMeans(TrnX[TrnG==i , ])
		# 求每一列的均值，具体是根据类别进行分类，第一行第一列表明，分类一下属性一的平均值。第二行第一列表明，分类二下属性一的平均值
	}
	D <- matrix(0 , nrow=g ,ncol=nx )
	# 建造储存计算数值结果的矩阵，行之间表示种类，列之间表示测试集的数据，第一行第一列表示样品1和种类1的马氏距离。
	if(val.equal==TRUE || var.equal == T){  # 协方差相同
		for(i in 1:g){
			D[i , ] <- mahalanobis(TstX , mu[ i , ] , var(TrnX))
		}
	}
	else{   # 协方差不同
		for(i in 1:g){
			D[ i , ] <- mahalanobis(TstX , mu[i, ] , var(TrnX[TrnG==i] , ))
		}
	}
	for(j in 1:nx){
		dmin <- Inf  # Inf: 正无穷大
		for(i in 1:g){
			if(D[i , j] < dmin){
				dmin<-D[i , j]
				blong[j] <- i
			}
		}
	}
	blong
	# 输出检测结果
}
