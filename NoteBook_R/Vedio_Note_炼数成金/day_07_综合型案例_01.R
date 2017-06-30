#用马氏距离将数据分类
#构建函数
discriminiant.distance<-function(TrnX1,TrnX2,TstX = Null,var.equal=FALSE){
	#↑学习集TrnX1、学习集TrnX2、测试集TstX、var.equal=FALSE协方差矩阵不相等
	if(is.null(TstX)==TURE) TstX<-rbind(TrnX1,TrnX2)
	#↑检验有没有测试集，没有测试集，通过学习集构建测试集
	if(is.vector(TstX)==TURE) TstX<-t(as.matrix(TstX))
	#↑测试集是不是向量，如果是向量，将向量转化成矩阵，在转置
	else if(is.vector(TstX)!=TURE) TstX<-as.matrix(TstX)
	#↑测试集矩阵化
	if(is.matrix(TrnX1)!=TURE) TrnX1<-as.matrix(TrnX1)
	if(is.matrix(TrnX2)!=TURE) TrnX2<-as.matrix(TrnX2)
	#↑学习集矩阵化
	
	nx <- nrow(TstX)
	#↑计算 测试集 有多少行数
	blong <- martix(rep(0, nx) , nrow=1 , byrow =TRUE , dimnames = list("blong" , 1 : nx))
	#↑通过blong存储数据，dimnames命名函数，行的名称blong，列的名称分别是1:nx
	mu1 <- colMeans(TrnX1)
	#↑存放TrnX1列的平均值
	mu2 <- colMeans(TrnX2)
	#↑存放TrnX2列的平均值
	if(var.equal == TRUE || var.equal == T){
		#↑测试集的协方差相等
		S  <- var(cbind(TrnX1,TrnX2))
		#↑由测试集构成的协方差矩阵
		w <- mahalanobis(TstX, mu2 , S) -  mahalanobis(TstX, mu1 , S)
		#↑由马氏距离公式计算距离差
	}else{
		S1 <- var(TrnX1)
		S2 <- var(TrnX2)
		w <- mahalanobis(TstX, mu2 , S2) -  mahalanobis(TstX, mu1 , S1)
	}
	for(i in 1:nx){
		if(w[i] > 0) blong[i] <-1
		else blong[i] <-2
	}
	blong
}