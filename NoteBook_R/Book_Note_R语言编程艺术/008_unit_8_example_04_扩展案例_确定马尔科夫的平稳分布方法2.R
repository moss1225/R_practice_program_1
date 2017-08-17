#       另一种方法是建立在特征值基础上，要是用更高级的知识。从等式8.4可以看出x是矩
#阵P的做特征向量，对应的特征值是1.受此启发，可以使用R中eigen()函数，选择与特征
#值1相对应的特征向量。（可以使用数学里的Perron-Frobenius定理来证明这个结论。）

#        因为R是作特征向量，在函数eigen()中使用的参数必须是P的转置而不能是P。除此之
#外，由于一个特征向量与非零标量相乘后依然跟原来的特征向量相等，针对eigen()返回特
#征向量有两个问题需要处理：
# *它的分量可能为负，如果有的化，把它乘以-1.
# *它可能不满足等式8.6，补救方法是把它除以向量的长度

findpi2 <- function(p){
	n <- nrow(p)
	# find first eigenvetor of P transpose
	pivec <- eigen(t(p))$vectors[,1]
	# guarateed to be real , but could be negative
	if(pivec[1]<0) pivec<- -pivec
	# normalize to sum to 1
	pivec <- pivec/sum(pivec)
	return(pivec)
}
#        函数eigen()返回值是一个列表。该列表的一个分量是名为vectors的矩阵，包含的都是
#特征向量，该矩阵的第i列是对应于第i个特征向量，所以取第一列。