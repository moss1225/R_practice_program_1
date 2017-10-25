# 我们需要预测变量的一个幂次矩阵。当时我们使用的是下面的代码：
# forms matrix of powers of vector x , through degree dg
powers <- function(x , dg){
	pw <- matrix(x , nrow = length(x))
	prod <- x # current product
	for(i in 2 : dg){
		prod <- prod*x
		pw <- cbind(pw , prod)
	}
	return(pw)
}

#         一个很明显的问题是，我们使用cbind()来住劣生成最终的矩阵，二者在内存分配上是
# 需要消耗时间的。更好地办法是在最开始给出完整的矩阵分配内存，即使该矩阵可能是
# 空的。其他的原因在于，这样的操作只会进行一次分配内存的操作。
# forms matrix of powers of the vector x , through degree dg
powers2 <- function(x , dg){
	pw <- matrix(nrow=length(x) , ncol=dg) # 一次性给出完成内存空间
	prod <- x
	pw[ , 1] <- prod
	for(i in 2:dg){
		prod <- prod*x
		pw[ , i] <- prod # 向量的方式进行操作
	}
	return(pw)
} # 此函数效果较好

# 通过outer()函数去除循环
powers3 <- function(x , dg){return(outer(x , 1:dg , "^"))}
# 结果效果不理想

# 最崩溃的修改方式
powers4 <- function(x , dg){
	repx <- matrix(rep(x , dg) , nrow=length(x))
	return(t(apply(repx , 1 , cumprod))) # 此处的apply操作，扩大了拷贝量。
}