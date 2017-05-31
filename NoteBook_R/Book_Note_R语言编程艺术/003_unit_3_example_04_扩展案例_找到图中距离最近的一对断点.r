#假设有一个距离矩阵，其第i行第j列的元素代表城市i和城市j间的距离。
#我们需要写下函数，输入城市距离矩阵，输出城市间最短的距离，
#以及对应的两个城市，代码如下：

# returns the minimum value of d[i , j] , i != j , and the row/col attaining
# that minimum, for square symmetric d, no special policy on ties
mind<-function(d){
	n<-nrow(d)
	dd<-cbind(d,1:n)
# add a column to identify row number for apply()
	wmins<-apply(dd[-n, ],1,imin)
# apply data dd by row in function imin() 
	i <-which.min(wmins[2, ])
	j<-wmins[1 ,i]
	return(c(d[i,j], i, j))
}

imin <- function(x){
	lx <- length(x)
	i <-x[lx] 
# original row number
	j<-which.min(x[i+1:(lx-1)])
	k<- i+j
	return(c(k,x[k]))
}

dataQ=matrix(c(0,12,13,8,20,12,0,15,28,88,13,15,0,6,9,8,28,6,0,33,20,88,9,33,0),nrow=5,byrow=T)
resultQ=mind(dataQ)
print( paste("最小距离",resultQ[1] , sep = "",collapse=""))