# 矩阵如下，只储存非零部分，从而节省储存空间。
#  1  5 12
#  0  6  9
#  0  0  2
#         这个类的mat组件用于储存矩阵。如前文所诉，为了节省存储空间，此处只储存了对角线
# 及其以上的元素，并且是按列的顺序。例如，矩阵的存储方法是向量(1,5,6,12,9,2),
# mat的值就是这个向量。
#         此类还包含组件ix，用来显示在mat中分的每一列的起点。在上例中，ix是c(1,2,4),这意味
# 着第一列开始于mat[1]，第二列开始于mat[2]，第三列开始于mat[4]，这有利于随时访问矩
# 阵中特定的元素和列。

# class "ut" , compact storage of upper-triangular matrices

# utility function , returns 1+...+i
sum1toi <- function(i){return(i*(i+1)/2)}

# create an object of class "ut" from the full matrix inmat (Os included)
ut <- function(inmat){
	n <- nrow(inmat)
	rtrn <-list() # start to build the object
	class(rtrn) <- "ut"
	rtrn$mat <- vector(length=sum1toi(n))
	rtrn$ix <- sum1toi(0:(n-1))+1
	for(i in 1:n){
		# store column i
		 ixi <- rtrn$ix[i]
		 rtrn$mat[ixi:(ixi+i-1)]<-inmat[1:i , i]
	}
	# rtrn$mat <- inmat[row(inmat) <= col(inmat)]
	# 用一行代码，代替上述for循环，此外默认输出是按列。
	return(rtrn)
}

# uncompress utmat to a full matrix
expandut <- function(utmat){
	n <- length(utmat$ix) # numbers of rows and cols of martrix
	fullmat <- matrix( nrow = n , ncol = n)
	for(j in 1:n ){
		# fill jth column
		start <- utmat$ix[j]
		fin <- start + j -1
		abovediagj <- utmat$mat[start:fin] # above-diag part of col j
		fullmat[ , j] <- c(abovediagj , rep(0 , n-j))
	}
	return(fullmat)
}

# print matrix
print.ut <- function(utmat){
	print(expandut(utmat))
}

# multiply one ut matrix by another , returning another ut instance;
# implement as a binary operation
"%mut%" <- function(utmat1 , utmat2){
	n <- length(utmat1$ix) # number of rows and cols of matrix
	utprod <- ut(matrix(0 , nrow=n , ncol = n))
	for (i in 1:n){ # compute col i of product 
		# let a[ j ] and bj denote column j of utmat1 and utmat2 , respectively ,
		# so that , e.g. b2[1] means element 1 of column 2 of utmat2
		# then column i of product is equal to
		# bi[1]*a[1]+...+bi[i]*a[i]
		# find index of start of column i in utmat2
		startbi <- utmat2$ix[i]
		# initialize vector that will become bi[1]*a[1]+...+bi[i]*a[i]
		prodcoli <- rep(0 , i)
		for( j in 1:i){ # find bi[j]*a[j] , add to prodcoli
			startaj <- utmat1$ix[j]
			bielement <-utmat2$mat[startaj + j - 1]
			prodcoli[1: j]<- prodcoli[1:j] + bielement *utmat1$mat[startaj:(startaj+j-1)]
		}
		# now need to tack on the lower Os
		startprodcoli <- sum1toi[i-1]+1
		utprod$mat[(startbi+i-j)] <- prodcoli
	}
	return(utprod)
}