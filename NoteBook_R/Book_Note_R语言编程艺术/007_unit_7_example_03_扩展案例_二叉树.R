#          树形数据结构在计算机科学和统计学中非常普遍的。例如，在R中，用于回归和分类
# 的递归分块方法库-repart，就非常受欢迎。很明显，树在系谱中有所应用，更一般的方法，
# 图分为了社交网络分析基础。

#         然而，R中的树结构也有些实际问题，它们中很多是由于R语言中没有指针类型的引用所造
# 成的。事实上，因为这个原因，也为了获得更好的运算性能，一个更好的选择通常是用C语言编
# 写核心代码。

# routines to create trees and insert items into them are included
# below ; a delete routine is left to the reader as an exercise.

# storage is a matrix , say m , one row per node of the tree; if row
# i contain(u , v , w) , the node i stores the value w , and has left and
# right links to rows u and v ; null links have the value NA.

# the tree is represented as a list(mat , nxt , inc) , where mat is the
# matrix , nxt is the next empty row to be used , and inc is the number of
# row of expansion to be allocated whenever the matrix becomes full

# print sorted tree via in-order  traversal

printtree <- function(hdidx , tr){
	left <- tr$mat[hdidx , 1]
	if(!is.null(left)){
		printtree(left , tr)
	}
	print (tr$mat[hdidx,3]) # print root
	right <-tr$mat[hdidx , 3]
	if(!is.null(right)){
		printtree(right , tr)
	}
}

# initializes a storage matrix , with initial stored value firstval
newtree <- function (firstval , inc) {
	m <- matrix(rep(NA , inc*3) , nrow=inc , ncol=3 )
	m[1 , 3] <- firstval
	return(list(mat=m , nxt =2 , inc = inc))
}

# insert newval into the subtree of tr , with the subtree's root being
# at index hdidx ; note that return value must be reassigned to tr by the
# caller (including ins() itself , due to recursion)
ins <- function(hdidx , tr , newval){
	# which direction will this new node go , left or right?
	dir <- if(new<=tr$mat[hdidx,3]){1}else(2)
	# if null link in that direction , place the new node here , otherwise recure
	if(is.na(tr$mat[hdidx , dir])){
		newdix <- tr$nxt # where new node goes
		# check for room to add a new element
		if(tr$nxt == nrow(tr$mat) + 1){
			tr$mat <- rbind(tr$mat , martrix(rep(NA , tr$inc*3) , nrow=trinc , ncol=3))
		}
		# insert new tree node
		tr$mat[newidx , 3]<-newval
		# link to the new node
		tr$mat[hdidx , dir]<-newdix
		tr$mat<-tr$mat+1 # ready for next insert
		return(tr)
	}else tr <- ins(tr$mat[hdidx , dir] , tr , newval)
}
