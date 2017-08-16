# 假设我们希望跟踪向量的写操作。换句话说，在执行以下语句时：
# x[2]<-8
# 我们不仅希望把x[2]改为8，也希望增加一个计数器记录x[2]被写入的次数。为了做到这
# 点，我们可以给向量下标操作编写一个针对特定类的泛型置换函数。

# 这段代码用到了类，目前，只需知道在创建S3类的时候创建了一个列表，并通过class()函
# 数给这个列表赋予了类的属性。

# class "bookvec" of vetor that count writes of their element.

# each instance of the class consist of a list whose components are the
# vector values and a vetor of counts

# construct a new object of class bookvec

newbookvek <- function(x){
	tmp <- list()
	tmp$vec <- x # the vector itself
	tmp$wrts <- rep(0 , length(x)) # counts of the writes , one for each element
	class(tmp) <- "bookvec"
	return(tmp)
}

# function to read
"[.bookvec" <-function(bv , subs){
	return (bv$vec[subs])
}

#function to write
"[<-.bookvec" <-function(bv , subs , value){
	bv$wrts[subs] <- bv$wrts[subs] + 1 # note the recycling
	bv$vec[subs]<-value
	return(bv)
}
# 上述代码说明：
#         我们把这个类命名为“bookvec”，因为这些向量可以记录自己的写操作次数。于是，下
# 表操作函数包括了[.bookvec和[<-.bookvec。
#         函数newbookvec()（代码第16行）的功能是构建类。在函数代码里，你可以看到类的构
# 造：一个包含了向量本身的对象vec，和一个记录操作的向量wrts。
# 顺便提一下，第20行中函数class()本身就是一个置换函数！
# 函数[<-.bookvec和[<-.bookvec本身相当简单。请记住要在后者返回的值。

# 测试上述代码
# 调用newbookvek，创建数据
b <- newbookvek(c(3 , 4 , 5 , 5 ,12 , 13))
# 显示b中数据
b
# 显示b的类
attr(b,"class")
# b[2]的数值
b[2]
# b[2]赋值
b[2] <- 88
# 检验 赋值是否成功
b[2]
# 输出编写次数
b$wrts
