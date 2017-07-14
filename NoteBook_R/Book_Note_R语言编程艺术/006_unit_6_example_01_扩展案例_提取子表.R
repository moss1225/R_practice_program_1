# 如果一张表有很多行或很多维的化，浏览它的数据是比较困难的。一种方法是关注频
#数最大的单元格。这就是下文将给出的函数tabdom()的目的，它表示了一个表中追溯显
#著的数据。下面是一个简单的调用：
#编写数据
a<-list(Vote.for.X=c("Yes" , "Yes" , "No" , "Not Sure" , "No" ))
b<-list(Vote.for.X.Last.Time=c("Yes" , "No" , "No" , "Yes" , "No"  ))
cttab.data <- data.frame(a , b)
cattab<-table(cttab.data)
#假定我们需要在一个会议中展示数据，重点关注的是那些知道自己在本次选举中会投
#票给X的调查对象。换句话说，我们希望消除不确定(Not Sure)的条码，然后呈现类似于
#下面的子表：
#结果类似：cattab[c(1,3),]不包含“Not Sure”
#下文提出的函数subtable()可以完成子表的提取。它有两个参数：
#tbl：感兴趣的表，它是"table"类的对象。
#subnames：是一个列表，用来设定想要提取的子表。该列表的每个组件都是以tbl的某个
#维度命名，组件的值是所需数值的名称向量。

#因此，在看具体代码之前，先看一下这个例子有什么，参数cttab将是一个二维表，维度
#的名字分别是Vote.for.X和Vote.for.X.Last.Time。在这两个维度中，第一维中各水平的名称是
#No、Not Sure、Yes，在二维中各水平的名称是No和Yes。其中。我们希望剔除Not sure的案
#例，所以形成参数subnemes 的实际值为：
#调用函数格式subtable(cttab , list(Vote.for.X=c("No" , "Yes")),list(Vote.for.X.Last.Time=c("No" , "Yes")))
subtable <- function ( tbl  ,  subnames ) {
	tblarray <- unclass ( tbl ) 
	#↑函数unclass去除函数属性，此处将table格式转换成martrix
	dcargs <- list ( tblarray )
	#↑数据降维？
	ndims <- length ( subnames )
	#↑确定参数包含的数量，返回值为2
	#↑返回ndims[1]=$Vote.for.X
	#↑返回ndims[2]=$Vote.for.X.Last.Time
	for ( i in 1 : ndims  ) {
		dcargs[[ i + 1 ]] <- subnames[[ i ]]
		#↑核心语句
		#↑建立一个列表，该列表第一项dcargs[[1]]是tblarray数据
		#↑接下来是dcargs[[2]],dcargs[[3]]是用户所需的每个维度的水平
	}
	subarray <- do.call ( "[" , dcargs )
	#↑核心语句
	#↑函数do.call(函数f , 参数列表)，相当于f(x1,x2,x3....)，其中"["为内置参数
	#↑存放新数据到subarray
	#↑函数do.call()自动根据dcargs[[2]],dcargs[[3]]中的维度水平的数据，处理dcargs[[1]]
	dims <-lapply(subnames , length )
	subtbl <- array ( subarray , dims , dimnames = subnames )
	class( subtbl )
	return(subtbl)
}

subtable(cattab , list( Vote.for.X = c("No" , "Yes") ,  Vote.for.X.Last.Time = c("No" , "Yes")))