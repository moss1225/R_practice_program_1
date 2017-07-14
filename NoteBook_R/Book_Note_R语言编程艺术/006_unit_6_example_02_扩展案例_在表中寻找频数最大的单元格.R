#        如果有一张表有很多行或很多维的话，浏览它的数据是比较困难的。一种方法是关注频
#数最大的单元格。这就是下文将给出的函数tabdom()的目的，它表示一个表中最显著的频数。
#下面是一个简单的调用：
# tabdom( tbl ， k) 函数会给出tbl中频数占前k位的单元格。
#下面是一个例子：
# 构建数据
a<-list(Vote.for.X=c("Yes" , "Yes" , "No" , "Not Sure" , "No" ))
b<-list(Vote.for.X.Last.Time=c("Yes" , "No" , "No" , "Yes" , "No"  ))
cttab.data <- data.frame(a , b)
cattab<-table(cttab.data)

# 编写函数
tabdom <-function(tbl , k){
	tbldf <- as.data.frame( tbl )
	#↑将表转换成数据框，自动体检频路的一项
	freqord <- order(tbldf$Freq , decreasing =TRUE)
	#↑进行倒序排列
	dom <- tbldf[freqord , ][1:k , ]
	return ( dom )
}

# 调用函数
tabdom(cattab , 2)
#↑调用函数tabdom，对cattab数据，找到并返回频路最高的两个数据