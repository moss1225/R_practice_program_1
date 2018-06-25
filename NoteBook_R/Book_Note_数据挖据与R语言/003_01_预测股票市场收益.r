#读取时间序列的包
library(xts)
# 从包中获取数据集
library(DMwR)
data(GSPC)
# 读取现有csv文件，GSPC为股票时间序列数据
setwd("C:\\Users\\Administrator\\git\\R_practice_program_1\\NoteBook_R\\Book_Note_数据挖据与R语言")
GSPCdata<-data.frame(read.csv("^GSPC.csv" , head=T))
GSPC <- as.xts(read.zoo(GSPCdata) , head=T)
# 设置函数T.ind，函数目标：
T.ind <- function(quotes , tgt.margin = 0.025 , n.days=10){
	# 设置函数HLC，函数目标：取出最高值、最低值、收盘值
	HLC <- function(x){
		x = x[,c("High" , "Low" , "Close")]
		return(x)
	}
	# 计算每日平均价格
	v <- apply(HLC(quotes) , 1 , mean)
	#创建空矩阵，用于储存？
	r <- matrix(NA , ncol = n.days , nrow=NROW(quotes))
	# 设置函数Delt，函数目标：计算未来n天后相对该日的百分比收益，v平均价格，k对比天数
	# 思路[(平均价i+j)-(收盘价i)]/(收盘价i)
	Delt <- function(v , k){
		# 计算i+k天相对i天数变动的百分比
		DATArows <- NROW(quotes)
		DeltReturnData <- matrix(NA , ncol=1 , DATArows)
		for(i in 0:(DATArows-k)){
			DeltReturnData[DATArows - i , 1]=(v[DATArows-i]-v[DATArows-i-k+1])/v[DATArows-i-k+1]
		}
		return(DeltReturnData)
	}
	# 设置函数Next()，函数目标：按时间平移一个时间序列
	Next<-function(f , x){
		# Next函数与Delt合并
	}
	# 调用Next和Delt函数，有三个参数v，k，x，将结果储存在r中
	# for(x in 1:n.days) r[,x]<-Next(Delt(v , k = x ) , x) # 修改成下行代码
	for(x in 1:n.days) r[,x]<-Delt( v , k = x )
	x <- apply(r , 1 , function(x) sum(x[x> tgt.margin | x < -tgt.margin] ))
	if(is.xts(quotes)) xts(x , time(quotes)) else x
	### 以下为测试部分
	# return (x)
}
### 以下为测试部分
#data=T.ind(GSPC, 0.025,10)