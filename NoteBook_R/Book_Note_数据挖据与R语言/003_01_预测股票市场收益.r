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
		x = x[,c("High" , "Low" , "Clode")]
		return(x)
	}
	# 计算每日平均价格
	v <- apply(HLC(quotes) , 1 , mean)
	#创建空矩阵，用于储存？
	r <- matrix(NA , ncol = n.days , nrow=NROW(quotes))
	# 设置函数Delt，函数目标：计算未来n天后相对该日的百分比收益，v平均价格，k对比天数
	Delt <- function(v , k){
		
	}
	#设置函数Next()，函数目标：按时间平移一个时间序列
	NEXT<-function(){
		
	}
	for(x in 1:n.day) r[,x]<-Next(Delt(v , k = x ) , x)
	x <- apply(r , 1 , function(x) sum(x[x> tgt.margin | x<-tgt.margin]))
	if(is.xts(quotes)) xts(x , time(quotes)) else x
}