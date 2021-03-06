#考虑x和y，它们是时间序列，比如说他们是每小时收集的气温和气压测量值。
#我们定义两者的相关性为x和y同时上升或下降次数占总观测数的比例，即y[i+1]-y[i]与x[i+1]-x[i]符号相同时的次数占总数i的比例。
#时间序列概念：（时间序列是指将同一统计指标的数值按其发生的时间先后顺序排列而成的数列。时间序列分析的主要目的是根据已有的历史数据对未来进行预测。）

findud<-function(v){
	vud= v[-1]-v[-length(v)]
	#↑v[-1]表示v除去第一个值得到的数列，v[-length(v)]表示v除去第最后一个值得到的数列
	#↑vud =c(v[2]-v[1],v[3]-v[2],...,v[n]-v[n-1])
	return(ifelse(vud>0,1,-1))
	#↑1表示数据上升，-1表示数据下降
}

udcorr<-function(x,y){
	ud<-lapply(list(x,y),findud)
	#↑分别将x,y带入findud函数，分别返回到ud[[1]]和ud[[2]]中
	return(mean(ud[[1]]==ud[[2]]))
}
udcorrLESS<-function(x,y){
	#↑精简udcorr函数
	mean(sign(diff(x))==sign(diff(y)))
}
x=c(5,12,13,3,6,0,1,15,16,8,88)
y=c(4,2,3,23,6,10,11,12,6,3,2)

udcorr(x,y)