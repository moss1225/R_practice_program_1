# 编写功能函数#################################################################
#
# 读取数据函数
readdata<-function(){
	data1<-read.csv("C:\\Documents and Settings\\Administrator\\workspace\\data_analysis\\data.csv",head=TRUE)
	data1=data.frame(data1)
	return(data1)
}

#画图函数
plotP <-function(){
	plot(dataP[,3],dataP[,2])
	lines(dataP[,3],resultP[1]$coefficients[1]+resultP[1]$coefficients[2]*dataP[,3])
	print("画图完成")
}

# 分数预测选项
predictP <- function(changeX){
	if(length(scoreP)>0){	
		changeX <- scoreP
		predictDataTime <<- predict(resultP , data.frame(x = changeX))
		return(predictDataTime)
	}else{
		print("预测值为空")
	}
}

# 数据输出
writeDataToCSV <- function(dataprint){
	write.table(dataprint,file="C:\\Documents and Settings\\Administrator\\桌面\\mark.csv",col.names=F,row.names=F,sep=",")
}

# 编写主函数#################################################################
mainFunction <-function(){
	i=1 # 循环初始值
	while(i <= 30){
		dataP<<-readdata()
		x <- dataP[,2]
		y <- dataP[,i+2]
		resultP <<- lm(y ~ x)
		predictDataTime<-predictP(changeX) # 调用预测数据predictP(分数)函数，返回预测选项。
		t=1 # 循环初始值
		q5=quantile(predictDataTime , probs=(0.15))
		q25=quantile(predictDataTime , probs=(0.25))
		q55=quantile(predictDataTime , probs=(0.55))
		q80=quantile(predictDataTime , probs=(0.80))
		while(t <= 27){
			valueTime <- 5 # 初始默认选项为5
			if(predictDataTime[t]<=q5){valueTime=1}else{if(predictDataTime[t]<=q25){valueTime=2}else{if(predictDataTime[t]<=q55){valueTime=3}else{if(predictDataTime[t]<=q80){valueTime=4}}}}
			predictData[t,i] <<- valueTime #predictDataTime[t]
			t=t+1
		}
		i <- i+1
	}
	writeDataToCSV(predictData)
}

# 声明全局变量#################################################################
dataP=""
resultP=""
predictData=matrix(nrow=27,ncol=30)

# 手动修改参数#################################################################
scoreP=c(
132,
190,
200,
190,
120,
158,
150,
150,
209,
199,
200,
180,
164,
150,
136,
100,
225,
130,
199,
159,
200,
180,
159,
158,
132,
150,
190
)

# 调用函数#################################################################
mainFunction() # summary(resultP)

