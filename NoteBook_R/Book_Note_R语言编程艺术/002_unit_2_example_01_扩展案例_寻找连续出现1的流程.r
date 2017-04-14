#假设一个向量由若干0和1构成，我们想找出其中连续出现1的游程。例如，对于向量
#(1,0,0,1,1,10,1,1)，从它第4索引处开始又称为3的游程，而长度为2的游程分别开始于4,5,8处
#因此，用函数FindRuns(c(1,0,0,1,1,10,1,1),2)返回又成为2的开始索引

run_data=c(1,0,0,1,1,10,1,1)
	#↑输入原始数据
	
FindRuns<-function(x,k){
	#↑创建函数，x为检测数据，k为游程的长度
	n<-length(x)
	runs<-NULL
	for(i in 1:n-k+1){
		if(all(x[i:(i+k-1)]==1))runs<-c(runs,i)
	}
	return(runs)
}

FindRuns(run_data,2)
	#↑应用数据