#寻找每一行离平均值最远数值的位置
findols<-function(x){
	findol<-function(xrow){
		mdn<-median(xrow)	
		devs<-abs(xrow-mdn)
		return(which.max(devs))
	}
	return(apply(x,1,findol))
	#↑函数体内调用子函数
}

x=matrix(c(1,2,3,4,5,9,2,3,5,6,4,5),nrow=2,byrow=T)

findols(x)

