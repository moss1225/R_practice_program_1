#          parLapply是snow包中的一个高级函数，它比clusterApply函数更搞笑，如果参数x的长度与内核数量相等，parLapply
# 的优势不明显，如果参数x的长度远大于内核的数量，parLapply相比clusterApply是一个更好的选择。
#          应用一个并行休眠函数的用法，首先使用clusterApply。
library("snow")
cl <- makeCluster(4 , type="SOCK")
bigsleep <- function(valA , mat){
	Sys.sleep(valA)
}
bigmatrix <- matrix(0 , 2000 , 2000)
sleeptime <- abs(rep(0.1 , 100))
# 设置参数
#tm4<-snow.time(parLapply(cl , sleeptime , bigsleep)) # 单一参数形式
testTime<-snow.time(parLapply(cl ,  sleeptime , bigsleep , bigmatrix)) # 25s
# 说明：parLapply(选择集群 , 参数一 , 调用函数 , 参数二)
# testTime <- snow.time(clusterApply(cl , sleeptime, Sys.sleep ))
# 测试clusterApply方法 , 似乎没区别。
plot(testTime)