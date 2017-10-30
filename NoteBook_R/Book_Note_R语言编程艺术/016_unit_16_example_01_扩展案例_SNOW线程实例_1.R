cl <- makeSOCKcluster(c("localhost","localhost"))
#↑ 创建集群
clusterApply(cl, 1:2, get("+"), 3)
#↑ 第一个节点算1+3第二个节点算2+3
clusterEvalQ(cl, library(boot))
#↑ 每个节点加载lib
x<-1
clusterExport(cl , "x")
#↑ 每个节点加载x作为环境变量
testX <- clusterCall(cl, function(y)return(x + y), 2)
#↑ 每个节点计算一次1+3，并将计算结果返回给testX。