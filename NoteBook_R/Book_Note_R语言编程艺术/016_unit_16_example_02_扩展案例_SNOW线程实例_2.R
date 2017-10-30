cl <- makeSOCKcluster(c("localhost","localhost"))
x=c(1 , 2)
countX <- function(x){return(x+1)}
testX=clusterMap(cl , countX ,c(1,2))
#↑传入不同的值进行计算