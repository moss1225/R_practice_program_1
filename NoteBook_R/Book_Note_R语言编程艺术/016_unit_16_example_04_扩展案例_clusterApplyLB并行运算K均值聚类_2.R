# clusterApplyLB与clusterApply函数相似，但是clusterApplyLB的运行效率更高。在使用clusterApply并行运算时，如果每一
# 个内核花费不同的时间进行运算，那么在那个运行时间长的内核结束之前，运行时间段的内核不能进行下一次运算，
# 而clusterApplyLB不同，它是运行时间短的内核结束之后接着就运行下一次的运算，这样就减少了时间的浪费，因此提
# 高了效率。
# 为了说明clusterApplyLB的效率，我们使用控制任务时间长度的函数Sys.sleep。用snow.time收集整个执行过程的时间信息。
library("snow")
cl <- makeCluster(4 , type="SOCK")
set.seed(7777442)
sleeptime <- abs(rnorm(10 , 10 , 10))
tm <- snow.time(clusterApplyLB(cl , sleeptime , Sys.sleep))
# 运行clusterApplyLB
plot(tm)
tml <-snow.time(clusterApply(cl , sleeptime , Sys.sleep))
# 运行clusterApply
plot(tml)