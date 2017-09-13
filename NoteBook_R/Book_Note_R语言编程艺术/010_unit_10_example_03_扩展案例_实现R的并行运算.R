#         一些统计分析需要运行很长时间，所以我们很自然会对"R并行计算"感兴趣。在R并行
# 计算的过程中，若干个R进程可以共同完成同一个任务。另外一个使用并行计算的原因是内
# 存限制。如果一台机器没有足够的内存来完成某个任务，就需要将若干个机器的内存用某
# 种方式组合起来使用。本书第16章将介绍这个重要的主题。
#         socket在许多R并行计算的软件包中起到了关键作用。并行协作的R进程既可以运行在同
# 一台计算机上，也可以运行在多台计算机器上。在后一种情况下（有时甚至包括前一种情况），实
# 现并行计算的一个很自然分方法是使用R的socket。这是snow包和我编写的Rdsm包（都可以
# 在R的代码仓库CRAN中获取到；详情请见本机附件）所采用的的一种方法，如下所示：
#         在snow中，服务器将任务发送给客户端，客户端完成各自的任务后将结果返回给服务
# 器，服务器再将结果汇总为最终结果。服务器与客户端之间的通信是通过serialize()
# 和unserialize()完成，服务器利用socketSelect()来确定哪些客户端结果已经计算完成了。
#        Rdsm实现了虚拟共享内存的范式，其中利用服务器来储存共享变量。每当客户端需要读
# 取或写入共享变量时就会与服务器进行通信。为了提升速度买服务器与客户端的通信
# 是通过readBIn()和writeBin()完成的，而不是用serialize()和unserialize()。

# set up socket connections with clients
#
cons <<- vector(mode="list" , length = ncon) # list of connections
# prevnt connection from dying during debug or long compute spell
options("timeout" = 10000)
for(i in 1:ncon){
	cons[[1]] <<- socketConnection(port=port , server = TRUE , blocking = TRUE , open = "a+b")
	# wait to hear from client i
	checkin <- unserialize( con[[i]] )
}
# send ACKs
for(i in 1 : ncon){
	# send the client its ID number , and the group size
	serialize(c(i , ncon) , cons[[i]])
}
#         由于客户端的信息和服务器的回应都是短消息，所以使用serialize()和unerialize()已
# 经足够了。
#         服务器端主循环的第一部分将寻找一个准备就绪的客户端，然后从中读取数据。
repeat{
	# any clients still there？
	if (remainingclients == 0) break
	# wait for service request , then read it
	# find all the pending client requests
	rdy <- which( socketSelsct(cons))
	# choose one
	j <- sample(1 : length(rdy) , 1)
}
#         同样， 此处serialize()和 unserialize()已经足够用于从客户端读取短消息，以说明请求
# 的是何种操作--通常是读取或写入一个共享变量。然而，在读取或写入共享变量时，我们
# 使用的是速度更快的readBin()和writeBin()函数，以下是写入部分的代码：
# write data dt , of mode md (integer of double) , to connection cn
binwrite <- function(dt , mt , cn){
	binwrite <- function(dt , mt , cn){
		writeBin(dt , con = cn)
}}
# 下面是读取部分代码
binread <- function(cn ,md ,sz){
	return(readBin(con = cn , what = md , n = sz))
}
# 在客户端，建立连接的代码如下所示：
options("timeout" = 1000)
# connect to server
con <- sockectConnection(host=host , port = port , block = TRUE , open="a+b")
serialize(list(req="checking in") , con)
myinfo <<- list(con = con , myid =myidandnclnt[1] , nlnt=myidandnclnt[2])