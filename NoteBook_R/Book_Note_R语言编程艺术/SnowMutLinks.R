# INTRO: 
#	This pragmme is uesd to unit 16 snow package test
# 运行不成功，逻辑不清晰，放弃。
# 运行问题，下标出界。
# Author: Moss
###############################################################################

# 为了说明snow是如何工作的，下面先给出上一节共同外链问题的代码。
# snow version of mutual links problem
mtl <- function(ichunk , m){
	print("mtl 已经执行")
	n <- ncol(m)
	matches <- 0
	for(i in ichunk){
		if(i < n){
			print("循环"+i)
			rowi <- m[i , ]
			matches <- matches + sum(m[i+1:n , ]%*% rowi )
		}
	}
	matches
}

mutlinks <- function(cls , m){
	n <- nrow(m)
	nc <- length(cls)
	# determine which worker gets which chunk of i 
	options(warn = -1)
	ichunks <- 2 #split(1:n , 1:nc) # ? 可能穿在错误ichunks <- split(1:nc , 1:n)
	options(warn = 0)
	counts <- clusterApply(cls , ichunks , mtl , m)
	do.call(sum , counts) / (n*(n-1)/2)
}

cl <- makeCluster(4, type="SOCK")
testm <- matrix( , nrow=4)
mutlinks(cl ,testm)