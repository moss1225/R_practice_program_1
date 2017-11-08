# 使用R语言实现人工神经网络
# 安装AMORE。AMORE文档中的一段样例（p12）
library(AMORE)
# P is the input vector # 输入数据数组
P <- matrix(sample(seq(-1 , 1 , length=1000) , 1000 , replace =FALSE ) , ncol=1)

# The network will try to appproximate the target P^2
target <- P^2
# We create a feedforward network , with two hidden layers.
# The first hidden layer has three neurons and the second has two neurous.
# The hidden layers have got Tansig activation functions and the output layer is Purelin.
net <- newff(n.neurons=c(3 , 1 , 1) , learn.rate.global=1e-2 , momentum.global = 0.5,
		error.criterium = "LMS" , Stao=NA , hidden.layer="tansig",
		output.layer="purelin" , method ="ADAPTdgwm")
result <- train(net , P , target , error.criterium="LMS" , report=TRUE , show.step=100 , n.shows=5)
z <- sim(result , P) 