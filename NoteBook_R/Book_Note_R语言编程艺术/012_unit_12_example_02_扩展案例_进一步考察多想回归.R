#         考虑有一个预测变量的统计回归模型。由于任何统计模型都仅是对显示问题的近似，原
# 则上你可以用更高阶的多项式拟合出更接近数据的模型。尽管如此，在某个程序上这会带来
# 过拟合（overfitting）问题，当多项式的阶数高于一定值时，模型对新的、将来的数据做出
# 的预测实际上效果很糟糕。

#        "polyreg"类可以处理这个问题，它拟合若干个不同阶数的多项式模型后通过交叉验
# 证（cross-validation）来评估拟合度，以减少过拟合的风险。此处的交叉验证又称为"留一
# 在外法"（leaving-one-out method），即对于每个点，使用除了这个点的观测值以外所有的数
# 据建立回归模型，并根据拟合模型求出该点的预测值。这个类的对象包括各个回归模型的输
# 出，以及原始数据。

# "polyerg ," S3 class for polynomial regression in one predictor variable
# polyfit(y , x , maxdeg) fits all polynomials up to degree maxdeg ; y is
# vector for response variable , x for predictor ; creates an object of 
# class "polyreg"
# 该函数是针对"polyreg"类的构造函数
# 作为改程序的主函数
polyfit <- function( y , x , maxdeg){
	# y：因变量
	# x：自变量
	# maxdeg：事先设定的最高阶数
	# form powers of predictor variable , ith power in ith column
	pwrs <- powers( x , maxdeg ) # could use orthog polys for greater accuracy
	# pwrs 的格式：第一列是原始x，第二列是x^2，第n列是x^n
	# power()为自定义函数
	lmout <- list() # state to bulid class
	class(lmout) <- "ployreg" # create a new class
	for(i in 1:maxdeg){
		lmo <- lm(y~pwrs[,1:i])
		# extend the lm class here ,with the class-validated predictions
		lmo$fitted.cvvalues <- lvoneout(y , pwrs[ , 1 : i , drop=F ])
		lmout[[i]] <- lmo
	}
	lmout$x <- x
	lmout$y <- y
	return(lmout)
}

# print() for an object fits of class "polyreg" : print
# cross-validated mean-squared prediction errors
# 该函数是针对"polyreg"类的打印函数
print.polyreg <- function(fits){
	maxdeg <- length(fits) - 2
	n <- length(fits$y)
	tbl <- matrix(nrow=maxdeg , ncol = 1)
	colnames(tbl) <- "MSPE"
	for(i in 1 : maxdeg){
		fi <- fits[[i]]
		errs <- fits$y-fi$fitted.cvvalues
		spe <- crossprod (errs , errs) # sum of squarred of predicted errors
		tbl[i , 1] <- spe/n
	}
	cat("mean squared prediction errors ,by degree\n")
	print(tbl)
}

# forms matrix of powers of the vector x , thought degree dg
# 该函数用于根据x和最大阶数dg，返回对应格式的数据
powers <- function(x , dg){
	pw <- matrix(x , nrow=length(x))
	prod <- x
	for(i in 2:dg){
		prod <- prod * x
		pw <- cbind(pw , prod)
	}
	return(pw)
	# pw 的格式：第一列是原始x，第二列是x^2，第n列是x^n
}

# find cross-validated predicted values ; could be mad much faster via
# matrix-update method
# 通过"留一在外法"，通过比较预测数据的误差，确定函数的阶数。
lvoneout <- function(y , xmat){
	n <- length(y)
	predy <- vector(length=n)
	for(i in 1:n){
		# regress , leaving out ith observation
		lmo <- lm(y[-i]~xmat[-i , ])
		# 上述所说的leaving-one-out method，用于检测误差，
		# 用测试集抽出第i行，进行结果检验。
		betahat <- as.vector(lmo$coef) # $coef 返回截距和各阶的参数，对于一元就是返回截距和斜率
		print(paste("循环第" , i))
		print("betahat:") # 输出变量
		print(betahat) # 输出变量
		# the 1 accomodates the constant term
		predy[i] <- betahat %*% c(1 , xmat[i , ])
		# 将抽出的那一行，代入预测的表达式
		# 1用于 pred=截距*1 + C1*x + C2 *x^2 ...
	}
	return(predy)
}

# polynomial function of x , coefficients cfs
poly <- function(x , cfs){
	val <- cfs[1]
	prod <- 1
	dg <- length(cfs) - 1
	for (i in 1:dg){
		prod <- prod*x
		val <- val + cfs[i + 1]*prod
	}
}

# 以下为新增代码
plot.polyreg <- function(fits){
	plot(fits$x , fits$y , xlab="X" , ylab="Y")
	# plot data points as background
	maxdg <- length(fits)-2
	cols <- c("red" , "green" , "blue")
	dg <- curvecount <- 1
	while(dg < maxdg){
		prompt <- paste(" RETURN for XV fit for degree" , dg ,  "or type degree" , "or q for quit")
		rl <- readline(prompt)
		# 等待读取输入框输入信息，该效果达到等待的效果。
		dg<-if(rl == ""){dg}else{
					if(rl != "q"){
						as.integer(rl)}else{
						break}}
		lines(fits$x , fits[[dg]]$fitted.values , col=cols[curvecount%%3 +1])
		dg=dg+1
		curvecount <- curvecount+1
	}
}


n <- 60
x <- (1 : n)/n
y <- vector(length = n)
for( i in 1:n) y[i]<-sin((3*pi/2)*x[i] + x[i])^2 + rnorm(1 , mean = 0 , sd = 0.5)
dg=15
(lmo <- polyfit( y , x , dg))
# 将整个赋值语句放在括号里，就能在打印出结果的同时创建变量lmo，以免有时可能会用到它。
print.polyreg (lmo)
# 输出结果里最后几个均方差为NA，这里是因为摄入误差太高，使得R无法估计级数如此高的多项式模型

# 新增代码，绘制图形，多次恩回车，达到插入数数据框的作用。
plot.polyreg(lmo)
