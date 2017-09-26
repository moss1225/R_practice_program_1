#         当你用curve绘制一个函数后，你可以希望能"放大"这条曲线的某一部分。要实现
# 这一效果，你需要在同一函数上再次调用curve()，并将x的范围进行限制。然后，或许你想
# 要将原始的曲线和放大后的曲线展示在同一幅图中。在此，我们将边线一个名为inset()的函
# 数来实现这一目标。
#          为了避免重复curve()绘制原始曲线工作，我们将稍微修改其代码，即为其加上一个返
# 回值，以保存当前的工作。由于查看R现成的函数的源代码很容易(与之相反，那些用C语
# 言写成的底层R函数往往不容易查看)，我们可以利用这一优势达到修改函数的目的，如
# 下所示：
# curve
# 上述查看curve的源代码，复制源代码，进行修改，并将其命名为crv()：
crv<-function (expr, from = NULL, to = NULL, n = 101, add = FALSE, 
		type = "l", xname = "x", xlab = xname, ylab = NULL, log = NULL, 
		xlim = NULL, ...) 
{
	sexpr <- substitute(expr)
	if (is.name(sexpr)) {
		expr <- call(as.character(sexpr), as.name(xname))
	}
	else {
		if (!((is.call(sexpr) || is.expression(sexpr)) && xname %in% 
					all.vars(sexpr))) 
			stop(gettextf("'expr' must be a function, or a call or an expression containing '%s'", 
							xname), domain = NA)
		expr <- sexpr
	}
	if (dev.cur() == 1L && !identical(add, FALSE)) {
		warning("'add' will be ignored as there is no existing plot")
		add <- FALSE
	}
	addF <- identical(add, FALSE)
	if (is.null(ylab)) 
		ylab <- deparse(expr)
	if (is.null(from) || is.null(to)) {
		xl <- if (!is.null(xlim)) 
					xlim
				else if (!addF) {
					pu <- par("usr")[1L:2L]
					if (par("xaxs") == "r") 
						pu <- extendrange(pu, f = -1/27)
					if (par("xlog")) 
						10^pu
					else pu
				}
				else c(0, 1)
		if (is.null(from)) 
			from <- xl[1L]
		if (is.null(to)) 
			to <- xl[2L]
	}
	lg <- if (length(log)) 
				log
			else if (!addF && par("xlog")) 
				"x"
			else ""
	if (length(lg) == 0) 
		lg <- ""
	if (grepl("x", lg, fixed = TRUE)) {
		if (from <= 0 || to <= 0) 
			stop("'from' and 'to' must be > 0 with log=\"x\"")
		x <- exp(seq.int(log(from), log(to), length.out = n))
	}
	else x <- seq.int(from, to, length.out = n)
	ll <- list(x = x)
	names(ll) <- xname
	y <- eval(expr, envir = ll, enclos = parent.frame())
	if (length(y) != length(x)) 
		stop("'expr' did not evaluate to an object of length 'n'")
	if (isTRUE(add)) 
		lines(x = x, y = y, type = type, ...)
	else plot(x = x, y = y, type = type, xlab = xlab, ylab = ylab, 
				xlim = xlim, log = lg, ...)
	invisible(list(x = x, y = y))
	return(list(x=x , y=y))
	# 唯一修改的一行
}
# 接下来得到inset()函数
# suvexy : list consisting of x and y vector returned by crv()
# x1 , y1 , x2 , y2 : coordinates of rectangular region to be magnified
# x3 , y3 , x4 , x4 : coordinates of inset region
inset <- function(savexy , x1 , y1 ,x2 , y2 , x3 , y3 , x4 , y4){
	# savexy：返回函数的数值
	rect(x1 , y1 , x2 , y2)
	rect(x3 , y3 , x4 , y4)
	# rect 函数用来在一张图上添加矩形，只需要指定左下角和右上角的坐标的位置，就可以画出一个矩形
	# get vector of coordinates of previously plotter points
	savex <- savexy$x
	# 自变量x的取值范围
	savey <- savexy$y
	# 因变量y的数值
	# get subscripts of xi our range to be magnified
	n <- length(savex)
	xvalsinrange <- which(savex >= x1 & savex <= x2)
	# 获取放大部分的因变量的下标
	yvalsforthosex <- savey[xvalsinrange]
	# 获取放大部分的自变量的数值
	# check that our first box contains the entire curve for that X range
	if(any( yvalsforthosex < y1 | yvalsforthosex > y2)){
		print("Y value outside first box")
		return()
	}
	# record some differences
	x2mnsx1 <-x2 - x1
	x4mnsx3 <-x4 - x3
	y2mnsy1 <-y2 - y1
	y4mnsy3 <-y4 - y3
	# for the ith point in the original curve ,the function plotpt() will
	# calculate the position of this point in the inset curve
	plotpt <- function(i){
		newx <- x3+((savex[i] - x1)/x2mnsx1)*x4mnsx3
		newy <- y3+((savey[i] - y1)/y2mnsy1)*y4mnsy3
		# 先标准化，再放大倍数
		# 并让图像画在矩形里
		return(c(newx , newy))
	}
	newxy <- sapply(xvalsinrange , plotpt)
	lines(newxy[1 , ] , newxy[2 , ])
}

xyout <- crv(exp(-x)*sin(1/(x-1.5)) , 0.1 , 4 , n=5001)
inset(xyout , 1.3 , -0.3 , 1.47 , 0.3 , 2.5 , -0.3 , 4 , -0.1 )
