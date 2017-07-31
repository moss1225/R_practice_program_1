# 扩展案例：显示调用框函数
#        在调用模式下逐步运行代码时，你经常希望知道当前函数中的局部变量的数值。你也许也
#会想知道上级函数(即调用当前函数的函数)的局部变量的数值。这里，我们会通过编写代码展示
#这些数值，并且进一步展示如何访问环境层次。（此处所说的代码改编自我编写的调用调试工具
#edtdbg，edtdbg在R的CRAN代码库中可以找到。）

# 显示调用框函数
showframe <- function(upn){
	if(upn < 0){
		env <- .GlobalEnv
	}else{
		env <- parent.frame(n=upn+1)
	}	
	vars  <- ls ( envir = env )
	for(vr in vars){
		vrg <- get(vr , envir = env)
		#↑函数get()用法：它的功能非常简单：输入对象的名称，它就会输出该对象。
		if(!is.function(vrg)){
			cat(vr,":\n",sep="")
			#↑函数cat()的作用，与paste()类似，此处为了输出标题，类似“aa:”
			print(vrg)
		}
	}
}

# 测试函数
f <-function(){a<-1 ; return (paste( 'result of function f():' , as.character(g ( a )+a)) )}
g<-function(aa){b<-2 ; showframe(0);showframe(1);aab <- h(aa+b) ; return(aab)}
h<-function(aaa){c <-3;return(aaa+c)}
#调用函数
f()