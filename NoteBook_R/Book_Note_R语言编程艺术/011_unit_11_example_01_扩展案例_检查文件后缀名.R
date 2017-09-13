testsuffix <- function(fn , suff){
	# fn为文件名，suff是测试的文件名结尾
	parts <- strsplit(fn , "." , fixed = TRUE)
	# 将fn根据"."分为若干个字符串，parts[[1]][1]表示第一个分割后的字符串
	# 关键参数fixed，如果TRUE匹配split完全，否则使用正则表达式。
	# 也可以用strsplit(fn , "//." )代替
	naparts <- length(parts[[1]])
	# 获取有几段分割后的字符串
	return(parts[[1]][naparts] == suff)
	# 返回后缀名是否等于suff
}

# 测试一下
testsuffix("x.abc" , "abc")