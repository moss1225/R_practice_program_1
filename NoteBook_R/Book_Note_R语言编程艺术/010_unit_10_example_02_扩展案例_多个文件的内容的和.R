#          现在，我们要建立一个函数来计算目录树下所有文件内容之和(假设都是数值型)。在
# 这个例子中，文件目录dir1包含文件filea和fileb，以及子目录dir2，它包含文件filec，这些
# 文件内容如下：
# filea：5 , 12 , 13
# fileb：3 , 4 , 5
# filec：24 , 25 , 7
#         如果dir1是当前工作目录，调用sumtree("dir1")会返回这些文件包含的9个数字之和，
# 98。否则，我们需要设定dir1的完整路径名，例如sumtree("/home/nm/dir1")。代码如下：
sumtree <- function(drtr){
	tot <- 0
	# get names of all files in the tree
	fls <- dir(drtr , recursive = TRUE)
	# 获取该文件目录下所有文件名称
	for(f in  fls){
		# is f a directory?
		f <- file.path(drtr , f)
		if(!file.info(f)$isdir){
			tot <- tot + sum(scan(f , quiet=TRUE))
		}
	}
	return(tot)
}
#         请注意，这个问题在递归中很常见，我们在7.9节也会详细讨论。但是这里没通过设置
# dir()中的一个参数，R已经完整这个递归。因此，在第12行设置recursive=TRUE，以便查找
# 整个目录树各个层级的文件。

#         在调用file.info()时，有个事实必须事先说明：当前文件名f是相对于drtr的，所以文件
# filea指的是dir1/filea。为了形成路径名，需要把drtr、斜杠以及filea拼接起来，及使用的
# R的字符串拼接函数paste()，但是对于Windows系统，需要使用反斜杠，而不是斜杠。不过
# file.path()实现了上面所有的步骤。

#         有必要解释下第17行的意义。file.info()返回一个包含文件/所有信息的数据框，其中
# 一列为isdir()，每一行对应一个文件且行名为文件名。该列已包含不二变量表示每个文件是否
# 为目录。在第17行中，可以检测当前文件f是否为目录。如果f是一个普通文件，就继续运行，将
# 其内容添加到求和运算中。