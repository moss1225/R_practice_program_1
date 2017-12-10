# 程序字典
# dirPath:文件所在文件夹绝对地址
# fileNames：读取文件名称变量
# readDataFunction()：读取数据函数
# Type1：其中1表示种植业，2表示畜牧业，3表示水产

# 第一部分：基本设置
# 设置文件所在文件夹路径
dirPath <-"C:\\Documents and Settings\\Administrator\\git\\R_practice_program_1\\Programe_R\\Programe_005_根据农业数据生成报表\\data"
setwd(dirPath)
#初始化全局变量，在简单程序下，方便数据查看。
readData <-data.frame()

# 第二部分：功能函数
# 读取数据函数
readDataFunction <- function(fileNames){
	readData <- read.table(fileNames , header = TRUE , sep =",")
	#print(readData[2,2]) #通过print()输出数据
	martixData <- data.frame(readData)
	return(martixData)
	}

# 第三部分：主函数
mainFunction <- function(fileNames){
	# 获取数据
	readData <<- readDataFunction(fileNames)
	# 获取种植业信息
	readDataType1 <- readData[readData[,4]=="种植业" , 1:4]
	# 获取种植业信息数量 
	Type1num <- nrow(readDataType1)
} 

# 第四部分部分：通过对话框获取文件名称参数
# fileName <- readline("Please inpute your files name :")
fileName <-  "朝阳区.csv"
# 备选：fileNames <- scan()
		
# 第五部分：执行主函数
mainFunction( fileNames = fileName  )
# fileNames：读取文件名称
