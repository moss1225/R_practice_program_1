# 参数字典
# fileNames：读取文件名称
# readDataFunction()：读取数据

# 第一部分： 设置文件所在文件夹路径
dirPath <-"C:\\Users\\Administrator\\git\\R_practice_program_1\\Programe_R\\Programe_005_根据农业数据生成报表\\data"
setwd(dirPath)

# 第二部分：功能函数
# ↓读取数据函数
readDataFunction <- function(fileNames){
	readData <- read.table(fileNames , header = TRUE , sep =",")
	#print(readData[2,2]) #通过print()输出数据
	martixData <- data.frame(readData)
	return(martixData)
	}

# 第三部分：主函数
mainFunction <- function(fileNames){
	# 获取数据
	readData <- readDataFunction(fileNames)

} 

# 第四部分部分：通过对话框获取地址参数
fileName  <- readline("Please inpute your files name :")
# 备选：fileNames <- scan()
		
# 第五部分：执行主函数
mainFunction( fileNames = fileName  )
# fileNames：读取文件名称
