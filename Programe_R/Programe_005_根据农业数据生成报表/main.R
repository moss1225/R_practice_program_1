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

# 读取企业、合作社、个人、其他不同主体类型的函数
countTypeNum <- function(readDataType1){
	Type1num <- nrow(readDataType1)
	# 获取种植业企业数量
	readDataType11 <- nrow(readDataType1[readDataType1[,6]=="企业" , ])
	# 获取种植业合作社数量
	readDataType12 <- nrow(readDataType1[readDataType1[,6]=="合作社" , ])
	# 获取种植业个人数量
	readDataType13 <- nrow(readDataType1[readDataType1[,6]=="个人" , ])
	# 获取种植业其他数量
	readDataType14 <- nrow(readDataType1[readDataType1[,6]=="其他" , ])
	# 返回数据，分别是该行业总数、行业企业、行业畜牧、行业个人、行业其他
	return(c(Type1num , readDataType11 , readDataType12 , readDataType13 , readDataType14))
} 

# 自动编写文档函数
writePaper <- function(numAllData , numDataType1 , numDataType2 , numDataType3){
	return(paste("朝阳区登录在案经确认的主体信息", numAllData , "条，其中种植信息" , numDataType1[1] ,"条，畜牧信息" , numDataType2[1] ,"条，水产" , numDataType3[1]  ,"条，（1）种植信息详细：企业数量" , numDataType1[2] ,"条（约占种植总数%），合作社" , numDataType1[3],"条（约占种植总数%），个人", numDataType1[4] ,"条（约占种植总数%），其他类型", numDataType1[5],"条（约占种植总数%）。（2）畜牧信息详细：企业数量" , numDataType2[2] ,"条（约占种植总数%），合作社" , numDataType2[3] ,"条（约占种植总数%），个人" , numDataType2[4] ,"条（约占种植总数%），其他类型" , numDataType2[5] ,"条（约占种植总数%）。（3）渔业信息详细：企业数量" , numDataType3[2] ,"条（约占种植总数%），合作社" , numDataType3[3] ,"条（约占种植总数%），个人" , numDataType3[4] ,"条（约占种植总数%），其他类型" , numDataType3[5] ,"条（约占种植总数%）。"))
}

# 第三部分：主函数
mainFunction <- function(fileNames){
	# 获取数据
	readData <<- readDataFunction(fileNames)
	# 总数据量
	numAllData <- nrow(readData)
	# 获取种植业信息
	readDataType1 <- readData[readData[,4]=="种植业" , ]
	# 获取种植业信息数量 
	numDataType1 <- countTypeNum(readDataType1)
	# 获取种植业信息
	readDataType2 <- readData[readData[,4]=="畜牧业" , ]
	# 获取种植业信息数量 
	numDataType2 <- countTypeNum(readDataType2)
	# 获取种植业信息
	readDataType3 <- readData[readData[,4]=="渔业" , ]
	# 获取种植业信息数量 
	numDataType3 <- countTypeNum(readDataType3)
	# 调用编写文档
	# print(writePaper(numAllData , numDataType1 , numDataType2 , numDataType3))
	print(numAllData)
	print(numDataType1[c(2:5,1)])
	print(numDataType2[c(2:5,1)])
	print(numDataType3[c(2:5,1)])
	print(paste("企业" , numDataType1[2]+numDataType2[2]+numDataType3[2]))
	print(paste("合作" , numDataType1[3]+numDataType2[3]+numDataType3[3]))
	print(paste("个人" , numDataType1[4]+numDataType2[4]+numDataType3[4]))
	print(paste("其他" , numDataType1[5]+numDataType2[5]+numDataType3[5]))
} 

# 第四部分部分：通过对话框获取文件名称参数
# fileName <- readline("Please inpute your files name :")
fileName <-  "怀柔区.csv"
# 备选：fileNames <- scan()
		
# 第五部分：执行主函数
mainFunction( fileNames = fileName  )
# fileNames：读取文件名称
