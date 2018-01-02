dirPath <-"C:\\Users\\Administrator\\git\\R_practice_program_1\\Programe_R\\Programe_006_主体数据核查分析\\data"
setwd(dirPath)
fileNames="agri.csv"
readData <- read.table(fileNames , header = TRUE , sep =",")
mainData <- data.frame(readData)

# 企业、合作社、个人序号
num1 <- which(mainData[,2]=="企业")
num2 <- which(mainData[,2]=="合作社")
num3 <- which(mainData[,2]=="个人")

# 企业、合作社、个人调查总数
sum1 <- sum(mainData[num1 ,  c(3 , 4 , 5)] ) # 企业
sum2 <- sum(mainData[num2 ,  c(3 , 4 , 5)] ) # 合作社
sum3 <- sum(mainData[num3 ,  c(3 , 4 , 5)] ) # 个人

# 使用factor()函数将地区属性因子化
factorDistrict <- factor(mainData[ , 1])
# 获取地区属性因子水平
levDistrict <- c( levels(factorDistrict) )

# 各城区调查总数
# 创建地区统计结果集
District=c("地区")
sumNum=c("数量")
DistrictData <- data.frame(District , sumNum , stringsAsFactors =F)
# 通过循环计算和返回该地区总数
for ( i in levDistrict){
	sumDistrict <- sum(mainData[which(mainData==i) , c(3 , 4 , 5)])
    # print( paste(i , sumDistrict))
	# 将结果返回到结果集合
	addDistrict <- data.frame(District=c(i) , sumNum=c(sumDistrict) , stringsAsFactors =F)
	# print(addDistrict)
	DistrictData <- data.frame(District=c(DistrictData$District , addDistrict$District) , sumNum=c(DistrictData$sumNum , addDistrict$sumNum) , stringsAsFactors =F)
}