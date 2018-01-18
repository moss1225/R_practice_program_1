# 1、加载数据
library(DMwR)

# 2、查看数据集前6行数据
head(algae)

# 3、读取Analysis.txt数据
# 用if的方式进行多行注释
if(F){ 
algae <- read.table('Analysis.txt' , 
	head=F ,
	dec='.' ,
	col.names=c('season' , 'size' , 'speed' ,  'mxPH' , 'mnO2' , 'C1' ,
	'NO3' , 'NH4' , 'oP04' , 'Ch1a' , 'a1' , 'a2' , 'a3' , 'a4' ,
	'a5' , 'a6' , 'a7'),
	na.strings=c('XXXXXXX'))
}# if end

# 4、数据可视化和摘要
# 获取数据统计特性的一个方法是获取数据的如此稀少
