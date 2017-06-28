#假设存在表一和表二
#表一DA包含的字段：ID、内容1、内容2
#表二DB包含的字段：ID、内容3、内容4

#data=count.fields("C:\\Users\\Administrator\\git\\R_practice_program_1\\NoteBook_R\\Book_Note_R语言编程艺术\\data_5_1.csv",sep=",")
#↑函数count.fields，读取数据并将数据按照step分割，格式类型data
data=read.csv("C:\\Users\\Administrator\\git\\R_practice_program_1\\NoteBook_R\\Book_Note_R语言编程艺术\\data_5_1.csv",sep=",",header=FALSE)
data=count.fields(data)
all(data>=5)

da=read.csv("DA",header=TRUE,stringsAsFactors=FALSE)
db=read.csv("DB",header=FALSE,stringsAsFactors=FALSE)

for(col in 1:6){
	print(unique(sort[da(,col)]))
}
	
mrg<-merge(da,db,by.x=1,by.y=1)
#↑合并数据，两个数据的第一项为键值

