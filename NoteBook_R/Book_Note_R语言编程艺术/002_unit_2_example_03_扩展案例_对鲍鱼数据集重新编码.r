aba<-read.table("C:\\Users\\Administrator\\git\\R_practice_program_1\\NoteBook_R\\Book_Note_R语言编程艺术\\abalone.csv",header=T,as.is=T,sep=",")
	#↑引入abalone鲍鱼数据集

m=which(aba[,"Gender"]=="M")
f=which(aba[,"Gender"]=="F")
i=which(aba[,"Gender"]=="I")
	#↑按性别筛选索引

grps<-list()
	#↑创建grps列表

for(gen in c("M","F","I")){
	grps[[gen]]=which(aba[,"Gender"]==gen)
}
	#↑按性别筛选索引，并将索引放在列表grps。

abaM=aba[grps[["M"]],]
abaF=aba[grps[["F"]],]
abaI=aba[grps[["I"]],]
	#↑依据索引，将数据按分类放置在对象里。
plot(abaM$Length,abaM$Diameter)
points(abaF$Length,abaF$Diameter,col="red",pch="x")







