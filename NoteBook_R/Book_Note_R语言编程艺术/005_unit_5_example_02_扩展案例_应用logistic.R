#项目目标：在爆鱼数据上建立Logistic模型，每次按照高度、重量、环数等8个变量
#中的一个预测性别。

#Logistic模型用一个或多个变量预测0-1随机变量Y。函数值表示给定解释变量Y时Y=1
#的概率。假设只有一个变量X。则模型形式如下：
#Pr(Y=1|X=t)=1/(1+exp[-B0+B1*t])

#与线性回归模型类似，使用函数glm()并设置参数family=binomial了，代入数据就可以估
#计出B1值。

#可以用sapply()来拟合8个变量模型，针对除去性别以外的8个变量，每个变量一个模型，
#拟合过程只用一行代码。

aba<-read.table("C:\\Users\\Administrator\\git\\R_practice_program_1\\NoteBook_R\\Book_Note_R语言编程艺术\\abalone.csv",header=T,as.is=T,sep=",")

#length(which(aba[,1]=="M"))
#↑雄性数量

abamf<-aba[which(aba$Gender !="I"),]
#↑排除幼崽数据
for(num in 1:nrow(abamf)){
	if(abamf[num,1]=="M"){
		abamf[num,1]=1
	}else{
		abamf[num,1]=0
	}
}

abamf[,1]=as.numeric(abamf[,1])
#↑将“M”的性别数据的字符串格式转换成数字格式

lftn<-function(clmn){
	glm(abamf$Gender~ clmn , family = binomial )$coef
	#↑函数glm是Logistic模型的lm()
}

loall<-sapply(abamf[,-1] , lftn)
#↑剔除性别数据，并通过sapply函数传入ltfn。

print(loall)
#↑输出数据

class(loall[1])
#↑查看所属类，glm()是lm()的一个子类。