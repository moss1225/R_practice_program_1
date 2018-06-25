#��ȡʱ�����еİ�
library(xts)
# �Ӱ��л�ȡ���ݼ�
library(DMwR)
data(GSPC)
# ��ȡ����csv�ļ���GSPCΪ��Ʊʱ����������
setwd("C:\\Users\\Administrator\\git\\R_practice_program_1\\NoteBook_R\\Book_Note_�����ھ���R����")
GSPCdata<-data.frame(read.csv("^GSPC.csv" , head=T))
GSPC <- as.xts(read.zoo(GSPCdata) , head=T)
# ���ú���T.ind������Ŀ�꣺
T.ind <- function(quotes , tgt.margin = 0.025 , n.days=10){
	# ���ú���HLC������Ŀ�꣺ȡ�����ֵ�����ֵ������ֵ
	HLC <- function(x){
		x = x[,c("High" , "Low" , "Close")]
		return(x)
	}
	# ����ÿ��ƽ���۸�
	v <- apply(HLC(quotes) , 1 , mean)
	#�����վ������ڴ��棿
	r <- matrix(NA , ncol = n.days , nrow=NROW(quotes))
	# ���ú���Delt������Ŀ�꣺����δ��n�����Ը��յİٷֱ����棬vƽ���۸�k�Ա�����
	# ˼·[(ƽ����i+j)-(���̼�i)]/(���̼�i)
	Delt <- function(v , k){
		# ����i+k�����i�����䶯�İٷֱ�
		DATArows <- NROW(quotes)
		DeltReturnData <- matrix(NA , ncol=1 , DATArows)
		for(i in 0:(DATArows-k)){
			DeltReturnData[DATArows - i , 1]=(v[DATArows-i]-v[DATArows-i-k+1])/v[DATArows-i-k+1]
		}
		return(DeltReturnData)
	}
	# ���ú���Next()������Ŀ�꣺��ʱ��ƽ��һ��ʱ������
	Next<-function(f , x){
		# Next������Delt�ϲ�
	}
	# ����Next��Delt����������������v��k��x�������������r��
	# for(x in 1:n.days) r[,x]<-Next(Delt(v , k = x ) , x) # �޸ĳ����д���
	for(x in 1:n.days) r[,x]<-Delt( v , k = x )
	x <- apply(r , 1 , function(x) sum(x[x> tgt.margin | x < -tgt.margin] ))
	if(is.xts(quotes)) xts(x , time(quotes)) else x
	### ����Ϊ���Բ���
	# return (x)
}
### ����Ϊ���Բ���
#data=T.ind(GSPC, 0.025,10)