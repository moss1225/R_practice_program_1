#          parLapply��snow���е�һ���߼�����������clusterApply��������Ц���������x�ĳ������ں�������ȣ�parLapply
# �����Ʋ����ԣ��������x�ĳ���Զ�����ں˵�������parLapply���clusterApply��һ�����õ�ѡ��
#          Ӧ��һ���������ߺ������÷�������ʹ��clusterApply��
library("snow")
cl <- makeCluster(4 , type="SOCK")
bigsleep <- function(valA , mat){
	Sys.sleep(valA)
}
bigmatrix <- matrix(0 , 2000 , 2000)
sleeptime <- abs(rep(0.1 , 100))
# ���ò���
#tm4<-snow.time(parLapply(cl , sleeptime , bigsleep)) # ��һ������ʽ
testTime<-snow.time(parLapply(cl ,  sleeptime , bigsleep , bigmatrix)) # 25s
# ˵����parLapply(ѡ��Ⱥ , ����һ , ���ú��� , ������)
# testTime <- snow.time(clusterApply(cl , sleeptime, Sys.sleep ))
# ����clusterApply���� , �ƺ�û����
plot(testTime)