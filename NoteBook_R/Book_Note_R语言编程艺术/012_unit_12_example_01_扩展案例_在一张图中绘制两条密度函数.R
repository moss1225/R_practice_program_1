d1 = density(testscore$Exam1 , from=0 , to=100)
d2 = density(testscore$Exam2 , from=0 , to=100)
# �ܶȺ���density
plot(d1 , main = "" , xlab="")
lines(d2)