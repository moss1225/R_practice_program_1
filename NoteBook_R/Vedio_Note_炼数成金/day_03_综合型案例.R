#一元线性回归
#身高与体重的回归关系
h=c(171,175,159,155,152,158,154,164,168,166,159,164)
	#↑身高数据
w=c(57,64,41,38,35,44,41,51,57,49,47,46)
	#↑体重数据
x=data.frame(h,w)
	#↑创建"身高-体重"数据框x
a=lm(w~1+h)
	#↑利用系统自带的一元回归模型
print(paste("斜率",as.character(a[1]$coefficients[2]),sep=":"))
	#↑输出斜率，paste()字符串粘贴，as.character()转换成字符串
print(paste("截距",as.character(a[1]$coefficients[1]),sep=":"))
	#↑输出截距
plot(x,main="身高与体重的回归模型",xlim=c(100,200),ylim=c(30,80))
	#↑绘制"身高-体重"散点图
lines(h,a[1]$coefficients[1]+a[1]$coefficients[2]*h)
	#↑绘制"身高-体重"一元线性回归直线
z=data.frame(h=185)
	#↑将预测身高185转换成数据框格式
p=predict(a,z)
	#↑通过predict()函数预测，a为线性模型，z为预测对象
points(z,p[1],col="red")