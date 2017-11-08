# 使用R语言实现人工神经网络
# 给定线性数据 Y = (X1)^2 + (X2)^2 ，实验中未知。
# 思路：
# 1、随机选取2000个样品。1900个作为学习集，100个作为验证集
# 2、先使用2-5-1类型的BP神经网络进行训练和拟合
# 3、建立神经网络模型并用学习集进行训练

	# BP神经网络构建
# net = newff(inputn , outputn , 5) ;
	# 网络参数配置(迭代次数、学习率、目标)
# net.trainParam.epochs = 100 ;
# net.trainParam.lr=0.1 ;
# net.trainParam.goal=0.00004 ;
	# BP神经网络训练
# net = train(net ,inputn , outputn) ;

