# plot(x$position~x$date[orderdatetime])
# 问题：做完图如何保存和导出图片？？
setwd("C://AllRGraphs") #选取储存目录，建议和R工作表分开
future = paste("position",".jpg") #通过paste将文件名和后缀连接起来
jpeg(file=future)
plot(x$position~x$date[orderdate])
dev.off()