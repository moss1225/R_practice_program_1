# 通过mingw-get-setup在windows安装gcc软件
# 将Basic Setup全部安装
# 将ALL Paclages里的MinGW中的Base System全部安装
# 编写名为sd.c的C文件
#在cmd中调整目录
# 输入 gcc -std=gn99 -fpic -g -o2 -c sd.c sd.o
# 将 sd.c 转义为 sd.o
# 输入 gcc -std=gn99-shared -o sd.o sd.so
# 将 sd.o 转义为 sd.so

# 引入so文件
dyn.load("sd.so")
# 输入数据
m <- rbind(1:5 , 6:10 , 16:20 , 21:25)
k=2
# 调用函数
.C("subdiag" , as.double(m) , as.integer(dim(m)[1]),as.integer(k),result=double(dim(m)[1]-k))