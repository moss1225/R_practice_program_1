# TODO: Add comment
# 安装maps
# 安装geosphere
# 包加载
# Author: Moss
###############################################################################
library(maps)
library(geosphere)
		#↑装载安装包
map("state")
		#↑画出美国地图
map("world")
		#↑画出世界地图
xlim<-c(-171.738281,-56.601563)
ylim<-c(12.039321,71.855229)
map("world",col="#f2f2f2",fill=TRUE,bg="white",lwd=0.05,xlim=xlim,ylim=ylim)
		#↑画出世界地图
lat_ca<-39.164141
lon_ca<--121.64062
lat_me<-45.21300
lon_me<--68.905250
		#↑起始坐标和终点坐标
inter<-gcIntermediae(c(lon_ca,lat_ca),c(lon_me,lat_me),n=50,addStartEnd=True)
lines(inter)
airports<-table.csv("http://",header=TRUE)
flights<-table.csv("http://",header=TRUE,as.is=TRUE)
