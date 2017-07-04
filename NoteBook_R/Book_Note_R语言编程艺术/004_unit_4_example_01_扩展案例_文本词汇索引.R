# TODO: Add comment
# 网络搜索和其他形式的文本挖掘现在越来越热门了。我们用这个领域的例子演示列表的使用方法。
#我们先写一个findwords()函数来找到一个文本文件中的全部单词，并且标出各个单词所在位置。
#这个函数在语境分析中很有用。
#建设输入文本为textconccord.txt的内容如下：
#使用列表相对于矩阵，不用确定列数，节省空间。
# Author: Moss
data<-readLines("C:\\Users\\Administrator\\git\\R_practice_program_1\\NoteBook_R\\Book_Note_R语言编程艺术\\data_4_1.txt")
#↑按行读取信息
data<-gsub(",|\\.|!|?","",data)
data=as.character(data)
#↑用正则表达式去除句子中的标点
 writeLines(data, "C:\\Users\\Administrator\\git\\R_practice_program_1\\NoteBook_R\\Book_Note_R语言编程艺术\\data_4_1_change.txt")
#↑写入字符串到文件
findwords<-function(tf){
	txt<-scan(tf,"")
	#↑将数据读取，并且按分隔符位置，读取为小数据。
	wl<-list()
	#↑创建列表，用于储存记录。
	for (i in 1:length(txt)){
		wrd<-txt[i]
		#↑将字符串赋值到wtd
		wl[[wrd]]<-c(wl[[wrd]],i)
		#↑字符串wl[[wrd]]，其中wrd为索引
		#↑索引wrd相同，添加位置i。索引wrd不同，增加索引，添加位置，
	}
	return(wl)
}

findwords("C:\\Users\\Administrator\\git\\R_practice_program_1\\NoteBook_R\\Book_Note_R语言编程艺术\\data_4_1_change.txt")
#引用函数