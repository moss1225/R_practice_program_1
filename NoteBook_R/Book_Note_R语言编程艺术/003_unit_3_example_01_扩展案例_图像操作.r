#对图片部分打马赛克
library(jpeg)
img<-readJPEG("C:\\Users\\Administrator\\git\\R_practice_program_1\\NoteBook_R\\Book_Note_R语言编程艺术\\cara.jpg",native=TRUE)

bluepart<-function(objectA,beginRow,beginCols,rowA,colA,q){
	for(i1 in 1:rowA){
		for(i2 in 1:colA){
			
		}
	}
	return(objectA)
}

cara=bluepart(img,163,211,15,15,0.7)
writeJPEG(img, target = "C:\\Users\\Administrator\\Desktop\\a.jpg", quality = 0.7, bg = "white")