#         我们创建一个叫extractpums()的函数读取PUMS文件，并且利用其中所有个人记录创建
# 一个数据集。使用者可以设定文件名以及一个列表参数，这个列表由代提取的字符范围和相应
# 的名称组成。

# reads in PUMS file pf , extracting the Person records , return a data
# frame ; each row of output will consist of Household serial
# number and the fields specified in the list flds ; the columns of
# the data frame will have the names of the indice in flds

# 主函数
extractpums <- function(pf , flds){
	dtf <- data.frame() # data frame to be build
	con <- file(pf , "r") # connection
	# process the input file
	repeat{
		hrec <-  readLines(pf , "r")
		if(length(heac) == 0 ) break # end the file , leave loop
		# get household serial number
		serno <- intextract(hrec , c(2 , 8))
		# how many Person records ?
		npr <- intextract(hrec , c(106 , 107))
		if(npr > 0){
			for(i in 1 : npr){
				prec <- readLines(con , 1) # 
				# make this person's row for the data frame
				person <- makerow(serno , prec , flds)
				# add it to the data frame
				dtf <- rbind(dtf , person)
			}
		}
	}
	return (dtf)
}

# 功能函数-1
# set up this person's row for the data frame
makerow <- function(srn , pr , fl){
	l <- lilst()
	l[["serno"]] <- srn
	for(nm in name(fl)){
		l[["nm"]] <- intextract(pl , fl[[nm]])
	}
	return(l)
}

# 功能函数-2
# extracts an integer field in the string s , in chanracter positions
# rng[i] through rng[2]
intextract <- function(s ,rng){
	fld <- substr(s , rng[1] , rng[2])
	return(as.integer(fld))
}

# 调用主函数
pimsdf <- extractpums("psum" , list(Gender=c(23 , 23) , Age = c( 25, 26)))
# 调用extractpums函数
# 调用psum数据集
# Gende在23列
# Age在25 ，26列
#         注意，这里在列表中制定的名称使我们希望结果数据框里各列具有的名称。我们也可以
# 将其设定为其他名称-如Sex和Ancientness。