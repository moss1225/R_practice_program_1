#         离散事件仿真(Discrete-event simulation, DES)广泛应用于商业、工业和政府。离散
#事件指的是系统状态仅在分散的时间发生变化，而不连续地变化。
#        一个典型的例子是排列系统，比如人们排队使用ATM自动提款机。我们把系统在t时刻的
#状态定义为该时刻的排队人数。当某人加入排队队伍中，状态该变量为+1，而当某人用完ATM
#并离开，状态该变量为-1。这与模拟天气是不同的，因为气候和气压是连续变化的。
#         DES运作的核心问题是维护事件列表，也就是预定事件(scheduled event)的列表。这只
#是DES的一个普通术语，这里所指的列表指的并不是R原因中的数据类型。事实上，我们会以
#数据框的形式表示事件列表。
#         例如在ATM的例子中，在仿真的某个时刻，事件列表看起来大概是如下这样的：
# 客户1 在时刻23.12到达
# 客户2 在时刻25:88到达
# 客户3 在时刻25:97到达
# 客户1 在时刻26.02结束服务
#         因为最早的事件会先被处理，把事件列表以事件顺序储存是最简单的编码形式。(有计
#算专业背景的读者会注意到，用二叉树储存也许会更效率。)这里，我们会把它处理为一个
#数据框，第一行包含最早的事件，第二行包含第二早的事件，一次类推。
#        仿真的主循环反复迭代。每一次迭代把最早的事件从事件列表中取出，更新仿真的事件
#以表示该事件发生，并对该事件的发生做出响应，这通常会造成新的事件发生。例如，一位
#客户到达，队列是空的，那么客户会马上接受服务———这就是一个事件引发另一个事件。我
#们的代码必须要决定用户的服务时间，然后也会知道服务什么时候终止。终止服务也算是一
#个时间，也会加入到事件列表当中。
#        编写DES代码最传统的方法是向事件的范式。这里，处理一个事件发生的代码直接触发另
#一个事件，这与上述讨论一致。
#        用ATM的例子理一下思路。在0时刻，队列为空。仿真的代码随机生成一个客户到达
#时间，例如2.3。这个时候，事件列表就是（2.3，“arrival”）。这个事件从列表中取出来，仿
#真时间更新为2.3，对客户到达这一时间做出如下相应：
#	*自动取款机的队列为空，于是服务开始。我们随机生成服务时间，比如1.2，那么服务会
#	在2.3+1.2=3.5时刻结束。
#  *我们把服务事件结束加入到事件列表，于是事件列表包含(3.5 , "service done")。
#	*我们也声称下一个客户的到达时间，比如0.6，也就是下一个客户会在2.9时刻到达。现在
#	时间列表包含(2.9,"arrival")和(3.5,"service done")。
#此代码包含一个普通适用的库。我们也有个应用示例，可以仿真M/M/1排队系统。M/M/1
#排队系统是一个单服务排队系统，它的间隔时间和服务时间都符从指数分布。

#以下是库函数的总结：
# schedevnt():把事件列表中插入一个新事件。
# getnextevent():把最早的事件从事件列表中取出。
# dosim():包含仿真的主循环。重复调用getnextevent()得到最早的处理事件；更新当
#	前仿真时间，sim$currtime，以表示事件的发生；调用特定应用函数reactevent()以处理
#  这个新发生事件。

#代码使用以下特定应用函数：
# initglbls(): 初始化特定应用全局变量。
# reactevent(): 在事件发生采取恰当的操作，一般是生成新的事件。
# prntrslts(): 输出仿真的特定应用结束。

#        要注意，特定应用的意思是说，initglbls()、reactevent()和prntrslts()都是由程序员针
#对特定需求编写的，它们都会传递给dosim()当参数。在这里的M/M/1排队模型例子中，这些
#函数的名称为mm1intglbls、mm1reactevnt()和mm1prntrslts().。于是相应地，dosim()定义如下：
#	dosim <- function(initglbls , reactevnt , prntrslts , maxsimtime , apppars = NULL , dbg =FALSE){}
# 调用像是如下所示：
#	dosim(mm1initglbs , mm1reactevent , mm1prntrslts , 10000.0 , list(arrvrate = 0.5 , srvrate = 1.0) )

#下面是库的代码
#*******************************************************************************************#
# DES.R: R routines for discrete-event simulation (DES)
# each event will be represented by a data frame row consisting of the
# following components : eventtime(evnttm), the time the event is to occur;
# eventtype(evntty) , a character string for the programmer-defined event type;
# optional application-specific components, e.g.
# the job's arravial time in aqueuing app

# a global list named "sim" holds the events data frame , evnts , and
# current simulated time , currentime ; there is also a component dbg , which
# indicates debugging mode

# forms a row for an event of type evntty that will occur at time
# evnttm ; see comments in schedevnt () regarding appin
evntrow <- function(evnttm , evntty , appin = NULL){
	print("测试 evntrow 70行")
	#↑#将事件时间evnttm、类型evntty、时间/特性appin组成数据框返回。
	rw <- c(list(evmtime = evnttm , evnttype = evntty) , appin)
	print(as.data.frame(rw))
	print("74")
	return(as.data.frame(rw))
}

# insert event with time evnttm and type evntty into event list;
# appin is an optional set of application-specific traits of this event,
# specified in the form a list with named components
schedevnt <- function(evnttm , evntty , appin = NULL){
	print("测试 schedevnt 81行")
	newevnt <- evntrow(evnttm , evntty , appin)
	#↓# if the event list is empty , set it to consist of evnt and return
	print("测试 schedevnt 84行")
	if(is.null(sim$evnts)){
		sim$evnts <<- newevnt
		return()
	}
	#↓# otherwise , find insertion point
	inspt <- binsearch((sim$evnts)$evnttime,evnttm)
	### now "insert , " by reconstructing the data frame ; we find what
	### portion of the current martrix should come before the new event and
	### what portion should come after it , then string everything together
	before <- if(inspt == 1) NULL else sim$evnts[1 : (inspt-1) , ]
	nr <- nrow(sim$evnts)
	after <- if(inspt <- nr)sim$evnts[ input:nr , ] else NULL
	sim$evnts <<-rbind(before , newevnt , after)
}

# binary(二次元) search of insertion point of y in the sorted vector x ; returns
# the position in x before which y should be inserted , with the value
# length(x)+1 if y is large than x[xlength(x)] ; could be changes to C
# code for efficiency
binsearch <- function(x , y){
	n <- length(x)
	lo <- 1
	hi <- n
	while(lo + 1 < hi){
		mid <- floor((lo+hi)/2)
		if(y == x[mid]) return(mid)
		if(y < x[mid]){
			hi <-mid 
		}else { lo <- mid}
	}
	if(y<=x[lo])return(lo)
	if(y< x[hi]) return(hi)
	return(hi+1)
}

# start to process next event (second half done by appliction
# programmer via call to reactevnt())
getnextevnt <-function(){
	print("测试 getnextevnt")
	head <- sim$evnts[1, ]
	#delete head
	if(nrow(sim$evnts) == 1){
		sim$evnts <<- NULL
	}else sim$evnts <<- sim$evnts[-1, ]
	return(head)
}

# simulation(模仿，模拟) body(主函数)
# arguments(原型):
#	intitglbls : application-specific initialization function ; inits
#					globals to statistical totals for the app , etc.;records apppars
#					in globals ; schedules the first event ;
#	reactevnt : appliction-specific event handling funxtion , coding the 
#					proper action for each type of event.
#	prntrslts : list of appliction-specific results , e.g.mean queue wait.
# 	apppars : list of application-specific parameters , e.g. number of servers
#					in a queuing app
#	maxsimtime : simulation will be run until this simulated time
#	dbg : debug flag ; if ture , sim will be printed after each event
dosim <- function(initglbls , reactevnt , prntrslts , maxsimtime , apppars=NULL , dbg = FALSE){
	sim <<- list()
	sim$currtime <<- 0.0
	sim$evnts<<- NULL
	sim$dbg <<- dbg
	initglbls(apppars)
	while(sim$currtime < maxsimtime){
		head<-getnextevnt()
		sim$currtime <<- head$evnttime #upgrade current simulated time
		reactevnt(head)# process this event
		if(dbg)print(sim)
	}
	prntrslts()
}

# 以下是前面这段代码的一个应用示例。再次说明一下，仿真的是M/M/1排列系统。
# M/M/1排列系统是一个单服务排队系统，它的服务时间和任务间隔时间都服从指数分布。
# 通过调用库的代码，完成事件处理。
#*******************************************************************************************#
# DES application: M/M/1 queue , arrival rate 0.5 , service rate 1.0 
# the call
# dosim (mm1initglbls , mm1reactevnt , mm1prntrslts , 10000.0,
# list(arrvrate=0.5 , srvrate=1.0))
# should return a value of about 2 ( may talk a while )

# initializes(初始化) global variables specific to this app
mm1initglbls <- function(apppars){
	print("测试 mm1initglbls 170")
	mm1glbls <<- list()
	#↑#创建全局变量
	mm1glbls$arrvrate<<-apppars$apppars
	mm1glbls$srvrate<<-apppars$srvrate
	#↑#server queue , consisting of arrival time of queued jobs
	mm1glbls$srvq<<-vector(length=0)
	mm1glbls$njobdone<<-0
	mm1glbls$towait<<-0.0
	# set up first event , an arrival ; the application-specific data for
	# each event will consist of its arrival time , which we need to
	# record in order to later calculate the job`s residence(驻留) time
	# in the system
	#???
	arrvtime <- rexp(1 , mm1glbls$arrvrate)
	schedevnt(arrvtime , "arrv" , list(arrvtime=arrvtime))
}

# application-specific event processing function called by dosim()
# in the general DES library
mm1reactevnt <- function(head){
	print("测试 mm1reactevnt 191")
	if(head$evnttype=="arrv"){ #arrival
		# if server free , start service , else add a queue ( added to queue
		# even if empty , for convinience )
		if(length(mm1glbls$srvq)==0){
			print("测试 mm1reactevnt if 196")
			mm1glbls$srvq <<- head$arrvtime
			srvdonetime <- sim$currtime + rexp(1 , mm1glbls$srvrate)
			print(srvdonetime)	
			print("测试 mm1reactevnt if 1 working 200")
			schedevnt(srvdonetime , "srvdone" , list(arrvtime=head$arrvtime))
			print("测试 mm1reactevnt if 1 end 202")
		}else{
			print("function mm1reactevnt else 2")
			mm1glbls$srvq <<- c(mm1glbls$srvq , head$arrvtime)	
			# generate next arrival
			arrvtime <- sim$currtime + rexp(1 , head$arrvate)
			schedevnt(arrvtime , "arrv" , list(arrvtime=arrvtime))
		}
	}else{ #service done
		# process job that just finished
		# do accounting
		mm1glbls$njobsdone <<- mm1glbls$njobsdone+1
		mm1glbls$totwait <<- mm1glbls$totwait + mm1glbls$currtime - head$arrvate
		# remove from queue
		mm1glbls$srvq<<-mm1glbls$srvq[-1]
		#more still in queue?
		if(length(mm1glbls$srvq)>0){
			#schedule new service
			srvdonetime <- sim$currtime + rexp(1 , mm1glbls$srvrate)
			schedevnt(srvdonetime , "srvdone" , list(arrvtime=mm1glbls$srvq[1]))
		}
	}
}

mm1prntrslts <- function(){
	print("mean wait:")
	print(mm1glbls$totwait/mm1glbls$njobsdone)
}

 dosim (mm1initglbls , mm1reactevnt , mm1prntrslts , 10000.0 , list(arrvrate=0.5 , srvrate= 1.0 ))