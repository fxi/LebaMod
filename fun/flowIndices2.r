
extractHydroIndices <- function(pathToHydroDb, yBegin,yEnd){
	# function to extract yearly calendar hydrological indices, from hourly database by bassins.
	# based on IHA package and SQLite.
	#
	# Args : 
	#	pathToHydroDb: relative path to hourly database.
	#	yBegin : calendar start.
	#	yEnd : calendar end.
	#
	#	Returns : yearly hydrological indices

	require(chron)
	require(zoo)
	require(IHA)
	require(foreach)
	require(data.table)
	require(RSQLite)
	require(RSQLite.extfuns)
	require(doMC)
	require(lattice)
	registerDoMC(8)
	# create chron compatible format. Will be applied to read.zoo, on column yyyy,mm,dd
	toChron <- function(Y,M,D){as.chron(paste(Y,M,D), format = "%Y%m%d")}
	#	if(!quant %in% c('q25','q50','q75')){stop('not valid quantile, use q25, q50, q75')
	#}else{

	yearRange <- paste('YYYY BETWEEN',yBegin,'AND',yEnd)

	conDb <- dbConnect(SQLite(),pathToHydroDb)
	bList=as.numeric(dbGetQuery(conDb,"SELECT DISTINCT bassinID from bassin")[,1])
	bNames=as.numeric(dbGetQuery(conDb,"SELECT DISTINCT bassinName from bassin")[,1])
	qList=c('q25','q50','q75')

	# intersection between selected years and available years
	yList=as.numeric(dbGetQuery(conDb,paste0("SELECT DISTINCT yyyy from dateHourly WHERE ",yearRange))[,1])

	#disconnect connection : each workers will use it's own.
	dbDisconnect(conDb)

	foreach(quant=qList,.combine='rbind')%:%
	foreach(basID=bList[1], .combine='rbind')%do%{
		# each workers will have it's own connection object. 
		conWorker <- paste0('con',basID)
		assign(conWorker,dbConnect(SQLite(),pathToHydroDb))
		# set sqlite math extension, package RSQLite.extfuns
		init_extensions(get(conWorker))
		message(paste('Iha on bassin',basID,'on',length(bList)))
		sqlCmd <- paste0("SELECT yyyy,mm,dd,median(",quant,")medFlow FROM flowHourly NATURAL JOIN dateHourly WHERE bassinID=",basID," AND ",yearRange," group by YYYY,MM,DD")
		dbExt <- dbGetQuery(get(conWorker),sqlCmd)
		dbExtTest <- dbExt
		dbExtTest$did <- 1:nrow(dbExt)
		print(xyplot(medFlow~did,groups=YYYY,dbExtTest,main=paste('Bassin',basID,'.Q=',quant),type='l'))

		z1 <- read.zoo(dbExt,header=TRUE,FUN=toChron,index.column=list(1,2,3) )
		# extract IHA indices for each group
		ihaGrp <- paste('ihaGrp')
		for(i in 1:5){
			#	message(paste('importing group',i,'...'),appendLF=F)
			gr <- paste0('gr',i)
			grFin <- paste0('gr',i-1)
			grFun <- get(paste0('group',i))
			assign(gr,grFun(z1,'calendar'))
			if(class(get(gr))=='matrix'){mat=T}else(mat=F)
			assign(gr, data.table(get(gr),keep.rownames=mat))
			if(mat)setnames(get(gr),'rn','year')
			get(gr)[,year:=as.integer(get(gr)$year)]
			setkey(get(gr),'year')
			if(i>1)assign(gr,merge(get(grFin),get(gr),by='year',all=T))
		}

		# Bug in IHA : if there is a zero flow day during one year, 
		# IHA produce NA for every years and return only one line !
		# So in precedant loop, we took all values and now, we
		# replace all NA by zero to avoid all futur script to fail
		# need investigation to find if only one year produce NA,
		# which will be the impact on others years.

		# remove all na and replace by 0
		removeNaDT = function(DT) {
			for (j in seq_len(ncol(DT)))
				set(DT,which(is.na(DT[[j]])),j,0)
		}

		removeNaDT(gr5)
		simIHA <- gr5

		#simIHA <- gr1[gr2][gr3][gr4][gr5] # ok. update : not ok, see note above

		setnames(simIHA,'year','YYYY')
		simIHA[,bassinID:=as.integer(basID)]
		simIHA[,quant:=quant]
		setkey(simIHA,bassinID,YYYY,quant)
		#print(summaryFast(simIHA))
		#print('NA found in simIHA:')
		#print(simIHA[is.na(November),list(YYYY,bassinID,quant)])

		message('get sim yearly min')

		# get daily min, sqlite version. Aggregated min on daily median by years
		sqlCmd <- paste0(' SELECT YYYY,bassinID,min(mq) yearlyMin',
						 ' FROM (',
						 ' SELECT bassinID,YYYY,median(',quant,') mq',
						 ' FROM flowHourly NATURAL JOIN dateHourly',
						 ' WHERE bassinID=',basID,' AND ',yearRange,
						 ' GROUP BY YYYY,MM,DD) d',
						 ' GROUP BY YYYY')

		simYearlyMin <- data.table(dbGetQuery(get(conWorker),sqlCmd))
								   simYearlyMin[,quant:=quant]
								   setkey(simYearlyMin,bassinID,YYYY,quant)


		message('get sim median daily variability for summer months')
		# get mean variability within days for summer months
		sqlCmd <- paste0("SELECT d.YYYY,d.bassinID, median(d.dq) variaDaySummer FROM (SELECT bassinID,YYYY,DD, max(",quant,") - min(",quant,") dq from flowHourly natural join dateHourly where MM between 6 and 8 and ",yearRange," and bassinID =",basID," GROUP BY YYYY,MM,DD) d GROUP BY d.YYYY")
		simSummerVar <- data.table(dbGetQuery(get(conWorker), sqlCmd))
		simSummerVar[,quant:=quant]
		setkey(simSummerVar,bassinID,YYYY,quant)
		#print(summaryFast(simSummerVar))
		#print('NA found in simSummerVar:')
		#print(simSummerVar[is.na(variaDaySummer),list(YYYY,bassinID,quant)])


		message('get sim median daily variability all months ')
		# get mean variability within days for all month
		sqlCmd <- paste0("SELECT d.YYYY,d.bassinID, median(d.dq) variaDayYear FROM (SELECT bassinID,YYYY,DD, max(",quant,") - min(",quant,") dq from flowHourly natural join dateHourly where bassinID =",basID," and ",yearRange," group by YYYY,MM,DD) d group by d.YYYY")

		simYearVar <- data.table(dbGetQuery(get(conWorker), sqlCmd))
		simYearVar[,quant:=quant]
		setkey(simYearVar,bassinID,YYYY,quant)
		#print('NA found in simYearVar:')
		#print(simSummerVar[is.na(variaDayYear),list(YYYY,bassinID,quant)])

		# disconnect worker connection to db
		dbDisconnect(get(conWorker))

		# join results
		simIHA[simSummerVar][simYearVar][simYearlyMin]
	}
	#}
}




controlMedianValue <- function(bid,year,month,day,qtil,pDb,ihaExt){
	# control : compare output of Iha VS original dataBase. Same value should be found.  
	# Args :
	#	bid : bassin ID to test for median value
	#	year : year where we extract median
	#	month : month of median value
	#	qtil : whitch quantiles, based on stochastic simulations, should be used. Values : 'q25';'q50';'q75'
	#	pDb : path to original hourly database
	#	ihaExt : data.table of indices calculated by IHA
	require(RSQLite)
	conTemp<-dbConnect(SQLite(),pDb)
	valDb <- median(dbGetQuery(conTemp,paste0("select yyyy,mm,dd,avg(",qtil,")aq from flowHourly natural join dateHourly where yyyy=",year," and mm=",month," and bassinID=",bid," group by dd"))[,4])
	setkey(ihaExt,YYYY,quant,bassinID)
	valIHA <- subset(ihaExt[J(year,qtil,bid)], select=month.name[month])
	dbDisconnect(conTemp)
	return(as.numeric(valIHA)-as.numeric(valDb))
}


