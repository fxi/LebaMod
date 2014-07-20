
##########################################################
# Etract radom pseudo absence data set and produce a map
##########################################################

getTrainTestList <- function(jobList,dbInfo,paramList){
	# get presences and corresponding variables,
	# combine with pseudo absences.
	# to do : find better way to name resulting
	# list.
	# s= species, q=quantile, p/pr=pres|pseudo
	require(foreach)
	require(caret)
	require(sp)
	require(doMC)
	registerDoMC(8)
	jL=jobList
	pL=paramList
	u<-sapply(jL,unique)
	gNpa <- function(x,jL){setkey(jL,s);jL[x,nPa][1]$nPa}
	vU <- unlist(u['v']) # variables
	qU <- unlist(u['q']) # quantiles
	sU <- unlist(u['s']) # species
	rU <- unlist(u['r']) # runs


	vS <- strsplit(vU,',')[[1]]
	dbSp<-dbInfo$dbSpPred

	############################## loop on job list  #################################
	foreachRuns<-function(x,y){
		tt <- foreach(q=qU)%do%{
			prS <- foreach(s=sU)%dopar%{ 
				eval(x)
			}
			names(prS)<-sU
			paS<-foreach(p=prS)%dopar%{ # get pseudo abs for each species, each q, each run
				con<-dbConnect(SQLite(),dbSp)
				paR<-foreach(r=rU)%do%{
					eval(y)
				}
				names(paR)<-paste('run',rU)
				dbDisconnect(con)
				paR
			}
			names(paS)<-sU
			paS
		}
		names(tt)<-qU
		return(tt)
	}

	######################  expression to be evaluated in loop  ######################

	foreachRuns(

				# get predictors value for presence set
				expression({
					con<-dbConnect(SQLite(),dbSp)
					pr <- getPrData(s,q,vU,con)
					dbDisconnect(con)
					pr
				}),
				# get predictors value for absence  set
				expression({
					pa <- getPaData(q,vU,gNpa(p$sp,jL),p,con)

					tt<-rbind(p[,c('class',vS)],pa[,c('class',vS)])

					# find correlation and remove if asked	
					corMat <- cor(tt[vS])
					rmVars <- findCorrelation(corMat, pL$corrCutoff)
					vLoCorr <- vS[-rmVars]
					vHiCorr <- vS[rmVars]

					if(isTRUE(pL$corrAutoRemove)){
						tt <- tt[,c('class',vLoCorr)]
					}

					if(isTRUE(pL$pseudoAbsMap)){
						spDatMap<-rbind(pa[,c('x','y','class')],p[,c('x','y','class')])
						coordinates(spDatMap) <- ~x+y
					}else{
						spDatMap=NULL
					}

					ret<-list(
							  spDatPrPa=tt,
							  spDatMap=spDatMap,
							  vHiCorr=vHiCorr,
							  vLoCorr=vLoCorr,
							  vCorMat=corMat
							  )
					ret

				})

				)
}


################################################################################
getPrData<-function(s,q,v,dbCon){
	prDataSql<-paste0(" SELECT sp,x,y,yyyy,",v,
					  " FROM (SELECT DISTINCT sp,x,y,yyyy FROM SPECIES)",
					  " NATURAL JOIN predictors",
					  " WHERE sp='",s,"' AND quant='",q,"'")
	speciesPres <- dbGetQuery(dbCon,prDataSql)
	speciesPres$class<-factor('PRES')
	return(speciesPres)
}

################################################################################
getPaData<-function(q,v,nPa,prData,dbCon){
	mnY <- min(prData$yyyy)
	mxY <- max(prData$yyyy)

	# get pseudos absences
	paDataSql<- paste0("SELECT  x,y,yyyy,",v,
					   " FROM predictors",
					   " NATURAL JOIN",
					   " (SELECT DISTINCT x,y",
					   " FROM predictors",
					   " WHERE quant='",q,"'",
					   " AND YYYY <=",mxY,")",
					   " WHERE yyyy",
					   " BETWEEN ",mnY,
					   " AND ",mxY,
					   " AND quant='",q,"'",
					   " ORDER BY RANDOM()",
					   " LIMIT ",nPa
					   )

	speciesAbs <- dbGetQuery(dbCon,paDataSql)
	speciesAbs$class <- factor('ABS')
	return(speciesAbs)
}



################################################################################




##########################################################
# OLD STUFF
##########################################################



extractTrainTestOld <- function(paramList,dbInfo,inMemory=T,inParallel=T){
	require(RSQLite)
	require(caret)
	require(sp)

	# shortcuts
	idJob <- paramList$idJob
	spList <-dbInfo$speciesList
	dbSpPred <- dbInfo$dbSpPred
	dbMod<- dbInfo$dbModels
	numPa <- paramList$pseudoAbsNum
	spSel <- unlist(paramList$speciesSelect)
	quants <- paramList$quantSelect
	runs <- paste0('run',1:paramList$pseudoAbsRuns)



	dbConMod <- dbConnect(SQLite(),dbMod)



	# train test list structure :
	# species, run, quantile, 
	#						-trainTest
	#						-corMatrix
	#						-corMatrixClean
	#						-varUnCorr
	#						-varHigCorr

	if(inMemory){
		message('copy db in memory')
		dbCon <- dbConnect(SQLite(),dbSpPred)
		dbConMem  <-  dbConnect(SQLite(),':memory:')
		sqliteCopyDatabase(dbCon,dbConMem)
		dbCon <- dbConMem
	}


	foreachTrainTest <- function(x){
		if(FALSE %in% (spSel %in% spList$sp)){
			stop('Some species not found in species list')
		}else{
			require(doMC)
			registerDoMC(8)
			#loop on species and quantiles.
			if(inParallel){
				tt<-foreach(s=spSel)%:%
				foreach(r=runs)%:%
				foreach(q=quants)%dopar%{
					eval(x)
				}
			}else{		
				tt<-foreach(s=spSel)%:%
				foreach(r=runs)%:%
				foreach(q=quants)%do%{
					eval(x)
				}

			}

		}
		tt<-renameList3(tt,list(spSel,runs,quants))
		tt
	}




	aTT <- foreachTrainTest(expression({	
		if(!inMemory){
			# each workers will have a connection.
			dbCon <- gsub('\\s*','',paste0(s,r,q))
			assign(dbCon, dbConnect(SQLite(),dbSpPred))
			dbCon <- get(dbCon)
		}

		message(paste('sp:',s,'quant=',q,'run=',r))

		## 5.6 seconds.
		# get predictors value for selected species 
		sqlSpData=paste0(" SELECT sp,x,y,yyyy,",paste0(paramList$varSelect,collapse=','),
						 " FROM (SELECT DISTINCT sp,x,y,yyyy FROM SPECIES)", 
						 " NATURAL JOIN predictors", 
						 " WHERE sp='",s,"' AND quant='",q,"'")
		speciesPres <- dbGetQuery(dbCon,sqlSpData)

		# test if nrow match previous count with another method. 
		nPres = nrow(speciesPres)
		nPresDb=spList[spList$sp==s,'nDistinctSite']

		#message(paste('sp:',s,'nSel=',nPres))
		#message(paste('sp:',s,'nDb=',nPresDb))

		if(!identical(nPres,nPresDb)){
			stop('species site number differing from db')
		}
		if(is.null(numPa)){
			numPa=3*nPres
			message('No pseudo absence number given, set to default: 3*nPres')
		}

		# get pseudos absences
		sqlPseudoAbs <- paste0("SELECT  x,y,yyyy,",paste0(paramList$varSelect,collapse=','),
							   " FROM predictors",
							   " NATURAL JOIN",
							   " (SELECT DISTINCT x,y", 
							   " FROM predictors",
							   " WHERE quant='",q,"'",
							   " AND YYYY <=",dbInfo$speciesYearsRange$max,")",
							   " WHERE yyyy", 
							   " BETWEEN ",dbInfo$speciesYearsRange$min,
							   " AND ",dbInfo$speciesYearsRange$max,
							   " AND quant='",paramList$quantSelect,"'",
							   " ORDER BY RANDOM()",
							   " LIMIT ",numPa
							   )

		speciesAbs <- dbGetQuery(dbCon,sqlPseudoAbs)

		speciesPres$class <- as.factor('PRES')
		speciesAbs$class <- as.factor('ABS')


		# create data frame of training and testing values
		trainTestData <- rbind(speciesPres[,c('class',paramList$varSelect)],speciesAbs[,c('class',paramList$varSelect)])	

		# removing High correllated variables, cutoff defined by paramList$corrCutoff on
		# varSelect
		varCorOrig <- cor(trainTestData[paramList$varSelect])
		findCor <- findCorrelation(varCorOrig, paramList$corrCutoff)
		varUnCorrSelect <- paramList$varSelect[-findCor]
		varHighCorrSelect <- paramList$varSelect[findCor]
		trainTestData<-trainTestData[,c('class',varUnCorrSelect)]
		varCorClean <- cor(trainTestData[,varUnCorrSelect])

		if(TRUE){
			# create simple map of pseudo abs and presence data
			coordinates(speciesAbs) <- ~x+y
			coordinates(speciesPres) <- ~x+y
			#coordinates(speciesPres) <- ~xOrig+yOrig
			pseudoAbsMap=rbind(speciesPres[,c('class')],speciesAbs[,c('class')])
		}

		if(!inMemory)dbDisconnect(dbCon)
		lTT <- list(
					pseudoAbsMap=pseudoAbsMap,
					trainTest=trainTestData,
					corMatrixOrig=varCorOrig,
					corMatrixClean=varCorClean,
					varUnCorr=varUnCorrSelect,
					varHighCorr=varHighCorrSelect
					)

	}))
	if(inMemory)dbDisconnect(dbCon)

	aTT$idJob <- idJob
	return(aTT)
}


