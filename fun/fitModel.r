##########################################################
# function to create test and train data, fit the model 
# and evaluate results
##########################################################

# to do : 
# change try_default function to get usefull error message. 
# add journal of all fit model 

fitModel <- function(dbInfo,jobList,trainTestList,runAppend=F){
	require(caret)
	require(doMC)
	registerDoMC(8)
	require(pROC)
	require(e1071)
	require(RSQLite)
	require(RSQLite.extfuns)
	library(data.table)
	library(digest)
	library(plyr)

	# split job in list as input for foreach
	jL <- split(jobList,row.names(jobList))
	dL <- dbInfo
	tt <- trainTestList

	# check if db is available and contain models table
	dbCon <- dbConnect(SQLite(),dbInfo$dbModels)
	dbTabOk <- 'models' %in% dbListTables(dbCon)
	dbDisconnect(dbCon)


	# get data for proabilities. 9 sec for read 4.4 million row, 14 cols.

	dbCon <- dbConnect(SQLite(),dbInfo$dbSpPred)
	init_extensions(dbCon)
	vars<-unique(unlist(strsplit(jobList$v,',')))
	varsCollapse<-paste0(vars,collapse=',')

	if(T){
		avgVarList <- paste0('median(',vars,')',vars,collapse=',')
		indexCols<-c('x','y','idHex','dem','bassinID','YYYY','quant')
		# with aggregation by idHex, we select mode of bassin ID and median for dem.
		indexColsSql<-c('(xHex)x','(yHex)y','idHex','median(dem)dem','mode(bassinID)bassinID','YYYY','quant')
		indexColsCollapse<-paste(indexColsSql,collapse=',')
		hexSql <- paste0(" SELECT ",indexColsCollapse,",",avgVarList,
						 " FROM predictors NATURAL JOIN idHex",
						# " WHERE YYYY BETWEEN ",yearMin," AND ",yearMax," AND ",
						# " dem BETWEEN ",dMin," AND ",dMax,
						 " GROUP BY YYYY,quant,idHex")


		unkX <- data.table(na.omit(dbGetQuery(dbCon,hexSql)))

	}else{
		indexCols<-c('x','y','dem','YYYY','quant','bassinID')
		indexColsCollapse<-paste0(indexCols,collapse=',')
		pointsSql <- paste0('SELECT ', indexColsCollapse,',',varsCollapse,' FROM predictors')
		unkX <- data.table(dbGetQuery(dbCon,pointsSql))
	}
	setkey(unkX,quant)
	dbDisconnect(dbCon)
	
	# loop trough elements of jobList
	foreachFitModel <- function(x){
		require(doMC)
		registerDoMC(8)
		foreach(j=jL,.combine='rbind')%dopar%{
			with(j,eval(x))
		}
	}

	# expression to be evaluated from loop.
	fitSummaries<-foreachFitModel(expression({	

		modMsg <- paste('s=',s,'r=',r,'q=',q,'m=',m,'idRun=',idRun,'idJob=',idJob)
		message("Compute model for:",modMsg)
		## subsetting test train with species, runs, hydro quantiles ...
		ttSub <- tt[[q]][[s]][[r]]
		spDat <- ttSub$spDatPrPa

		#get vars names
		varLo <- ttSub$vLoCorr
		varHi <- ttSub$vHiCorr
		varOri <- strsplit(v,',')[[1]]

		#get vars length
		nVarLo<-length(varLo)
		nVarHi<-length(varHi)
		nVarOri<-length(varOri)

		# set unix timestamp. To convert in YYYY/MM/DD : 
		# as.POSIXct(1399917126, origin='1970/1/1')
		

		# unique con name for each worker. 
		uConMod <- digest(c(idRun,timeStamp,'mod'))
		assign(uConMod, dbConnect(SQLite(),dL$dbModels))

		# begin caret work
		inTrain <- createDataPartition(y = spDat$class,
									   p = .90,
									   list = FALSE)

		training <- spDat[ inTrain,]
		testing  <- spDat[-inTrain,]


		# custom summary indices used to determine best fited model.
		customControl <- trainControl(method="repeatedcv",
									  number=10,
									  repeats=2,
									  p=0.80,
									  returnData=TRUE,
									  returnResamp="final",
									  savePredictions=FALSE,
									  classProbs=TRUE,
									  summaryFunction=customSummary,
									  selectionFunction="best",
									  allowParallel=FALSE) #avoid parallel inside another parallel loop.. 

		# fit model with error handling. Return F if failed.
		fit <- try_default(train(class ~.,
								 data=training,
								 method=m,
								 importance = TRUE, # not an option ??
								 trControl=customControl, # as separate function
								 metric="TSS",
								 tuneLength=10,
								 preProc=c("center","scale")),
						   default=list('F'),quiet=T)

		if(fit[[1]]=='F'){
			message(paste('line125. Model failed',modMsg))
			# if model failed, write minimal infos in DB
			modelSummary <- data.frame(idRun=idRun,
									   idJob=idJob,
									   fail=T,
									   species=s,
									   method=m,
									   quant=q,
									   tss=NA,
									   kappa=NA,
									   sensitivity=NA,
									   specificity=NA,
									   nPr=nPr,
									   nPa=nPa,
									   nVarOri=nVarOri,
									   nVarLo=nVarLo,
									   nVarHi=nVarHi,
									   varMod=paste0(varLo,collapse=','),
									   fileName=NA,
									   row.names=NULL
									   )

			stopifnot(nrow(modelSummary)==1)
			dbWriteTable(get(uConMod),'models',modelSummary,row.names=F,append=T)
			modelSummary
		}else{
			# if model succeded, extract evaluation with best model. 
			probTest <- extractProb(list(fit), testX=testing[,varLo],testY=testing$class)
			print(summary(probTest))
			evalMod<-customSummary(probTest, lev=c('PRES','ABS'))
			print(evalMod)

			#probs on whole set. Large data set !
			probAll <- data.table(extractProb(list(fit),unkX=unkX[J(q),varLo,with=F],unkOnly=T))
			probAll<-probAll[,"PRES",with=F]
			
			# column bind probs AND value.
			if(T){
				probAll<-cbind(probAll,unkX[J(q),c(indexCols,varLo),with=F])
			}
			## set summary info
			tss=evalMod['TSS']
			kap=evalMod['Kappa']
			sens=evalMod['Sens']
			spec=evalMod['Spec']
			# create unique filename
			fileName =gsub('\\s+','_',paste(s,m,q,r,'tss',round(tss*100)/100,timeStamp,collapse='_'))
			bestTune=paste0(fit$bestTune,collapse=';')
			fit$probTestData <- cbind(probTest,rbind(training,testing))
			fit$probAllPoints <- probAll
			ttSub$trainTest <- NULL
			modelData <- updateList(fit,ttSub)

			#saveRDS(modelData,file.path(dbInfo$dbModelsFiles,fileName),compress='bzip2')
			saveRDS(modelData,file.path(dbInfo$dbModelsFiles,fileName))

			modelSummary <- data.frame(idRun=idRun,
									   idJob=idJob,
									   fail=F,
									   species=s,
									   method=m,
									   quant=q,
									   tss=tss,
									   kappa=kap,
									   sensitivity=sens,
									   specificity=spec,
									   nPr=nPr,
									   nPa=nPa,
									   nVarOri=nVarOri,
									   nVarLo=nVarLo,
									   nVarHi=nVarHi,
									   varMod=paste0(varLo,collapse=','),
									   fileName=fileName,
									   row.names=NULL
									   )

			stopifnot(nrow(modelSummary)==1)
			dbWriteTable(get(uConMod),'models',modelSummary,row.names=F,append=T)
			modelSummary
		}

		dbDisconnect(get(uConMod))
		modelSummary

	}))

}



fitModelOld <- function(paramList,dbInfo,trainTestList,runAppend=F){
	require(caret)
	require(doMC)
	registerDoMC(8)
	require(pROC)
	require(e1071)
	require(RSQLite())
	#	library(latticeExtra)
	library(data.table)
	library(digest)
	#	library(reshape2)

	# get model's input for loop
	spSel <- unlist(paramList$speciesSelect)
	quants <- paramList$quantSelect
	runs <- 1:paramList$pseudoAbsRuns
	met <- paramList$methodSelect
	tt <- trainTestList
	idJob <- tt$idJob

	# check that we are in lebaMap and create directories
	stopifnot(getwd()==dbInfo$projectLocal | getwd()==dbInfo$projectRemote) 
	dir.create(dbInfo$dbModelsFiles,recursive=T,showWarnings=F)

	# check if db is available and contain models table
	dbCon <- dbConnect(SQLite(),dbInfo$dbModels)
	dbTabOk <- 'models' %in% dbListTables(dbCon)
	dbDisconnect(dbCon)

	# parallel nested loop (multiple loop considered as one) 
	# from wich expression is evaluated
	foreachFitModel <- function(x){
		require(doMC)
		registerDoMC(8)
		# foreach species, methods, runs, quantiles, do...
		# without worring about errors
		fm<-foreach(s=spSel)%:%
		foreach(m=met)%:%
		foreach(q=quants)%:%
		#foreach(q=quants,.errorhandling=c('pass'))%dopar%{
		foreach(r=runs)%dopar%{
			eval(x)
		}
	}

	# expression to be evaluated from loop.
	foreachFitModel(expression({	

		modMsg <- paste(' s=',s,' r=',r,' q=',q,' m=',m)
		message("Compute model for:",modMsg)
		## subsetting test train with species, runs, hydro quantiles ...
		ttSub <- tt[[s]][[r]][[q]]
		spDat <- ttSub$trainTest
		vars <- ttSub$varUnCorr
		nPres <- nrow(spDat[spDat$class=='PRES',])
		nAbs <- nrow(spDat[spDat$class=='ABS',])
		#predictors list
		nVarUnCorr=length(ttSub$varUnCorr)
		nVarSelect=length(paramList$varSelect)
		varUnCorr <- paste0(vars,collapse=';')
		varSelect=paste0(paramList$varSelect,collapse=';')
		nRuns <- paramList$pseudoAbsRuns

		r=as.integer(r)
		# set unix timestamp. To convert in YYYY/MM/DD : 
		# as.POSIXct(1399917126, origin='1970/1/1')
		timeStamp <- round(as.numeric(Sys.time()))

		# set runID id based on md5 of unique string that doesn't depend on run number.
		idRun<- digest(c(s,m,q,nPres,nAbs,vars))

		# unique con name for each worker. 
		uCon <- digest(c(s,m,r,q,vars))
		assign(uCon, dbConnect(SQLite(),dbInfo$dbModels))


		if(dbTabOk){
			sqlCmd <- paste0(" SELECT COUNT(*) FROM models",
							 " WHERE idRun='",idRun,"'",
							 " AND idJob<",idJob)
			idRunCount <- dbGetQuery(get(uCon),sqlCmd)		
		}else{
			idRunCount=0
		}

		if(idRunCount>=nRuns & runAppend==F ){
			message(modMsg,": Identical parameters found on ",idRunCount,
					" runs. If you want to add more runs,",
					"set 'runAppends=T'")
		}else{
			inTrain <- createDataPartition(y = spDat$class,
										   p = .90,
										   list = FALSE)

			training <- spDat[ inTrain,]
			testing  <- spDat[-inTrain,]


			# custom summary indices used to determine best fited model.
			customControl <- trainControl(method="repeatedcv",
										  number=10,
										  repeats=2,
										  p=0.80,
										  returnData=TRUE,
										  returnResamp="final",
										  savePredictions=FALSE,
										  classProbs=TRUE,
										  summaryFunction=customSummary,
										  selectionFunction="best",
										  allowParallel=FALSE) #avoid parallel inside another parallel loop.. 

			fit <- try_default(train(class ~.,
									 data=training,
									 method=m,
									 importance = TRUE,
									 trControl=customControl, # as separate function
									 metric="TSS",
									 tuneLength=10,
									 preProc=c("center","scale")),
							   default=list('F'),quiet=T)



			if(fit[[1]]=='F'){
				message(paste('line125. Model failed',' s=',s,' r=',r,' q=',q,' m=',m))
				# if model failed, write minimal infos in DB
				modelSummary <- data.frame(idRun=idRun,
										   idJob=idJob,
										   fail=T,
										   species=s,
										   method=m,
										   quant=q,
										   tss=NA,
										   kappa=NA,
										   sensitivity=NA,
										   specificity=NA,
										   nPres=NA,
										   nAbs=NA,
										   nVarSelect=nVarSelect,
										   nVarUnCorr=nVarUnCorr,
										   varSelect=varSelect,
										   varUnCorr=varUnCorr,
										   fileName=NA,
										   row.names=NULL
										   )
				dbWriteTable(get(uCon),'models',modelSummary,row.names=F,append=T)

			}else{
				# if model succeded, extract evaluation with best model. 
				prob <- extractProb(list(fit), testX=testing[,vars],testY=testing$class)
				evalMod<-customSummary(prob, lev=c('PRES','ABS'))
				## set summary info
				tss=evalMod['TSS']
				kap=evalMod['Kappa']
				sens=evalMod['Sens']
				spec=evalMod['Spec']
				# create unique filename
				fileName =gsub('\\s+','_',paste(s,m,q,r,'tss',round(tss*100)/100,timeStamp,collapse='_'))
				bestTune=paste0(fit$bestTune,collapse=';')
				fit$probTestData <- cbind(prob,rbind(training,testing))
				ttSub$trainTest <- NULL
				modelData <- updateList(fit,ttSub)
				saveRDS(modelData,file.path(dbInfo$dbModelsFiles,fileName))

				modelSummary <- data.frame(idRun=idRun,
										   idJob=idJob,
										   fail=F,
										   species=s,
										   method=m,
										   quant=q,
										   tss=tss,
										   kappa=kap,
										   sensitivity=sens,
										   specificity=spec,
										   nPres=nPres,
										   nAbs=nAbs,
										   nVarSelect=nVarSelect,
										   nVarUnCorr=nVarUnCorr,
										   varSelect=varSelect,
										   varUnCorr=varUnCorr,
										   fileName=fileName,
										   row.names=NULL
										   )
				dbWriteTable(get(uCon),'models',modelSummary,row.names=F,append=T)
			}
		}
		dbDisconnect(get(uCon))
	}))

}


## info


#dbCri

#parameters for 

# tt[['Species']][[]][[]]
#train test list structure :
# species, run, quantile, 
#						-trainTest
#						-corMatrix
#						-corMatrixClean
#						-varUnCorr
#						-varHigCorr
#				#tryMaxTimes(summaryModWrite,3,0.1)
#	idRunCount <- tryCatch(dbGetQuery(dbCon,sqlCmd),
#								   error=function(cond){
#									   message('failed to read idRunExp in sqlite:')
#									   message(cond)
#									   return(0)
#								   },
#								   warning=function(cond){
#									   print('warning in idRunExp')
#								   } 
#								   )
#	sqlCmd <- paste0("SELECT COUNT(*) FROM models WHERE idRun='",idRun,"'")
#			idRunCount <- tryCatch(dbGetQuery(dbCon,sqlCmd),
#								   error=function(cond){
#									   message('failed to read idRunExp in sqlite:')
#									   message(cond)
#									   return(0)
#								   },
#								   warning=function(cond){
#									   print('warning in idRunExp')
#								   } 
#								   )
#
