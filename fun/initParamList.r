

initParamList <- function(varSelect=NULL,
						  groupSelect=NULL,
						  speciesSelect=NULL,
						  methodSelect=NULL,
						  corrCutoff=NULL,
						  #corrAutoRemove=NULL,
						  hexProbExtract=NULL,
						  probHexRadius=NULL,
              pseudoAbsType=NULL,
						  pseudoAbsNum=NULL,
						  pseudoAbsMult=NULL,
						  pseudoAbsRuns=1,
						  pseudoAbsMap=NULL,
						  avoidDuplicatedRuns=TRUE,
						  sendEmail=TRUE,
						  email="-"
						  ){
  # initParam : collect and control values, set default. 
  # do not allow less than 3 predictors
  if(length(varSelect)<3)varSelect=NULL
  
  # if any null in environment, return null,else list.
  checkEnv<-as.list(environment())

  if(any(TRUE %in% lapply(checkEnv,is.null))){
    message('initParamList found nulls in args.')
    return(NULL)
  }else{
    message('initParamList ok to set a new job. Content of input list: ')
    print(str(checkEnv))
    
	list(
		 varSelect=as.list(varSelect[order(varSelect)]),
		 groupSelect=as.list(groupSelect),
		 speciesSelect=as.list(speciesSelect),
		 methodSelect=as.list(methodSelect),
		 corrCutoff=corrCutoff,
		 #corrAutoRemove=corrAutoRemove,
		 hexProbExtract=hexProbExtract,
		 probHexRadius=probHexRadius,
		 pseudoAbsType=pseudoAbsType,
		 pseudoAbsNum=pseudoAbsNum,
		 pseudoAbsMult=pseudoAbsMult,
		 pseudoAbsRuns=pseudoAbsRuns,
		 pseudoAbsMap=pseudoAbsMap,
		 avoidDuplicatedRuns=avoidDuplicatedRuns,
		 email=ifelse(isTRUE(sendEmail),email,"")
		 )
  }
}


setId <- function(dbOut,table){
  # function setId : increment id based on rows count
  # value : idMax+1 or 0 if no table found
	require(RSQLite)
	# check if db is available and contain models table
	dbCon <- dbConnect(SQLite(),dbOut)
	dbTabOk <- table %in% dbListTables(dbCon)
	if(dbTabOk){
	  dbRowOk<- dbGetQuery(dbCon,paste('SELECT COUNT(*) FROM',table))>0
	  if(dbRowOk){
	    sqlCmd <- paste("SELECT max(id) FROM",table)
	    idMax <- dbGetQuery(dbCon,sqlCmd)+1
	  }else{
	    idMax=0
	  }
	}else{
	  idMax=0
	}
	
  idMax<-as.integer(idMax)
	dbWriteTable(dbCon,table,data.frame(id=idMax,time=Sys.time()),row.names=F,append=T)
	dbDisconnect(dbCon)
	#return(list(idJob=as.integer(idJobMax)))
  return(idMax)
}




getPaNum<-function(sp,dbInfo,nPa,mPa,paType){
  # get number of pseudo absence, based on number of pa or multiplicator of pa
	spDt <- data.table(dbInfo$speciesList)
	setkey(spDt,sp)
	dS <- spDt[sp]$nDistinctSite
	if(paType=='mPa'){
    nPa <- dS*mPa
    }else{
    nPa
	}
	return(as.integer(nPa))
}



getPrNum<-function(sp,dbInfo){
  # get number of distinct site by species
	spDt <- data.table(dbInfo$speciesList)
	setkey(spDt,sp)
	nPr<-spDt[sp]$nDistinctSite
	return(as.integer(nPr))
}



initJobTable <- function(paramList,dbInfo,computeModels=F){
  # convert parameters to data.table where each row represent a model parameters set
  # value : a list of job to be evaluated
  # to do : check why unlist is used here.
	require(data.table)
  require(RSQLite)
  require(foreach)
	require(digest)
	lMet <- unlist(paramList$methodSelect)
	lSp <- unlist(paramList$speciesSelect)
	lSpDb <- unlist(dbInfo$speciesList)
	lGroup <- unlist(paramList$groupSelect)
	predictors <- paste0(unlist(paramList$varSelect),collapse=',')
	nRuns <- paramList$pseudoAbsRuns
	nPa <- paramList$pseudoAbsNum
	mPa <- paramList$pseudoAbsMult
  paType<-paramList$pseudoAbsType

	# test if species exists
	if(!all(lSp %in% lSpDb)){
    stop("Error in set job. Selected species in parameters doesn't exists in data base.")
	}
  # expand combinaison of species, method and group. Add predictors. Add others parameters. 
	jobTable <- data.table(expand.grid(s=lSp,m=lMet,g=lGroup,stringsAsFactors=F),p=predictors)
	jobTable[,nPa:=getPaNum(s,dbInfo,nPa,mPa,paType),by=s]
	jobTable[,nPr:=getPrNum(s,dbInfo),by=s]
	jobTable[,idRun:=paste0('R',digest(c(s,m,g,p,nPa,nPr))),by=names(jobTable)]
  jobTable[,'corrCutoff':=paramList$corrCutoff,with=F]
  jobTable[,probHexRadius:=paramList$probHexRadius]
  jobTable[,email:=paramList$email]
  
	#set runs. All parameters multiplied by number of runs. Only runs change.
	jobTable <- foreach(rn=1:nRuns,.combine='rbind')%do%{
		assign(paste0('j',rn),jobTable[,r:=rn])
	}

	# check working path and create directories
	#stopifnot(getwd()==dbInfo$projectLocal | getwd()==dbInfo$projectRemote) 
	dir.create(dbInfo$pathList$models,recursive=T,showWarnings=F)
	
	if(paramList$avoidDuplicatedRuns){
		  message('Duplicated job will be deleted')
	}
  
	#browser()
	setkey(jobTable,s,m,g)
	return(jobTable)
}


writeJobTableDb<-function(jobTable, dbInfo){
  # check for duplicate in finished and pending jobs.
  
  require(RSQLite)
  require(data.table)
  dbOut<-dbInfo$pathList$dbOut
  dbCon <- dbConnect(SQLite(),dbOut)

  idRunJob <- paste(unique(jobTable$idRun),collapse="','")
  
  if('jobsPending' %in% dbListTables(dbCon)){
    sqlCmd <- paste('SELECT idRun FROM jobsPending WHERE idRun in',paste0("('",idRunJob,"')"))
    idRuns<-dbGetQuery(dbCon,sqlCmd)$idRun
    print(idRuns)
    idRunDup<- unique(idRuns)
    jobTable <- subset(jobTable,!idRun %in% idRunDup)
  }
  
  if('models' %in% dbListTables(dbCon)){
    sqlCmd <- paste('SELECT idRun FROM models WHERE idRun in',paste0("('",idRunJob,"')"))
    idRunDup<- unique(dbGetQuery(dbCon,sqlCmd)$idRun)
    jobTable <- subset(jobTable,!idRun %in% idRunDup)
  }
  idJob<-setId(dbOut,'idJobs')
  jobTable[,'idJob':=idJob,with=F]
  #jobTable[,'dbSp':=dbInfo$pathList$dbIn,with=F]
  #jobTable[,'dbMd':=dbInfo$pathList$dbOut,with=F]
  #jobTable[,'dbMdF':=dbInfo$pathList$models,with=F]
  # unique id instead of incremetial ?? lot of space lost ?
  #jobTable[,idRow:=1:nrow(jobTable)]
  jobTable[,id:=setId(dbOut,'idModels'),by=list(rownames(jobTable))] 

  #jobTable[,'email':=mail,with=F]
  #jobTable[,hexGridTable:=paste0('hexGrid',jobTable$probHexRadius[1])]
  
  if(nrow(jobTable)>0){
    dbWriteTable(dbCon,'jobsPending',jobTable,append=T,row.names=F)
  }else{
    message('After removing duplicates, no job remains.')
  }
  
  dbDisconnect(dbCon)
NULL
}


