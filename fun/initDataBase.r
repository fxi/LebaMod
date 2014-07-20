
getMethodClassProbList<-function(){
  
  # return list of classification method that compute probabilies
  # by class.
  require(caret)
  message("Get model info from caret modelLookup")
  methodList<-data.table(modelLookup())
  setkey(methodList,forClass,probModel)
  methodList<-methodList[J(TRUE,TRUE)]
  unique(methodList$model)
}


getMethodInfoShort<-function(methods){
  
  # for each method, get caret model info as list
  require(caret)
  require(foreach)
  message('Get model info (short) from caret')
  methInfo<-foreach(u=methods)%do%{
    modInf<-getModelInfo(u)[[u]][c('tags','library','parameters','label')]
    tags=modInf$tags
    lib=modInf$library
    param=modInf$parameters
    lab=modInf$label
    list(label=lab,tags=tags,library=lib,parameters=param)
  }
  names(methInfo)<-methods
  methInfo
}

getMethodInfoTable<-function(methods){
  
  # foreach method, get model info as data.table.
  require(caret)
  require(foreach)
  require(shiny)
  methTable<-foreach(m=methods,.combine='rbind')%do%{
    modInf<-getModelInfo(m)[[m]][c('tags','library','parameters','label')]
    method=m
    tags=paste0(modInf$tags,collapse=',')
    lab=paste0(modInf$label,collapse=',')
    lib=paste0(paste0("<a href='http://cran.r-project.org/web/packages/",modInf$library,"/' target='_blank'>",modInf$library,"</a>"),collapse=',')
    data.table(method=m,label=lab,tags=tags,library=lib)
  }
  methTable
}

extractSpeciesSummary<-function(con){
  require(reshape2)
  sqlCmd<-"select * from species natural join predictors"
  idVars<-c('sp','quant')
  spAllDat<-data.table(dbGetQuery(con,sqlCmd))
  spAllDat[,YYYY:=as.numeric(YYYY)]
  spAllMelt<-melt(spAllDat,id.vars=idVars)
  q0<-function(v){quantile(v,probs=0)}
  q25<-function(v){quantile(v,probs=.25)}
  q50<-function(v){quantile(v,probs=.50)}
  q75<-function(v){quantile(v,probs=.75)}
  q100<-function(v){quantile(v,probs=1)}
  spSummary<-spAllMelt[,list(
    n=.N,
    min=q0(value),
    q25=q25(value),
    q50=q50(value),
    q75=q75(value),
    max=q100(value),
    sd=sd(value),
    mean=mean(value)
  ),by=c('sp','variable','quant')]
  setnames(spSummary,'quant','group')
  setkey(spSummary,'sp','variable','group')
  spSummary
}



extractYearlySummary<-function(con,vList){
  # For all points, variable in vList calc median, lower, upper quartile.
  # indepdendant on all groups (quantile by bassins)
  # Give a spatial summary.
  # value : melted data.table with year,quartile,value,variable 
  require(RSQLite.extfuns)
  require(reshape2)
  require(data.table)
  init_extensions(con)
  require(foreach)
  require(doMC)
  registerDoMC(3)
  # aggregate function.
  q50vListSql <- paste0('median(',vList,')',vList,collapse=',')
  q25vListSql <- paste0('lower_quartile(',vList,')',vList,collapse=',')
  q75vListSql <- paste0('upper_quartile(',vList,')',vList,collapse=',')
  
  lSql<-list(q50=q50vListSql,q25=q25vListSql,q75=q75vListSql)
  datAggreg<-foreach(l=lSql,.combine='rbind')%do%{
    sqlCmd<-paste("select yyyy,",l," from predictors group by YYYY")
    aggregData <- data.table(dbGetQuery(con,sqlCmd))
    aggregData[,quartile:=strsplit(x=l,split='\\(')[[1]][1]]
  }
  
  datMelt<-melt(datAggreg,id.vars=c('YYYY','quartile'))
  setkey(datMelt,'YYYY','quartile','variable')
  return(datMelt)
}

xyplotYearlySummary <-function(yearlySummaryDT,predictors){
  require(lattice)
  require(data.table)
  setkey(yearlySummaryDT,variable)
  typLine=ifelse(length(predictors)>5,'l','b') 
  xyplot(value~YYYY|variable,group=quartile,yearlySummaryDT[J(predictors)],
         type=typLine,
         scales=list(y=list(rel='free')),
         #layout=c(1,length(vList)),
         auto.key=T,
         par.strip.text=list(cex=1)
         
  )
}


listToHtml<-function(listInput,htL='',h=2){
  hS<-paste0('<H',h,'><u>',collapse='')
  hE<-paste0('</u></H',h,'>',collapse='')
  h=h+1
  if(is.list(listInput)){
    nL<-names(listInput)
    htL<-append(htL,'<ul>')
    for(n in nL){
      #htL<-append(htL,c('<li>',n,'</li>'))
      htL<-append(htL,c(hS,n,hE))
      subL<-listInput[[n]]
      htL<-listToHtml(subL,htL=htL,h=h)
    }
    htL<-append(htL,'</ul>')
  }else{
    htL<-append(htL,c('<li>',paste(listInput,collapse=','),'</li>'))
  }
  return(paste(htL,collapse=''))
}



initDataBase <- function(pathList,clustersList,idxSpecies,idxPredictors,overwriteInfoFile=F,crsOrig,crsDest){
  ##########################################################
  # Initialise database and get metrics and variables names
  ##########################################################
  pL<-pathList
  
  #generate dbInfoFile path and yearly predictor summary.
  if(!file.exists(pL$dbIn)){
    stop('Data not found. Please verify config.r file.')
  }
  
  dbCache <- file.path(pL$cache, paste0(sub("[.][^.]*$",'',basename(pL$dbIn)),'.rds'))
  dbYearlySummary<-file.path(pL$cache,'dbYearlySummary.rds')
  dbSpeciesSummary<-file.path(pL$cache,'dbSpeciesSummary.rds')
  
  pL$dbCache=dbCache
  pL$dbYearlySummary=dbYearlySummary
  
  if(file.exists(dbCache) & !overwriteInfoFile ){
    
    message('Configuration already done. Skipping.',
            'Set overwriteInfoFile=T to delete cache file.')
    
    return(readRDS(dbCache))
  }else{
    require(RSQLite)
    require(RSQLite.extfuns)
    require(FNN)
    require(raster) # only for measure of extent. Should be replaced
    require(data.table)
    # connect database 
    dbCon <- dbConnect(SQLite(),pL$dbIn)
    #dbCon <- dbConnect(SQLite(),':memory:')
    #sqliteCopyDatabase(dbConFile, dbCon)
    
    init_extensions(dbCon)
    # expected table. 
    expectedTables <- c('predictors','species')
    # convert vector to string
    idxSpeciesSql=paste0(idxSpecies,collapse=',')
    idxPredictorsSql=paste0(idxPredictors,collapse=',')
    
    # Test if expected tables exist
    if(FALSE %in% dbExistsTable(dbCon,expectedTables)){
      stop('Tables species or predictors not found')
    }else{
      message('Required tables exist')
      
      # create index if needed.
      createIndexOnTable(dbCon,'species',idxSpeciesSql)
      createIndexOnTable(dbCon,'predictors',idxPredictorsSql)
      
      #clean erratic values (should be done before) ok.
      #dbGetQuery(dbCon,"DELETE FROM predictors WHERE dem=0")
      
      # set database metrics
      speciesYearsRange <- dbGetQuery(dbCon,paste0(
        "SELECT min(yyyy) min,max(yyyy) max ",
        "FROM species"
      ))
      
      predictorsYearsRange <- dbGetQuery(dbCon,paste0(
        "SELECT min(yyyy) min ,max(yyyy) max ",
        "FROM predictors"
      ))
      
      predictorsSpatialRange <- extent(as.numeric(
        dbGetQuery(dbCon,paste0( 
          "SELECT min(x),max(x),min(y),max(y) ",
          "FROM predictors"
        ))))
      
      speciesSpatialRange <- extent(as.numeric(
        dbGetQuery(dbCon,paste0(
          "SELECT min(x),max(x),min(y),max(y) ",
          "FROM species"
        ))))
      
      predictorsDemRange  <- dbGetQuery(dbCon,paste0(
        "SELECT min(dem)min,max(dem)min ",
        "FROM predictors"
      ))
      
      predictorsNrows <- dbGetQuery(dbCon,paste0(
        "SELECT count(*) ",
        "FROM predictors"
      ))
      
      # test if extend of species is smaller than predictors
      if(speciesSpatialRange>predictorsSpatialRange){
        message(paste(
          "The species spatial extent should be smaller",
          "than predictor's. Some data will be lost or inaccurate. "
        ))}
      
      # center species obs to the nearest point using KNN algorithm
      speciesColNames <- dbGetQuery(dbCon,
                                    'PRAGMA table_info(species)'
      )$name
      
      
      if(!'xOrig' %in% speciesColNames){
        predXY <- dbGetQuery(dbCon,"SELECT DISTINCT x,y FROM predictors")
        sp <- dbGetQuery(dbCon,"SELECT * FROM species")	
        dbGetQuery(dbCon,"DROP INDEX idx_species")
        spXY <- sp[,c('x','y')]
        spCentered <- predXY[get.knnx(predXY, spXY, k=1)$nn.index,c('x','y')]
        #use data.table setnames..
        setnames(sp, old=c('x','y'),new=c('xOrig','yOrig'))
        sp<-cbind(sp,spCentered)
        dbGetQuery(dbCon,"DROP TABLE species")
        dbWriteTable(dbCon,'species',sp,row.names=F)
        createIndexOnTable(dbCon,'species',idxSpeciesSql)
        message('Species data coord have been updated ',
                'with position of nearest predictors available.')
      }else{
        message('Species table seems to have already been centered')}
      speciesColNames <- dbGetQuery(dbCon,'PRAGMA table_info(species)')$name
      
      # get columns names of predictors, exclude year and coordinates
      predictorsColNames <- dbGetQuery(dbCon,'PRAGMA table_info(predictors)')$name
      predictorsList <- predictorsColNames[!predictorsColNames %in% idxPredictors]
      predictorsQuant <- dbGetQuery(dbCon,'SELECT DISTINCT quant FROM predictors')$quant
      
      # here we can set species list
      speciesList <- dbGetQuery(dbCon,paste0(
        " SELECT sp,count(sp)nDistinctSite",
        " FROM (SELECT DISTINCT sp,x,y,yyyy from species)",
        " GROUP BY sp"
      )) 
      
      
      if(file.exists(dbYearlySummary)){
        message("Predictors yearly summary cache file exist : loading.")
        predictorsYearlySummary<-readRDS(dbYearlySummary)
      }else{
        message('calc Yearly summary.Take a while.')
        predictorsYearlySummary<-extractYearlySummary(dbCon,predictorsList)
        saveRDS(predictorsYearlySummary,dbYearlySummary)
      }
      
      if(file.exists(dbSpeciesSummary)){
        message("Species summary cache file exist : loading.")
        speciesSummaryByAll<-readRDS(dbSpeciesSummary)
      }else{
        message('calc species summary.Take a while.')
        speciesSummaryByAll<-extractSpeciesSummary(dbCon)
        saveRDS(speciesSummaryByAll,dbSpeciesSummary)
      } 
      
      
      param <- list(
        clustersList=clustersList,
        pathList=pL,
        #expectedTables=expectedTables,
        crsOrig=crsOrig,
        crsDest=crsDest,
        speciesYearsRange=speciesYearsRange,
        predictorsYearsRange=predictorsYearsRange,
        predictorsDemRange=predictorsDemRange,
        predictorsNrows=predictorsNrows,
        predictorsQuant=predictorsQuant,
        predictorsYearlySummary=predictorsYearlySummary,
        speciesSpatialRange=speciesSpatialRange,
        speciesSpatialRange=speciesSpatialRange,
        speciesSummaryByAll=speciesSummaryByAll,
        predictorsSpatialRange=predictorsSpatialRange,
        speciesColNames=speciesColNames,
        predictorsColNames=predictorsColNames,
        idxSpecies=idxSpecies,
        idxPredictors=idxPredictors,
        #varList=varList,
        speciesList=speciesList,
        predictorsList=predictorsList,
        methodList=getMethodClassProbList(),
        methodSummary=getMethodInfoShort(getMethodClassProbList()),
        methodTable=getMethodInfoTable(getMethodClassProbList()),
        hexRadius=5000 
      )
      
      #param <- updateList(param,pathList)
      
      saveRDS(param,dbCache)
      dbDisconnect(dbCon)
      return(param)

    }}}

