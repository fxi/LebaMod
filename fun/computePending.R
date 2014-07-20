
#   test<-evalTimeout({
#     while(TRUE){
#       print(date())
#       Sys.sleep(1)
#     }
#   },timeout=2)
# 


evalTimeout<-function(...,timeout,envir=parent.frame()){
  # simplified version of R.utils::evalWithTimeout
  message('Time out set at ',timeout,' seconds.')
  stopifnot(is.numeric(timeout))
  setTimeLimit(cpu = timeout, elapsed = timeout, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  res<-try(eval(...,envir=envir))
}








computePending<-function(dbIn,dbOut,modFiles,ncpus=1,attributions=NULL,timeoutFit=3600){
  # main model computation. It's a wrapper around caret, so it's basically a wrapper to hundred
  # of modelisation method. computePending can be used inside lebaModDaemon to automate process
  # on remote cluster.
  require(digest)
  require(RSQLite)
  require(foreach)
  require(doMC)
  library(sendmailR)
  registerDoMC(ncpus)
  
  
  
  dbConModels<-dbConnect(SQLite(),dbOut)
  # set journalmode (http://www.sqlite.org/pragma.html#pragma_journal_mode)
  #dbGetQuery(dbConModels,'PRAGMA journal_mode=DELETE')
 
  
  if(!dbExistsTable(dbConModels,name='jobsPending')){
    message("No table jobsPending. List of tables :")
    print(dbListTables(dbConModels))
  }else{
    # get all jobs in jobsPending and delete
    jobTable<-dbGetQuery(dbConModels,'select * from jobsPending')
    dbGetQuery(dbConModels,'select * from jobsPending')
    
    message(date(),': length of jobs: ',nrow(jobTable))
    jobs<-split(jobTable,row.names(jobTable))
    dbDisconnect(dbConModels)
    
    
    
    ######### START FOREACH LOOP ###############
    summaryTable<-foreach(j=jobs,.combine='rbind')%dopar%{
      sourceFun('fun')
      #from here, using db Path from jobsPending list
      message(rep('#',options('width')))
      message(date())
      message(paste(j,collapse=';'))
      message(rep('#',options('width')))
      
      # start timer
      tic<-Sys.time()
      
      # set job data as list. At each step, list will be updated
      j<-as.list(j)
      # set short names/extract values
      j$p<-strsplit(j$p,',')[[1]] # variables/predictors  
      
      ######### MAKE TEST TRAIN ###############
      #pr<-extractPresences(j$p,j$g,j$s,j$dbSp)
      pr<-extractPresences(j$p,j$g,j$s,dbIn)
      yMin<-min(pr$YYYY)
      yMax<-max(pr$YYYY)
      #pa<-extractPseudoAbs(j$p,j$g,j$s,j$nPa,j$dbSp,yMin,yMax)
      pa<-extractPseudoAbs(j$p,j$g,j$s,j$nPa,dbIn,yMin,yMax)
      tt<-rbind(pr,pa)
      
      ######### SELECT VARS ##################
      # return a list : testTrainData,corrMatrix,high and low corr predictors names
      #j<-updateList(j,removeCorrPredictors(tt,j$corrCutoff,j$p))
   
      j<-c(j,removeCorrPredictors(tt,j$corrCutoff,j$p))
    
      # if only one predictor remains, create an error

      if(length(j$pLoCorr)<2){
        warning('pLoCorr<2')
        lPh<-length(j$pHiCorr)
        lP<-length(j$p)
        msg<-paste('Warning : length predictors < 2 with corr. cutoff of ',j$corrCutoff,
                   '. Removed:',paste(j$pHiCorr,collapse=','),
                   '. Kept:',paste(j$pLoCorr,collapse=','))
        warning(msg)
        j$caretFit<-msg
        class(j$caretFit)<-'try-error'
      }else{
        ######### CREDIT :) ################
        # from input arg 'credit'.
        j$attributions<-attributions
        append(j$attributions,paste('Contribution:',j$email))
        
        ######### COMPUTE MODEL ################
        modelData<-computeModel(testTrainData=j$testTrainData,
                                summaryFunction=customSummary,
                                tuneLength=10,
                                allowParallel=FALSE,
                                predictors=j$pLoCor,
                                method=j$m,
                                timeoutFit=timeoutFit)
        j<-c(j,modelData)
      }
      
      j$elapsed<-Sys.time()-tic
      
      ######### SAVE MODEL & handle errors ################
      summaryModel<-saveModelProb(j=j,
                                  dbIn=dbIn,
                                  dbOut=dbOut,
                                  modFiles,
                                  summaryFunction=customSummary)
      
      summaryModel
    }
   
#     if(length(summaryTable)>0){
#       dbWriteTable(dbConModels,'models',summaryTable,row.names=F,append=T)
#       idList<-paste0("('",paste0(summaryTable$id,collapse="','"),"')")
#       dbGetQuery(dbConModels,paste0("DELETE FROM jobsPending WHERE id in ",idList))
#     }
    
    return(summaryTable)
    
    
  }
  

}


saveModelProb<-function(j,dbIn,dbOut,modFiles,summaryFunction){
  require(RSQLite)
  require(caret)
  require(data.table)
  
  if('try-error' %in% class(j$caretFit)){
    msg<-paste('Failed after',
               round(as.numeric(j$elapsed)*10)/10,units(j$elapsed),
               'with error:',
               j$caretFit[[1]]
    )
    
    # output to be rbinded and writeed in sqlite model database
    modelSummary <- data.frame(
      species=j$s,
      method=j$m,
      group=j$g,
      id=j$id,
      idRun=j$idRun,
      idJob=j$idJob,
      fail=T,
      message=msg,
      email=j$email, 
      runNum=j$r,
      tss=NA,
      kappa=NA,
      sensitivity=NA,
      specificity=NA,
      nPr=j$nPr,
      nPa=j$nPa,
      prdLoCorr=paste0(j$pLoCorr,collapse=','),
      prdHiCorr=paste0(j$pHiCorr,collapse=','),
      fileName=as.character(NA_character_),
      row.names=NULL
    )
    
    dbC<-dbConnect(SQLite(),dbOut)
    
    writeExpr<-expression(dbWriteTable(dbC,'models',modelSummary,row.names=F,append=T))
    deleteExpr<-expression(dbGetQuery(dbC,paste0("DELETE FROM jobsPending WHERE id='",j$id,"'")))
    
    tryMaxTimes(expr=writeExpr,maxTry=4,delay=2)
    tryMaxTimes(expr=deleteExpr,maxTry=4,delay=2)
    
    dbDisconnect(dbC)
    # keep result to be rbinded by foreach
    modelSummary
    
  }else{
    
    msg<-paste('Model set and fit after',
               round(as.numeric(j$elapsed)*10)/10,units(j$elapsed))
    
    # if model succeded, extract evaluation with best model. 
    probTest <- extractProb(list(j$caretFit), testX=j$testing[,j$pLoCorr],testY=j$testing$class)
    perfSummary<-summaryFunction(probTest, lev=c('PRES','ABS'))
    confusionMat<-confusionMatrix(probTest$pred,probTest$obs,positive='PRES')
    vImp<-t(varImp(j$caretFit)$importance)[1,]

   
    #probs on whole set. Large data set ! x,y,dem,idHex are also extracted
    unkX<-extractHexUnkx(dbIn,j$pLoCorr,j$g)
    probAll <- extractProb( list(j$caretFit),unkX=unkX[j$pLoCorr], unkOnly=T)
    
    # keep only presence probabilies
    probAll<-probAll[c("PRES")]
    
    # column bind probs AND value.
    probAll<-cbind(probAll,unkX[c('x','y','dem','YYYY','quant','idHex',j$pLoCorr)])
    
    ## set summary info
    tss=perfSummary['TSS']
    kap=perfSummary['Kappa']
    sens=perfSummary['Sens']
    spec=perfSummary['Spec']
    
    # create custom filename
    fileName<-paste(
      gsub('\\s+','_',j$s),
      paste0('m','_',j$m),
      paste0('g','_',j$g),
      paste0('r','_',j$r),
      paste0('tss','_',round(tss*100)/100),
      paste0('nPr','_',j$nPr),
      paste0('nPa','_',j$nPa),
      #paste0('d','_',format(Sys.time(), "%d_%m_%Y"),'.rds'),
      paste0('j','_',j$idJob),
      paste0('i','_',j$id,'.rds'),
      sep='-')
    
    
    bestTune=paste0(j$caretFit$bestTune,collapse=';')
    
    ## make list with all results : will be saved to file.
    
    lebaBundle<-list(
      caretFit=j$caretFit,
      lebaData=list(
        idMod=j$id,
        idRun=j$idRun,
        idJob=j$idJob,
        species=j$s,
        method=j$m,
        group=j$g,
        nPseudoAbsences=j$nPa,
        nPresences=j$nPr ,
        probHexRadius=j$probHexRadius,
        attributions=j$attributions,
        email=j$email,
        date=date(),
        timing=j$ellapsed,
        dataTesting=j$testing,
        dataTraining=j$training,
        dataPredictions=probAll,
        perfSummary=perfSummary,
        perfConfusionMatrix=confusionMat,
        predictorsSet=j$p,
        predictorsKept=j$pLoCorr,
        predictorsRemoved=j$pHiCorr,
        predictorsCorrCutoff=j$corrCutoff,
        predictorsCorrMatrix=j$corMat,
        predictorsImportance=vImp
      )
    )
    
    class(lebaBundle)<-'lebaBundle'
    class(lebaBundle$lebaData)<-'lebaData'
    
    saveRDS(lebaBundle,file.path(modFiles,fileName))
    # output to be rbinded and written in sqlite model database
    modelSummary <- data.frame(
      species=j$s,
      method=j$m,
      group=j$g,
      id=j$id,
      idRun=j$idRun,
      idJob=j$idJob,
      fail=F,
      message=msg,
      email=j$email,
      runNum=j$r,
      tss=tss,
      kappa=kap,
      sensitivity=sens,
      specificity=spec,
      nPr=as.integer(j$nPr),
      nPa=as.integer(j$nPa),
      prdLoCorr=paste0( paste0(j$pLoCorr,'(i=',round(vImp),')'),collapse=','),
      prdHiCorr=paste0(j$pHiCorr,collapse=','),
      fileName=as.character(fileName),
      row.names=NULL
    )
    dbC<-dbConnect(SQLite(),dbOut)
    
    writeExpr<-expression(dbWriteTable(dbC,'models',modelSummary,row.names=F,append=T))
    deleteExpr<-expression(dbGetQuery(dbC,paste0("DELETE FROM jobsPending WHERE id='",j$id,"'")))
    
    tryMaxTimes(expr=writeExpr,maxTry=4,delay=2)
    tryMaxTimes(expr=deleteExpr,maxTry=4,delay=2)
    
    dbDisconnect(dbC)
    # keep result to be rbinded by foreach
    modelSummary
  }
}

#dbCon<-dbConnect(SQLite(),j$dbMd)
#dbWriteTable(dbCon,'models',modelSummary,row.names=F,append=T)
#dbGetQuery(dbCon,paste0("DELETE FROM jobsPending WHERE id='",j$id,"'"))
#dbDisconnect(dbCon)

#dbCon<-dbConnect(SQLite(),j$dbMd)
#dbWriteTable(dbCon,'models',modelSummary,row.names=F,append=T)
#dbGetQuery(dbCon,paste0("DELETE FROM jobsPending WHERE id='",j$id,"'"))
#dbDisconnect(dbCon)

extractHexUnkx<-function(dbSp,predictors,group){
  require(RSQLite)
  dbConMd<-dbConnect(SQLite(),dbSp)
  sqlCmd<-paste0("SELECT ",
                 "x,y,dem,YYYY,quant,idHex,",
                 paste0(predictors,collapse=','),
                 " FROM predictorsAggregHex WHERE quant='",group,"'")
  hexExpr<-expression(dbGetQuery(dbConMd,sqlCmd))
  hexDat<-tryMaxTimes(hexExpr)
  dbDisconnect(dbConMd)
  hexDat
}


computeModel<-function(testTrainData,
                       summaryFunction,
                       tuneLength,
                       allowParallel,
                       predictors,
                       method,
                       timeoutFit){
  # compute model
  # testTrainData : presence and pseudo absences with class indication
  # summary function : caret summary function to evaluate ressample
  # tuneLength : length of parameter expand grid evaluation : n^p. exemple :
  #                                                           3 parameters, tunelength=10
  #                                                           3^10 models to evaluate.
  # allowParallel : should caret launch internal parallel foreach loop ?
  #
  # return : caret fit object OR failed 'F'
  
  require(caret)
  ######### DATA PARTITION ##################
  tt <- testTrainData
  
  
  inTrain <- createDataPartition(y = tt$class,
                                 p = .80,
                                 list = FALSE)
  
  training <- tt[ inTrain,c('class',predictors)]
  
  testing  <- tt[-inTrain,c('class',predictors)]
  
  
  ######### CARET SETTINGS  ##################
  # custom summary indices used to determine best fited model.
  customControl <- trainControl(method="repeatedcv",
                                number=10,
                                repeats=2,
                                p=0.80,
                                returnData=TRUE,
                                returnResamp="final",
                                savePredictions=FALSE,
                                classProbs=TRUE,
                                summaryFunction=summaryFunction,
                                selectionFunction="best",
                                allowParallel=FALSE) #avoid parallel inside another parallel loop.. 
  
  
  ######### FIT MODEL  ##################
  # fit model with error handling. Return class "try-error" message = var[[1]] 

  
  #   test<-evalTimeout({
  #     while(TRUE){print(date());Sys.sleep(1)}
  #   },timeout=10)
  
  
  fit<- evalTimeout({
    train(class ~.,
          data=training,
          method=method,
          importance = TRUE, # not an option ??
          trControl=customControl, # as separate function
          metric="TSS",
          tuneLength=tuneLength,
          preProc=c("center","scale"))
  },timeout=timeoutFit)

  
  list(caretFit=fit,
       training=training,
       testing=testing
  )
}


removeCorrPredictors<-function(testTrainData,corrCutoff,predictors){
  # remove correlated predictors at specified cuttoff
  # testTrainData : table of pseudo absence,presence and predictors value
  # corrCutoff : pariwise absolute correlation
  # predictors : vector of predictors names.
  # value : list wiht test train data without highly correlated predictors,
  #         low and high corr predictors names, and correlation matrix.
  require(caret)

#   # test for zero var predictors and remove it
#   predVar<-sapply(pred,var)
#   predZeroVar<-names(predVar[predVar==0])
#   if(length(predZeroVar)>0){
#     pred<-pred[,!predictors %in% predZeroVar]
#     warning(paste(
#       'Zero variance predictor found and removed:',
#       paste(predZeroVar,collapse=',')
#     ))
#   }
  
  corMat <- cor(testTrainData[predictors])
  pRemove <- findCorrelation(corMat,corrCutoff)  
  
  #message('pRemove=',paste(pRemove,collapse=','))
  if(length(pRemove)>0){
    pLoCorr <- predictors[-pRemove]
    pHiCorr <- predictors[pRemove]
  }else{
    pLoCorr<-predictors
    pHiCorr<-character() 
  }

  ttNames<-names(testTrainData)[!(names(testTrainData)) %in% predictors]
  testTrainData <- testTrainData[,c(ttNames,pLoCorr)]

  return(list(testTrainData=testTrainData,
              corMat=corMat,
              pHiCorr=pHiCorr,
              pLoCorr=pLoCorr,
              indexCols=ttNames
  ))
}



extractPresences<-function(predictors,group,species,dbSpPred){
  # selection of species presences uniques locations by year.
  # predictors : vectors of selected predictors names
  # group : predictors group (ex. Quantiles of simulation)
  # species : species name
  # dbSpPred : data base location
  require(RSQLite)
  dbCon<-dbConnect(SQLite(),dbSpPred)
  sqlCmd<-paste0(" SELECT sp,x,y,YYYY,",paste0(predictors,collapse=','),
                 " FROM (SELECT DISTINCT sp,x,y,YYYY FROM SPECIES)",
                 " NATURAL JOIN predictors",
                 " WHERE sp='",species,"' AND quant='",group,"'")
  presences<-tryMaxTimes(expression(dbGetQuery(dbCon,sqlCmd)))
  presences$class<-factor('PRES')
  dbDisconnect(dbCon)
  return(presences)
}

extractPseudoAbs<-function(predictors,group,species,number,dbSpPred,yearMin,yearMax){
  # selection of random pseudo-absences in a specified time range.
  # predictors : vectors of selected predictors names
  # group : predictors group (ex. Quantiles of simulation)
  # number : number of pseudo-absance to generate
  # dbSpPred : data base location
  # yearMin/yearMax time range of random extraction.
  require(RSQLite)
  dbCon<-dbConnect(SQLite(),dbSpPred)
  sqlCmd<- paste0("SELECT  x,y,YYYY,",paste0(predictors,collapse=','),
                  " FROM predictors",
                  " NATURAL JOIN",
                  " (SELECT DISTINCT x,y", 
                  " FROM predictors",
                  " WHERE quant='",group,"'",
                  " AND YYYY <=",yearMax,")",
                  " WHERE YYYY", 
                  " BETWEEN ",yearMin,
                  " AND ",yearMax,
                  " AND quant='",group,"'",
                  " ORDER BY RANDOM()",
                  " LIMIT ",number
  )
  
  pseudoAbs<-tryMaxTimes(expression(dbGetQuery(dbCon,sqlCmd)))
  pseudoAbs$class<-factor('ABS')
  pseudoAbs$sp<-species
  dbDisconnect(dbCon)
  return(pseudoAbs)
}







