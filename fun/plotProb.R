# set of fonction to query model db and models files

# EXEMPLE
#

if(FALSE){
  testFilter<-filterResults(idJob=17,dbOut='../data/base/dbModels.db')
  testList<-getEnsembleListSpecies(resultsTable=testFilter,
                                   modelsLoc='../data/models/')
  
  testPresProb<-presProbSpecies(testList) 
}





filterResults<-function(species=NULL,method=NULL,group=NULL,predictors=NULL,tss=NULL,idJob=NULL,idRun=NULL,email=NULL,showFailed=FALSE,dbOut){
  # Filter sqlite table produced by lebaMod/fun/computePending.r.
  # If failed is selected, show only failed model. Else, if any argument is set, make a query on corresponding column.
  # all argument can be single value or vectors.
  # return : data.table with every columns, filtered by args.
  require(RSQLite)
  require(data.table)
  
  dbCon<-dbConnect(SQLite(),dbOut)
  if(!'models' %in% dbListTables(dbCon)){
    NULL
  }else{
    #
    if(showFailed){
      res<-dbGetQuery(dbCon, paste("SELECT * FROM models where fail=",as.integer(showFailed)))
      
    }else{
      if(!is.null(method)) mtd<-paste0("method in ('",paste0(method,collapse="','"),"')")
      if(!is.null(species)) sps<-paste0("species in ('",paste0(species,collapse="','"),"')")
      if(!is.null(group)) grp<-paste0("[group] in ('",paste0(group,collapse="','"),"')")
      if(!is.null(tss)) tsv<-paste0("tss between ",min(tss)," AND ", max(tss))
      if(!is.null(predictors)) prd<-paste0("prdLoCorr glob '*",predictors,"*'",collapse=' AND ')
      if(!is.null(email))if(!email=='') eml<-paste0("email in ('",paste0(email,collapse="','"),"')")
      if(!is.null(idJob))if(!idJob=='') idj<-paste0("idJob in ('",paste0(idJob,collapse="','"),"')")
      if(!is.null(idRun))if(!idRun=='') idr<-paste0("idRun in ('",paste0(idRun,collapse="','"),"')")
      
      
      lim<-paste0("limit ",1000)
      sqlCmd<-paste("SELECT * FROM models WHERE",
                    if(exists('mtd')){paste(mtd,"AND")},
                    if(exists('sps')){paste(sps,"AND")},
                    if(exists('grp')){paste(grp,"AND")},
                    if(exists('tsv')){paste(tsv,"AND")},
                    if(exists('prd')){paste(prd,"AND")},
                    if(exists('idj')){paste(idj,"AND")},
                    if(exists('idr')){paste(idr,"AND")},
                    if(exists('eml')){paste(eml,"AND")},
                    paste('fail=0'),
                    lim)
      res<-data.table(dbGetQuery(dbCon,sqlCmd))
      
    }
   
   
    dbDisconnect(dbCon) 
    class(res)<-c(class(res),'lebaResultsTable')
    res
  }
}



modelDelete<-function(resultsTable,dbOut,modelsLoc){
  # remove model and corresponding files.
  require(RSQLite)
  dbCon<-dbConnect(SQLite(),dbOut)
  
  if(!'models' %in% dbListTables(dbCon)){
    NULL }else{
      
      
      stopifnot('lebaResultsTable' %in% class(resultsTable))
      
      fL<-resultsTable$fileName
      id<-resultsTable$id
      idSql<-paste0("(",paste(id,collapse=','),")",collapse='') 
      file.remove(paste(file.path(modelsLoc,fL)))
      dbGetQuery(dbCon,paste('DELETE from models where id in',idSql))
      dbDisconnect(dbCon)
      message(length(jobIdList)," models deleted.")
    }
}

getEnsembleListSpecies<-function(resultsTable,modelsLoc){
  # produce an ensemble list by species from folder data/models :
  # from a result table produced by function 'filterResults' get file content and
  # append to a list named by species.
  stopifnot('lebaResultsTable' %in% class(resultsTable))
  
  fL<-resultsTable$fileName
  spList<-resultsTable$species
  mL<-foreach(f=fL)%do%{
    readRDS(file.path(modelsLoc,f))
  }
  names(mL)<-spList
  class(mL)<-c(class(mL),'lebaEnsembleListSpecies')
  mL
}



presProbSpecies<-function(lebaEnsembleListSpecies){
  # From a lebaEnsembleList, produce table of presence probabilities.

  require(doMC)
  require(foreach)
  registerDoMC(8)
  stopifnot('lebaEnsembleListSpecies' %in% class(lebaEnsembleListSpecies))
  # names of lebaEnsembleList are run identifiant
  spList<-lebaEnsembleListSpecies
  spNames<-unique(names(spList))
  
  # foreach runs groups found in lebaEnsemble list, get model subsets.
  pred<-foreach(spN=spNames)%do%{
    modSubset<-spList[spNames %in% spN]
    # for each models in group, get prediction
    speciesData<-foreach(m=modSubset,.combine='c')%dopar%{
      #message(m$lebaData$idMod,';',m$lebaData$idRun)
      predSp<-data.table(m$lebaData$dataPredictions)
      idMod<-m$lebaData$idMod
      idRun<-m$lebaData$idRun
      predSp[,idMod:=factor(idMod)]
      predSp<-list(pred=predSp)
      names(predSp)<-idRun
      predSp
    }
    speciesData
  }
  names(pred)<-spNames
  pred
}





presProbSpeciesByVar<-function(lebaEnsembleListSpecies,varBy='YYYY',rangeX,rangeY,rangeDem,rangeYear){
  # From a lebaEnsembleList, produce a summary table of presence probabilities by varBy.
  # Extent, elevation and years could be filtered.
  require(doMC)
  require(foreach)
  registerDoMC(8)
  stopifnot('lebaEnsembleListSpecies' %in% class(lebaEnsembleListSpecies))
  # names of lebaEnsembleList are run identifiant
  spList<-lebaEnsembleListSpecies
  spNames<-unique(names(spList))
  
  # foreach runs groups found in lebaEnsemble list, get model subsets.
  predMed<-foreach(spN=spNames,.combine='rbind')%do%{
    modSubset<-spList[spNames %in% spN]
    # for each models in group, get prediction
    speciesData<-foreach(m=modSubset,.combine='rbind')%dopar%{
      #message(m$lebaData$idMod,';',m$lebaData$idRun)
      pred<-data.table(m$lebaData$dataPredictions)
      idMod<-m$lebaData$idMod
      idRun<-m$lebaData$idRun
      #idRunString<-paste(m$lebaData[c('species','method','group','idRun')],collapse=';')
      idSpeciesString<-paste(m$lebaData[c('species')],collapse=';')
      #setkey(pred,dem,YYYY,x,y)
      pred<-pred[dem %between% rangeDem]
      pred<-pred[x %between% rangeX ]
      pred<-pred[y %between% rangeY ]
      pred<-pred[YYYY %between% rangeYear]
      predMed<-pred[,list(medPres=median(PRES)),by=c(varBy)]
      predMed[,idSpeciesString:=factor(idSpeciesString)]
      predMed[,idMod:=factor(idMod)]
      predMed[,idRun:=factor(idRun)]
      setkeyv(predMed,c(varBy,'idRun','idMod','idSpeciesString','medPres'))
    }
    
    speciesData
  }  
}


emptyPlotText<-function(infoText){
  par(mar = c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  
  text(x = 0.34, y = 0.9, paste(infoText), 
       cex = 1.5, col = "black", family="serif", font=2, adj=0.5)
}

