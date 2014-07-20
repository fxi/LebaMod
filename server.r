################################################################################
#
# LebaMod
# Laboratoire d'écologie et biologie aquatique, Universté de Genève
# Species presence only modeling and analysis application
#
# moser.frederic@gmail.com
# 2014
#
#
################################################################################



source('../data/config/configLebaMod.r')

# set global variables
spL<<-dbInfo$speciesList
prL<<-dbInfo$predictorsList
meL<<-dbInfo$methodList
grL<<-dbInfo$predictorsQuant #quant=group..
clL<<-names(dbInfo$clustersList)
# range
yearR<<-dbInfo$predictorsYearsRange
demR<<-dbInfo$predictorsDemRange
latR<<-c(dbInfo$predictorsSpatialRange@ymin,dbInfo$predictorsSpatialRange@ymax)
lonR<<-c(dbInfo$predictorsSpatialRange@xmin,dbInfo$predictorsSpatialRange@xmax)

paramList=list()
jobTable=data.table()
subModelTable=data.table()

shinyServer(function(input, output,session) {
  
  output$script<-renderUI({
    singleton(tagList(
      tags$body(tags$script("console.log('Hello. In case of fire, break glass. Or mail me at moser dot frederic at gmail dot com')")),
      tags$body(tags$script(src = 'js/leaflet.js')),
      tags$body(tags$script(src = "js/leaflet-providers.js")),
      tags$body(tags$script(src = "js/leaflet.markercluster.js")),
      tags$body(tags$script(src = "js/utils.js")),
      tags$body(tags$link(rel="stylesheet", href="css/leaflet.css" )),
      tags$body(tags$link(rel="stylesheet", href="css/MarkerCluster.css" )),
      tags$body(tags$link(rel="stylesheet", href="css/map.css" )),
      tags$body(tags$link(rel="stylesheet", href="css/MarkerCluster.Default.css" )),
      tags$body(tags$link(rel="stylesheet", href="css/htmlExportStyleSheetSimple.css" ))
    ))
  })
  
  ####################################################################
  #
  #  RESULTS
  #
  ####################################################################
  
  #####################
  #                   #
  # FEED PARAM LIST   #
  #                   #
  #####################
  
  messageLebaMod<-function(message=''){
    output$messageLebaMod <- renderUI({helpText(paste('INFO: ',message))})
  }
  
  messageLebaModResult<-function(message=''){
    output$messageLebaModResult <- renderUI({helpText(paste('INFO: ',message))})
  }
  
  messageLebaMod('Welcome')
  
  # ------------------{ species choice }------------------ #
  
  observe({
    selAllSp=input$selectAllSpecies
    if(isTRUE(selAllSp)){
      updateSelectInput(session,"selectSpecies",selected=spL$sp)
    }else{
      updateSelectInput(session,"selectSpecies",selected="")
    }
    
  })
  
  
  observe({
    spRange <- input$speciesSitesRange
    if(!is.null(spRange)){
      spRange<-min(spRange):max(spRange)
      spSite<-spL$nDistinctSite %in% spRange
      spList <- spL[spSite,'sp']
      updateSelectInput(session, "selectSpecies", choices=spList)
      updateCheckboxInput(session, "selectAllSpecies",value=FALSE)
    }
  })
  
  
  
  
  # ------------------{ predictors choice }------------------ #
  
  
  
  output$selectAllPredictors<-renderUI({
    checkboxInput("selectAllPredictors","Select all predictors",value=FALSE)
  })
  
  observe({
    selAllPred=input$selectAllPredictors
    if(isTRUE(selAllPred)){
      updateSelectInput(session,"selectPredictors",selected=prL)
    }else{
      updateSelectInput(session,"selectPredictors",selected="") 
    }
    
  })
  
  output$selectPredictors <- renderUI({
    selectInput("selectPredictors", "Select predictors (n min=3)",
                choices=prL,
                selected='variaDaySummer',
                multiple=T
    )
  })
  
  # ------------------{ group choice }------------------ #
  
  output$selectGroups<- renderUI({ 
    selectInput("selectGroups", "Select hydrological predictors group/quantiles",
                choices=grL,
                selected='q50',
                multiple=T
    )
  })
  
  
  # ------------------{ run choice }------------------ #
  
  output$selectRuns<-renderUI({
    sliderInput("selectRuns",
                "Number of runs (same model parameters with different set of pseudo-absences)",
                min=1,
                max=10,
                value=c(1),
                step=1
    )
  })
  
  
  # ------------------{ methode choice }------------------ #
  output$selectMethods <- renderUI({ 
    selectInput("selectMethods", "Select methods",
                choices=meL,
                selected='rf',
                multiple=T
    )
  })
  
  
  ##########################
  #                        #
  # REACTIVE DATA SUMARY   #
  #                        #
  ##########################
  
  # ------------------{ species map and predictor summary }------------------ #
  
  output$map<-renderUI({
    initMapFromDb(db=dbInfo$pathList$dbIn,
                  table='species',
                  idMap='spPresMap',
                  provider='Thunderforest.Landscape',
                  crsOrig=dbInfo$crsOrig,
                  crsDest=dbInfo$crsDest,
                  zoom=9)
  })
  
  output$speciesMap<-renderUI({
    markersClusterFromDb(
      db=dbInfo$pathList$dbIn,
      table='species',
      idMap='spPresMap',
      subset=input$selectSpecies,
      crsOrig=dbInfo$crsOrig,
      crsDest=dbInfo$crsDest)
  })
  
  
  output$speciesSummary<-renderTable({
    handleNullDataTable(
      summarySpeciesFromDbInfo(dbInfo,
                               includeSpatialVars=input$includeSpatialVars,
                               species=input$selectSpecies,
                               predictors=input$selectPredictors,
                               group=input$selectGroups)
      
    )
    
  })
  
  # ------------------{ predictors q1/2/3 time serie }------------------ #
  
  output$describePredictors<-renderPlot({
    if(!is.null(input$selectPredictors)){
      print(
        xyplotYearlySummary(
          yearlySummaryDT=dbInfo$predictorsYearlySummary,
          c(input$selectPredictors))
      )  
    }
  },height=500)
  
  
  # ------------------{ methods meta data }------------------ #
  
  output$describeMethods<-renderUI({
    #input$summaryMods
    HTML(listToHtml(dbInfo$methodSummary[input$selectMethods],h=4))
    
  })
  output$describeMethodsTable<-renderTable({
    methodTable<-dbInfo$methodTable
    setkey(methodTable,'method')
    if(!is.null(input$selectMethods)){
      methodTable[J(input$selectMethods)]
    }else{NULL}
  }, sanitize.text.function = function(x) x)  
  
  
  
  #################################################################################
  #
  # init parameter list and configure job List
  #
  #################################################################################
  
  #   observe({
  #     
  #    
  #       
  #     if(computeModels){
  #       updateSelectInput(session,"selectPredictors",selected="")
  #       updateSelectInput(session,"selectMethods",selected="")
  #       updateSelectInput(session,"selectSpecies",selected="")
  #     }  
  #   })
  
  
  # ------------------{ parameter list init. }------------------ #
  
  observe({
    # initParam : collect and control values, set default, etc.. 
    # if any null, return null,else list.
    paramList<-initParamList(
      varSelect=input$selectPredictors,
      groupSelect=input$selectGroups,
      speciesSelect=input$selectSpecies,
      methodSelect=input$selectMethods,
      corrCutoff=input$corrCutOff,
      #corrAutoRemove=TRUE,
      hexProbExtract=TRUE,
      probHexRadius=5000,
      pseudoAbsType=input$paType,
      pseudoAbsNum=input$nPa,
      pseudoAbsMult=input$mPa,
      pseudoAbsRuns=input$selectRuns,
      pseudoAbsMap=TRUE,
      avoidDuplicatedRuns=TRUE,
      #sendEmail=input$sendEmail,
      sendEmail=TRUE,
      email=input$email
    )
    
    
    if(!is.null(paramList)){
      messageLebaMod("Jobs added to job set. Press 'compute models' to submit.")
      jobTable <<- initJobTable(paramList,dbInfo,computeModels)
      jobTablePublic<<- jobTable
      jobTablePublic$email<-NULL
      
      if(paramList$hexProbExtract){
        # Set new table in db and keep polygons in dbInfo
        dbInfo <- updateDbHexGrid(paramList,dbinfo, overWrite=F)
      }
      
    }else{
      messageLebaMod('Please complete form')
      jobTable<<-data.table()
      jobTablePublic<<-data.table()
    }  
    # ------------------{ Display temp job table }------------------ #
    output$jobsTable<-renderTable({
      handleNullDataTable(jobTablePublic)
    },sanitize.text.function=formatCommasTable)   
  })
  
  # ------------------{ Set sqlite pending list }------------------ #
  
  observe({
    computeModels<-input$computeModels
    
    isolate(
      if(computeModels>0){
        messageLebaMod('Jobs added to pending list. LebaMod will compute it as soon as possible')
        
        if(nrow(jobTable)<1){
          messageLebaMod('No new and distinct job found. Complete all fields in form, check for identical models. ')
        }else{
          # write jobs for jobObserver and set jobTable to null
          jobTable[]<<-writeJobTableDb(jobTable,dbInfo)
          
        }
      }      
    )
  })
  
  
  # ------------------{ Display pending jobs table }------------------ #
  
  
  output$jobsPending<-renderTable({
    dbCon<-dbConnect(SQLite(),dbInfo$pathList$dbOut)
    if(dbExistsTable(dbCon,'jobsPEnding')){
      jobsPending<-dbGetQuery(dbCon, 'select idJob,s,m,g,p,nPa,nPr,idRun,corrCutoff,probHexRadius,r from jobsPending')
    }else{
      jobsPending<-data.frame()
    }
    dbDisconnect(dbCon)
    invalidateLater(2000, session)
    handleNullDataTable(jobsPending)
  },sanitize.text.function=formatCommasTable) 
  
  # ------------------{ Display jobs done table }------------------ #
  
  
  output$jobsDone<-renderTable({
    dbCon<-dbConnect(SQLite(),dbInfo$pathList$dbOut)
    if(dbExistsTable(dbCon,'models')){
      showFail=input$showFailed
      jobDone<-dbGetQuery(dbCon, paste0("select idJob,runNum,species,method,[group],nPa,nPr,prdLoCorr,prdHiCorr,tss,message from models where fail=",input$showFail))
    }else{
      jobDone<-data.frame()
    }
    dbDisconnect(dbCon)
    invalidateLater(2000, session)
    handleNullDataTable(jobDone)
  },sanitize.text.function=formatCommasTable)
  
  
  ####################################################################
  #
  #  RESULTS
  #
  ####################################################################
  

  
  
  
  #####################
  #                   #
  # GET LAST dbMODEL  #
  #                   #
  #####################
  
  observe({
    invalidateLater(session=session,millis=10000)
    allModelTable<<-filterResults(
      dbOut=dbInfo$pathList$dbOut,
      showFailed=input$showFailed)
  })
  
  
  #####################
  #                   #
  # FEED FILTERS BAR  #
  #                   #
  #####################
  
  # model filter
  # test if models are set in db.
  dbCon<-dbConnect(SQLite(),dbInfo$pathList$dbOut)
  if(!'models' %in% dbListTables(dbCon) ){
    dbDisconnect(dbCon)
    messageLebaModResult(' Warning, no models table found. Please set at last one model.')
  }else{
    nMods<-dbGetQuery(dbCon,'SELECT COUNT(*) FROM models')
    messageLebaModResult(paste(nMods,' models found in data base. '))
    dbDisconnect(dbCon)
  
  output$filterSpecies <- renderUI({ 
    selectInput("filterSpecies", "Species",
                choices=unique(allModelTable$species),
                selected='',
                multiple=T
    )
  })
  
  output$filterPredictor <- renderUI({ 
    predList<-sub("\\((.*?)\\)",'',unique(unlist(strsplit(allModelTable$prdLoCorr,','))))
    selectInput("filterPredictor", "Predictors",
                choices=predList,
                selected=,
                multiple=T
    )
  })
  
  output$filterMethod <- renderUI({ 
    selectInput("filterMethod", "Methods",
                choices=unique(allModelTable$method),
                selected='',
                multiple=T
    )
  })
  output$filterGroup <- renderUI({ 
    selectInput("filterGroup", "Groups",
                choices=unique(allModelTable$group),
                selected='',
                multiple=T
    )
  })
  
  output$filterTss<-renderUI({
    mn<-floor(min(allModelTable$tss)*100)/100
    mx<-ceiling(max(allModelTable$tss)*100)/100
    sliderInput('filterTss',label='TSS range',
                min=mn,
                max=mx,
                step=0.01,
                value=c(0.9,0.99))
  })
  
  # predictor filter
  output$filterDem<-renderUI({
    sliderInput('demRange',label='Dem range',
                min=min(demR),
                max=max(demR),
                step=10,
                value=c(min(demR),max(demR)))
  })
  output$filterLong<-renderUI({
    sliderInput(
      'longRange',label='Longitude range',
      min=min(lonR),
      max=max(lonR),
      step=1000,
      value=c(min(lonR),max(lonR))
    )
  })
  output$filterLat<-renderUI({
    sliderInput(
      'latRange',label='Latitude range',
      min=min(latR),
      max=max(latR),
      step=1000,
      value=c(min(latR),max(latR))
    )
  })
  output$filterYear<-renderUI({
    sliderInput(
      'yearRange',label='Year range',
      min=min(yearR),
      max=max(yearR),
      step=1,
      value=c(min(yearR),max(yearR))
    )
  })
  
  
  output$subModelTable<-renderTable({

    subModelTable<<-filterResults(species=input$filterSpecies,
                                  method=input$filterMethod,
                                  group=input$filterGroup,
                                  predictors=input$filterPredictor,
                                  tss=input$filterTss,
                                  idJob=input$filterJob,
                                  idRun=input$filterRun,
                                  email=input$filterEmail,
                                  showFailed=input$showFailed,
                                  dbOut=dbInfo$pathList$dbOut)
    
    subModelTablePrint<-subset(subModelTable, select=c(
      'id',
      'idJob',
      'idRun',
      'species',
      'method',
      'group',
      'tss',
      'nPr',
      'nPa',
      'prdLoCorr',
      'prdHiCorr',
      'message'))
  
 

    
    handleNullDataTable(subModelTablePrint)
  },sanitize.text.function=formatCommasTable)
  
  #####################
  #                   #
  # DOWNLOAD MODELS   #
  #                   #
  #####################  
  

  
  output$downloadModels <- downloadHandler(
    
    filename = function() {
      paste('lebaModels-', Sys.Date(), '.zip', sep='')
    },
    content = function(file) {
      tmpFile<-tempfile(tempdir(),pattern='lebaModels',fileext='.zip')
      modelsPaths<- c(file.path(dbInfo$pathList$models,subModelTable$fileName))
      zip(zipfile=tmpFile,files=modelsPaths) 
      file.rename(tmpFile, file)
    },
    contentType='application/zip'
  )
  
  
  
  
  output$downloadPred <- downloadHandler(
    filename = function() {
      paste('lebaPred-', Sys.Date(), '.rds', sep='')
    },
    content = function(file) {
      lebaList<-getEnsembleListSpecies(subModelTable,dbInfo$pathList$models)
      saveRDS(presProbSpecies(lebaList),file)
    }
  )
  
  
  
  
  #####################
  #                   #
  # PLOT PROBS VS VAR #
  #                   #
  #####################
  
  
  output$spVsVar<-renderPlot({
    # avoid first incrementation of action button
    if(input$plotSpVsVar>0){


#       resultTableTest<-filterResults(species=c('Baetis alpinus','Alainites muticus'),
#                                      dbOut=dbInfo$pathList$dbOut)
#       
      
      elr<-getEnsembleListSpecies(resultsTable=subModelTable,
#                                dbOut=dbInfo$pathList$dbOut,
                               modelsLoc=dbInfo$pathList$models)
      

listLength<-length(elr)
if(listLength>100){
  messageLebaModResult('WARNING : more than 100 models selected, please be patient.')
}



      ppsv<-presProbSpeciesByVar(lebaEnsembleListSpecies=elr,
                                 varBy=input$varPlotBy,
                                 rangeX=input$longRange,
                                 rangeY=input$latRange,
                                 rangeDem =input$demRange,
                                 rangeYear = input$yearRange)  
      
      
      form<-as.formula(paste0("medPres~",input$varPlotBy,"|idSpeciesString"))

      plotSpVsVar<-xyplot(form,group=idMod,ppsv,col='black',type='l',alpha=0.4)+
        layer(panel.smoother(x, y, method = "loess",span=1), style = 4)
print(plotSpVsVar)
      
    }else{
      plotSpVsVar<-xyplot(x~y,data=data.frame(x=0,y=0))
      print(plotSpVsVar)
    }


  })


output$downloadPlotPresVsYearLat <- downloadHandler(
  filename = function() {
    paste('lebaPlotLattice-', Sys.Date(), '.rds', sep='')
  },
  content = function(file) {
    elr<-getEnsembleListSpecies(resultsTable=subModelTable,
                                #                                dbOut=dbInfo$pathList$dbOut,
                                modelsLoc=dbInfo$pathList$models)
    
    
    ppsv<-presProbSpeciesByVar(lebaEnsembleListSpecies=elr,
                               varBy=input$varPlotBy,
                               rangeX=input$longRange,
                               rangeY=input$latRange,
                               rangeDem =input$demRange,
                               rangeYear = input$yearRange)  
    
    
    form<-as.formula(paste0("medPres~",input$varPlotBy,"|idSpeciesString"))
    
    plotSpVsVar<-xyplot(form,group=idMod,ppsv,col='black',type='l',alpha=0.4)+
      layer(panel.smoother(x, y, method = "loess",span=1), style = 4)
    saveRDS(plotSpVsVar,file)
  }
)

output$RsessionLocal<-renderPrint({sessionInfo()})


output$RsessionRemote<-renderPrint({
  clDat<-dbInfo$clustersList[[input$selectRemote]]
  clDat<-dbInfo$clustersList[['climdal']]
  
  tmpF<-tempfile('remoteSessionInfo',tempdir(),fileext='.rds')
  
  cmdScp<-paste0('scp ',clDat$user,'@',clDat$hostName, ":", 
                 clDat$pathProject, '/data/logs/sessionInfo.rds ',
                 tmpF)
  system(cmdScp)
  
  rSessionRemote<-readRDS(tmpF)
  rSessionRemote
  })


}

  
})

#dbInfo
#dbModels




