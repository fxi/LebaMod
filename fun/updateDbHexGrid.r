
updateDbHexGrid <- function(paramList,dbinfo,overWrite=F){
  message('Updade hexgrid if needed')
  require(raster)
  require(sp)
  require(RSQLite)
  
  # path to db
  dbSpPred <- dbInfo$pathList$dbIn
  # path to db cache 
  dbSpPredInfo <- dbInfo$pathList$dbCache
  
  crsObj <- dbInfo$crsOrig
  # If idHex table exist, do nothing : the hex class/id have to be linked
  # between database AND spatialPolyGons in paramList !
  
  
  # We update db info from file : this script will updated it and reload it
  paramDb <- readRDS(dbSpPredInfo)
  
  dbCon<- dbConnect(SQLite(),dbSpPred)
  existsHexClass <- 'idHex' %in% dbListTables(dbCon)
  
  if(existsHexClass & overWrite==T){
    dbGetQuery(dbCon,'DROP TABLE idHex')
  }
  
  oldRadius <- paramDb$hexRadius
  newRadius <- paramList$probHexRadius
  
  changeRadius <- !identical(oldRadius,newRadius)
  
  
  message('updateHexGrid. oldRadius: ',oldRadius,' newRadius ', newRadius)
  
  
  if(!existsHexClass | overWrite==T | changeRadius){
    # this function will produce hexagonal grid based on predictors
    # extent and selected radius, and make a table in db with corresponding
    # hexagonal class for each pair of xy.
    # get distinct xy
    
    xy <- dbGetQuery(dbCon,"SELECT DISTINCT x,y FROM predictors")
    xySpatial <- xy
    coordinates(xySpatial) <- ~x+y
    proj4string(xySpatial) <- crsObj
    predictorsExtent <- bbx2poly(xySpatial,crsObj,addMargin=newRadius)
    
    hexGrid <- spsample(predictorsExtent,n=1,type='hexagonal',cellsize=newRadius)
    #	hexGrid <- spsample(xySpatial,n=1,type='hexagonal',cellsize=paramList$probHexRadius)
    # rename row as match.id expect in spatialPolyGonDataFrame
    row.names(hexGrid) <- paste0('ID',row.names(hexGrid))
    # make the data frame for spatialPolyData.frame
    hexGridDf <- data.frame(hexGrid,idHex=row.names(hexGrid))
    # create actual hex grid polygons
    hexPoly <- HexPoints2SpatialPolygons(hexGrid)
    # assemble hex grid and df by ID (match value from rows names...)
    hexPolyDf <- SpatialPolygonsDataFrame(HexPoints2SpatialPolygons(hexGrid),hexGridDf,match.ID=T)
    # retrieve value of hex poly for all xySpatial
    idHexOver <- over(xySpatial,hexPolyDf)
    # merge values.
    idHex <- data.frame(xy,idHexOver)
    names(idHex) <- c('x','y','xHex','yHex','idHex')
    
    message('HexClass table will be written')
    dbWriteTable(dbCon,'idHex',idHex,overwrite=T,row.names=F)
    createIndexOnTable(dbCon,'idHex','x,y,idHex')
    
    # subset of hexPolyDf with unique classes
    uniquesClass <- as.character(unique(idHex[,'idHex']))
    hexSubsetPoly <- hexPolyDf[hexPolyDf$idHex %in% uniquesClass,'idHex']
    hexSubsetPoly$idHex <- as.character(hexSubsetPoly$idHex)
    
    # save poly in db file.
    paramDb$hexPoly <- hexSubsetPoly
    paramDb$hexExtent <- predictorsExtent
    paramDb$hexRadius <- newRadius
    saveRDS(paramDb,dbSpPredInfo)
    
    # calc median of all predictors under hex cell
    
    extractAggregHexTable(dbCon,dbInfo$predictorsList)
    return(paramDb)
    
  }else{message('HexTable already exists,skipping')}
  dbDisconnect(dbCon)
  return(paramDb)
}


extractAggregHexTable<-function(dbCon,predictors){
  message('Aggregate under hex grid. Could take a while!')
  require(RSQLite.extfuns)
  init_extensions(dbCon)
  avgVarList <- paste0('median(',predictors,')',predictors,collapse=',')
  indexCols<-c('x','y','idHex','dem','bassinID','YYYY','quant')
  # with aggregation by idHex, we select mode of bassin ID and median for dem.
  indexColsSql<-c('(xHex)x','(yHex)y','idHex','median(dem)dem','mode(bassinID)bassinID','YYYY','quant')
  indexColsCollapse<-paste(indexColsSql,collapse=',')
  hexSql <- paste0(" SELECT ",indexColsCollapse,",",avgVarList,
                   " FROM predictors NATURAL JOIN idHex",
                   " GROUP BY YYYY,quant,idHex")
  
  unkX <- data.table(na.omit(dbGetQuery(dbCon,hexSql)))
  dbWriteTable(dbCon,'predictorsAggregHex',unkX,row.names=F)
  createIndexOnTable(dbCon,'predictorsHexGrid','quant,YYYY,idHex')
  
}


# use hexGrid to aggregate predictors. Will be used as unkx in extractProbs
# to do 
# - export this code in update hexGrid : put table in modeldb instead ?
# - change predictors : all predictors instead.
# - new function to extract only hexGrid cols needed.

# hexTableName<-jobTable$hexGridTable[1]
# if(!hexTableName %in% dbListTable(dbCon)){
#   message("hex table '",hexTableName,"' not found in modelDb. Extract aggregated median under all cells")
#   unkX<-extractHexUnkx(dbSpPred=jobs[[1]]$dbSp, predictors=strsplit(jobs[[1]]$p,',')[[1]])
#   dbWriteTable(dbCon,hexTableName,unkX)
# }

