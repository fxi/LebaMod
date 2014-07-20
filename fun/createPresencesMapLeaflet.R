

initMapFromDb<-function(db,table,idMap,provider,crsOrig=NULL,crsDest=NULL,zoom){
  # set div id=idMap in ui.r, and launch a script to create L.map object with tile layers.
  # db = sqlite database to connect
  # table = table in db
  # idMap = name/id of map to create
  # layer provider, name from in http://leaflet-extras.github.io/leaflet-providers/preview/index.html
  # crsOrig: sp::crs object with coordinate reference system for input data (sqlite database)
  # crsDest: sp::crs object for output map (wgs84)
  # zoom : leflet zoom parameter
  require(sp)
  require(rgdal)
  require(RSQLite)

  # get center
  con<-dbConnect(SQLite(),db)
  stopifnot(table %in% dbListTables(con))
  namesInDb<-dbGetQuery(con,sprintf('PRAGMA table_info(%s)',table))$name
  stopifnot(all(c('x','y') %in% namesInDb))
  sqlQuery<-sprintf("SELECT min(x),max(x),min(y),max(y) FROM %s",table)
  ext <- data.frame(t(bbox(matrix(dbGetQuery(con,sqlQuery),2))))
  
  dbDisconnect(con)
  if(!is.null(crsOrig)&!is.null(crsDest)){
    message('In init map, new coordinate system will be applied. ')
    coordinates(ext)<-~x+y
    proj4string(ext)<-crsOrig
    ext<-as.data.frame(spTransform(ext,crsDest))
  }
  xCenter<-mean(ext$x)
  yCenter<-mean(ext$y)
  
  HTML(paste(div(id=idMap,style="height:400px;width:100%"),
             sprintf("<script>
  var %s = L.map('%s').setView([%f, %f], %f);
  var thunderForest = L.tileLayer.provider('%s', {}).addTo(%s);
</script>",idMap,idMap,yCenter,xCenter,zoom,provider,idMap)))
}



markersClusterFromDb<-function(db,table,idMap,subset=NULL,allRecords=F,crsOrig=NULL,crsDest=NULL){
  # db and table : expecting a database table with sp,x,y,YYYY columns.
  # idMap : same id as set in initMap function
  # subset : char vector of species names, as found in db, used with IN sqlite operator
  # crsOrig: sp::crs object with coordinate reference system for input data (sqlite database)
  # crsDest: sp::crs object for output map (eg.  +proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0 )
  require(sp)
  require(rgdal)
  require(RJSONIO)
  require(RSQLite)
  if(is.null(subset)&!isTRUE(allRecords)){
    return(NULL)
  }else{
    tmp<-tempfile()
    con<-dbConnect(SQLite(),db)
    stopifnot(table %in% dbListTables(con))
    namesInDb<-dbGetQuery(con,sprintf('PRAGMA table_info(%s)',table))$name
    stopifnot(all(c('x','y') %in% namesInDb))
    
    if(!allRecords){
      subset<-paste0("('",paste0(subset,collapse="','"),"')")
      sqlQuery<-sprintf("SELECT * FROM %s WHERE sp IN %s",table,subset)
    }else{
      sqlQuery<-sprintf("SELECT * FROM %s",table)
    }
    
    spTable<-dbGetQuery(con,sqlQuery)
    if(!nrow(spTable)>0){
      message('spTable length is 0')
    }else{
      dbDisconnect(con)
      coordinates(spTable)<-~x+y
      proj4string(spTable)<-crsOrig
      if(!is.null(crsOrig)&!is.null(crsDest)){
        spTable<-spTransform(spTable,crsDest)
      }
      bbxSp<-bbox(spTable)
      xCenter<-mean(bbxSp['x',])
      yCenter<-mean(bbxSp['y',])
      writeOGR(spTable,dsn=tmp,layer='species',driver='GeoJSON')
      
      spDataJs<-"
<script>
if(%s.hasLayer(markers)){%s.removeLayer(markers)}
var markers = L.markerClusterGroup({maxClusterRadius:30, singleMarkerMode:true});
var geoJsonLayer = L.geoJson(%s, {
 onEachFeature:onEachFeatureGetProp
});

markers.addLayer(geoJsonLayer);
%s.fitBounds(markers.getBounds());
%s.addLayer(markers);
</script>

 " 
      #map.fitBounds(markers.getBounds()); 
      spMap<-sprintf(spDataJs,idMap,idMap,readChar(tmp, file.info(tmp)$size),idMap,idMap)
      HTML(spMap)
    }}
  
}

summarySpeciesFromDbInfo<-function(dbInfo,includeSpatialVars=FALSE,species=NULL,predictors=NULL,group=NULL){
  if(is.null(species)|is.null(predictors)|is.null(group)){
    NULL
  }else{
    # subset of dbInfo$speciesSummaryByAll
    dat<-dbInfo$speciesSummaryByAll
    if(length(includeSpatialVars)>0){
      if(includeSpatialVars){
      predictors<-c('x','y','dem',predictors)
      }
    }
    setkeyv(dat,c('variable'))
    pFilt<-dat[predictors]
  
    setkeyv(pFilt,'sp')
    spFilt<-pFilt[species]
    
    setkeyv(spFilt,'group')
    groupFilt<-spFilt[group]
    
    groupFilt

    return(groupFilt)
  }
  
}




