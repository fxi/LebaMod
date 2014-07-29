# shiny helper functions


formatCommasTable<-function(x){
  gsub(',',' ',x)
}

handleNullDataTable<-function(dataTable){
  nm<-names(dataTable)
  if(length(nm)<1 | !any(c('data.table','data.frame','matrix','vector') %in% class(dataTable))){
    dataTable<-data.table(data='noData')
  }else{
    if(nrow(dataTable)<1  & length(nm)>1){
      lDt<-length(names(dataTable))
      dataTable<-rbind(
        dataTable,
        as.list(rep('-',lDt))
        )
      names(dataTable) <-nm
      print(dataTable)
    }
  }
  return(dataTable)
}


#conditionalHelper
condHelp<-function(id='showHelp',helpMsg){
  conditionalPanel(paste0('input.',id,' == true'),
                   helpText(helpMsg)
  )
}
