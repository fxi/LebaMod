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
      dataTable<-rbind(dataTable,t(rep('-',length(names(dataTable)))))
      names(dataTable) <-nm
    }else{
      dataTable
    }
  }
  return(dataTable)
}