
createIndexOnTablePath <- function(pathToDb,tableName,columnsNames){
	## creation of index on sqlite db table
	# Args :
	#	pathToDb : relative path to sqlite database
	#	tableName : name of table to index
	#   columnsNames : vector of columns to index on.
	require(RSQLite)	
	conDb <- dbConnect(SQLite(),pathToDb) 
	colNames<-paste0(columnsNames,collapse=',')
	idxName = paste0('idx_',tableName)
	message(paste('Create index',idxName,'on',tableName))
	querCreate <- paste0('CREATE INDEX IF NOT EXISTS ',idxName,' ON ',tableName,'(',columnsNames,')')
	#	message(querCreate)
	dbGetQuery(conDb,querCreate)
}


