
createIndexOnTable <- function(con,tableName,columnsNames){
#create index if needed
	idxName = paste0('idx_',tableName)
	message(paste('Create index',idxName,'on',tableName))
	querCreate <- paste0('CREATE INDEX IF NOT EXISTS ',
						 idxName,' ON ',
						 tableName,
						 '(',columnsNames,')')
	#	message(querCreate)
	dbGetQuery(con,querCreate)
}
