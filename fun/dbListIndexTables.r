
dbListIndexTables <- function(dbCon){
# function to get all index tables from db
	require(RSQLite)
	dbGetQuery(dbCon,"SELECT * FROM sqlite_master WHERE type = 'index';")
}



