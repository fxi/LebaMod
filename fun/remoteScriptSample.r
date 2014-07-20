
remoteScriptSample <- function(paramList=paramList,dataList=dataList){
	# here we construct function to apply to data, 
	# with configuration read from paramList
	# testing process: do nothing but a list. 
	name <- paramList$test
	medParam <- median(dataList$data)
	list(
		 name=name,
		 medParam=medParam
		 )
}



