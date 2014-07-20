
# 
# createBundle <- function(expr,dataSet=NULL,paramList=NULL,objectToReturn){
# 	# expr = r script to execute remotely
# 	# dataSet = optionnal data to include. Prefer using remote db
# 	# paramList = local parameters for remote script.
# 	# objectToReturn = object that will be compressed and returned.
# 	rBundle <- list(dat=dataSet,
# 					par=paramList,
# 					ret=objectToReturn,
# 					exp=as.expression(substitute(expr)) 
# 					)
# }



createBundle <- function(expr,jobList){
  # expr = r script to execute remotely
  # jobList = list of jobs to be evaluated by exp
  rBundle <- list(jobs=jobList,
                  exp=as.expression(substitute(expr)) 
  )
}