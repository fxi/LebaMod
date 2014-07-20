

remoteBundle <- function(expr,dataSet,paramList){
	list(dat=dataSet,
		 par=paramList,
		 exp=as.expression(substitute(x)) 
		 )
}

#
#re <- remoteEval({
#	message('alors=')
#	print(ls(envir=.GlobalEnv))
#	print(getwd())
#})
#
#
#eval(re)
#

