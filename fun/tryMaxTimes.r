

tryMaxTimes <- function(expr,maxTry=3,delay=1) {
	stopifnot(is.expression(expr))
	i=1
	test=F
	while (i==1 | !test ) {
    message('TryMaxTimes. t=',i)
		res <- try(eval(expr,envir=parent.frame()))
		wMsg=paste(expr,'failed. ',maxTry-i,' try left.')
		fMsg=paste(expr,'failed after ',i,' try')
		#ifelse(!isTRUE(res)|i<maxTry,{Sys.sleep(delay);warning(wMsg);i=i+1},{test=T;return(res)})
    if(class(res)=='try-error' & i<maxTry){
      Sys.sleep(delay)
      warning(wMsg)
      i=i+1
    }else if(class(res)=='try-error' & i>=maxTry){
      test=T
      warning(fMsg)
      return(res)
    }else{
      test=T
      return(res)
    }
	}
}

