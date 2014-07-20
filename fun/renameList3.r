renameList3 <- function(x,r){
	# rename a list with repeatitive value accross nested list.
	# the original problem is that i could not find a way
	# to name list accross a nested foreach using %:% operator.
	#
	# args : 
	# x = a list to be recursivly renamed
	# r = a list that contains names 
	#
	# to do :
	# 1.make a function that call itself (recursive function).
	# 2.check of foreach nested loop could make it faster
	stopifnot(is.list(r),is.list(x),length(r)==3)
	names(x)=r[[1]]
	for(na in names(x)){
		names(x[[na]]) <- r[[2]]
		for(nb in names(x[[na]])){
			names(x[[na]][[nb]]) <- r[[3]]
		}
	}
	x
}
##
#
#
#lA <- list('a','b','c')
#lB <- lapply(lA,c,lA)
#lC <- lapply(lB,c,lB)
#
#
#
#dat <- c(1:3)
#
#dList <- list(as.list(nA),as.list(nB),as.list(nC))
#
#testList <- 
#
#
#tL <- nlist(lA)
#
#
#
#nlist <- function(...) {
#	    L <- list(...)
#    if (!is.null(names(L))) return(L)
#	    n <- lapply(match.call(),deparse)[-1]
#	    setNames(L,n)
#}
#
#b <- c <- d <- 1
#
#nlist(b,c,d)
#nlist(d=b,b=c,c=d)
#
#
#
#match.call(get, call("get", "abc", i = FALSE, p = 3))o
#
#
#
#ft <- function(...){lapply(match.call(),deparse)[-1]}
