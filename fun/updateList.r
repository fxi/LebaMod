

updateList <- function(oldList,newList){
## update named list with another named list. 
	if(is.list(oldList) & is.list(newList)){
	nNames <- names(newList)
	for(n in nNames){
		oldList[n]<-newList[n]
	}			 
	oldList}else{
	message('One list is not a list. Returning old list')
	oldList
	}
}


