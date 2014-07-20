summaryFast<-function(DT,cores=2,excludes=NULL){
	require(foreach)
	require(doMC)
	require(data.table)
	registerDoMC(cores)
	if(!is.data.table(DT))stop('Please provide a valid data.table')
	foreach(n=names(DT), .combine='rbind')%dopar%{
		if(!n %in% excludes){
			cla=DT[1:20,class(get(n))]
			isNum=DT[1:20,is.numeric(get(n))]
			if(isNum){
				setkeyv(DT,n)
				r1e2 <- function(x)round(x*100)/100
				data.table(var=n,
						   min=ifelse(isNum,r1e2(DT[,min(get(n),na.rm=T)]),'--'),
						   max=ifelse(isNum,r1e2(DT[,max(get(n),na.rm=T)]),'--'),
						   median=ifelse(isNum,r1e2(DT[,median(get(n),na.rm=T)]),'--'),
						   mean=ifelse(isNum,r1e2(DT[,mean(get(n),na.rm=T)]),'--'),
						   sd=ifelse(isNum,r1e2(DT[,sd(get(n),na.rm=T)]),'--'),
						   nulls=nrow(DT[is.na(get(n))]),
						   length=nrow(DT),
						   class=cla
						   )
			}
		}
	}
}



