

gJsonOgrByYear <- function(paramList,yearRange,qtl){
# produce a plot by year, for a quantile.
	require(rgdal)
	require(foreach)
	require(doMC)
	registerDoMC(8)

	pL=paramList
	# get stored polygon object
	pol <- pL$hexPoly
	# check if row matches hexClass column
	stopifnot(identical(row.names(pol),pol$hexClass))
	# extract probability under hexagonal grid for one year
	foreach(y=yearRange)%do%{
		prob <- subset(paramList$hexaProb,YYYY==y & quant==qtl)
		#prob<-as.data.frame(lapply(prob,as.character))
		prob$probPresTxt<-paste('Probs:',prob$PRES,'.i Quantile=',qtl,'.Method=',pL$methodSelect)
		row.names(prob)<-prob$hexClass
		stopifnot(unique(row.names(prob)) %in% row.names(pol))
		spHexa <- SpatialPolygonsDataFrame(pol,prob,match.ID=T)
		spHexa <- spTransform(spHexa,pL$CRSwgs84)
		writeOGR(spHexa[c('PRES','probPresTxt')],layer='sp',dsn=paste0('geoJSON/',y,'.GeoJSON'),driver='GeoJSON')
		message(y)
	}
	return(NULL)
}


