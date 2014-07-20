bbx2poly<-function(obj, crs, addMargin){
# get object extent
	b<-bbox(obj) # martrix
	b[c(3,4)] <- b[c(3,4)]+addMargin #x,y max
	b[c(1,2)] <- b[c(1,2)]-addMargin #x,y min
	m<-matrix(c(b[c(1,2)],
		    b[c(1,4)], 
		    b[c(3,4)], 
		    b[c(3,2)], 
		    b[c(1,2)]),
		  ncol=2, byrow=TRUE)
	names(m)<-c('x','y')
	SpatialPolygons(list(Polygons((list(Polygon(m))),ID="1")), proj4string=crs)
}
