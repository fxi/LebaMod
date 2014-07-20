






varBy='YYYY'
form<-as.formula(paste0("medPres~",varBy,"|idSpeciesString"))
xyplot(form,group=idMod,predMed,type='l',col='black',alpha=0.4)+
  layer(panel.smoother(x, y, method = "loess",span=1), style = 4)
















data(economics, package = "ggplot2")
econ <- transform(economics, date = as.character(date))
m1 <- mPlot(x = "date", y = c("psavert", "uempmed"), type = "Line", data = econ)
m1$set(pointSize = 0, lineWidth = 1)
m1$print("chart2")


require(devtools)
install_github('rCharts', 'ramnathv')
devtools::install_github('rCharts', 'ramnathv', ref = 'dev')
require(rCharts)
names(iris) = gsub("\\.", "", names(iris))
rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'line')


m1 <- mPlot(x = "date", y = c("psavert", "uempmed"), type = "Line", data = econ)
m1$set(pointSize = 0, lineWidth = 1)
m1$print("chart2")

# 
rangeX<-filterX
rangeY<-filterY
rangeDem<-filterDem
rangeYear<-filterYear


 medByVar<-presProbRunsByVar(testEnsemble,'YYYY',rangeX,rangeY,rangeDem,rangeYear)
# 
varBy='YYYY'
form<-as.formula(paste0("medPres~",varBy,"|idRunString"))
n1<-nPlot(form,group=idMod,data=predMed,type='lineChart')





names(iris) = gsub("\\.", "", names(iris))
p1<-nPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'lineChart')
p1$set(pointSize = 0, lineWidth = 1)
p1$addParams(dom = 'testPlot')
return(p1)



options(rcharts.cdn = TRUE)


require(rCharts)
names(iris) = gsub("\\.", "", names(iris))
p1<-rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'line')
p1$addParams(dom = 'testPlot')
return(p1)



