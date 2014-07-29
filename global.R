################################################################################
#
# LebaMod
# Laboratoire d'écologie et biologie aquatique, Universté de Genève
# Species presence only modeling and analysis application
#
# moser.frederic@gmail.com
# 2014
#
#
################################################################################



source('../data/config/configLebaMod.r')

# set global variables
spL<-dbInfo$speciesList
prL<-dbInfo$predictorsList
meL<-dbInfo$methodList
grL<-dbInfo$predictorsQuant #quant=group..
clL<-names(dbInfo$clustersList)
# range
yearR<-dbInfo$predictorsYearsRange
demR<-dbInfo$predictorsDemRange
latR<-c(dbInfo$predictorsSpatialRange@ymin,dbInfo$predictorsSpatialRange@ymax)
lonR<-c(dbInfo$predictorsSpatialRange@xmin,dbInfo$predictorsSpatialRange@xmax)

# set DEFAULT list and data.table
paramList=list()
jobTable=data.table()
subModelTable=data.table()