
library(readxl)

electricity_consumption <- read_excel("spatial_project/electricity_consumption.xls",skip = 10)




#working with shapefile

require(rgdal)
# Read SHAPEFILE.shp from the current working directory (".")
shape <- readOGR(dsn="/students/agsinkevich/spatial_project/CNTR_RG_01M_2013.shp")

setwd("/students/agsinkevich/spatial_project")

EU_abbreviations <- read_excel("EU_abbreviations.xlsx", col_names = c("GEO/TIME","Short"))


#plotting

plot(shape)

summary(shape)


#distance-based neibourghs
library(spdep)

coord<-coordinates(shape)


b <- knn2nb(knearneigh(coord, k = 1), row.names = shape$CNTR_ID)

plot(b, coord, add=TRUE)

