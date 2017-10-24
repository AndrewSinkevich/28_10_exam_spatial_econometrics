


library(readr)
electricity_prices_non_household <- read_csv("electricity_prices_non_household.csv")


library(readxl)
employment_EU <- read_excel("employment_EU.xls")


#working with shapefile

require(rgdal)
# Read SHAPEFILE.shp from the current working directory (".")
shape <- readOGR(dsn="/students/agsinkevich/28_10_exam_spatial_econometrics/CNTR_RG_01M_2013.shp")

setwd("/students/agsinkevich/28_10_exam_spatial_econometrics")

EU_abbreviations <- read_excel("EU_abbreviations.xlsx", col_names = c("GEO","Short"))


#plotting

png("EU.png", width = 1800, height = 1600)
plot(shape)
dev.off()

summary(shape)


#distance-based neibourghs
library(spdep)

coord<-coordinates(shape)


b <- knn2nb(knearneigh(coord, k = 1), row.names = shape$CNTR_ID)

png("EU.png", width = 1800, height = 1600)
plot(b, coord, add=TRUE)
dev.off()

