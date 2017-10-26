


library(readr)
electricity_prices_non_household <- read_csv("electricity_prices_non_household.csv")


employment_EU <- read_csv("employment_EU.csv")


#working with shapefile

require(rgdal)
# Read SHAPEFILE.shp from the current working directory (".")
shape <- readOGR(dsn="/students/agsinkevich/28_10_exam_spatial_econometrics/CNTR_RG_01M_2013.shp")

setwd("/students/agsinkevich/28_10_exam_spatial_econometrics")

EU_abbreviations <- read_excel("EU_abbreviations.xlsx", col_names = c("GEO","Short"))


#plotting

#png("EU.png", width = 800, height = 400)
#plot(shape)
#dev.off()

plot.new()
summary(shape)


#distance-based neibourghs
library(spdep)

coord<-coordinates(shape)


distance_10 <- knn2nb(knearneigh(coord, k = 10), row.names = shape$CNTR_ID)

plot.new()

png("EU_dist_10.png", width = 800, height = 400)


plot(distance_10, coord, add=TRUE)
dev.off()

#Modify the data


#remain only EU_abbreviations countries
library(dplyr)
y<- filter(employment_EU, employment_EU$GEO %in% shape$CNTR_ID)
y$GEO<-as.factor(y$GEO)


x<-filter(electricity_prices_non_household, electricity_prices_non_household$GEO %in% shape$CNTR_ID)
x$GEO<-as.factor(x$GEO)
x$Value<-as.numeric(x$Value)

x<-filter(x,x$CURRENCY=="EUR")
x<-filter(x,x$TAX=="All taxes and levies included")
x[ x==":"]<-NA
#x <- x[,-10]
x<-na.omit(x)

#Date wrapping in x
x$TIME<-as.Date(x$TIME, format = "%m/%d/%Y")
class(x$TIME)

#x<-filter(x, GEO==y$GEO)

myvars <- levels(x$GEO) %in% levels(y$GEO)

xx$GEO <- subset(xx, myvars)

xx<-subset(x, x$GEO %in% y$GEO)

x<-subset(x, y$GEO %in% x$GEO)
levels(xx$GEO)
levels(y$GEO)

library(tidyr)
y<-gather(y, key = TIME, value = Employment_rate, 2:63)
#Time wrapping in y

#library(lubridate)
#yy$TIME<-parse_date_time(yy$TIME, orders="mdy")
class(y$TIME)

y$TIME <- as.Date(y$TIME,format="%m/%d/%Y")
class(y$TIME)


#yy$TIME<-as.Date(yy$TIME, format = "%m/%d/%Y", tz = "GMT")

#Modify the data
y$Employment_rate<-as.numeric(y$Employment_rate)

library(stats)
plot.ts()

library(ggplot2)
ggplot(y,aes(TIME, Employment_rate))+ geom_line(aes(col=GEO))+  labs(x = "Years", y = "Employment (%)")+theme_bw()

ggplot(x, aes(TIME, Value)) + geom_line(aes(col=GEO))+ labs(x = "Years", y = "EUR0")+theme_bw()

library(maptools)
library(ggmap)

#subset a shapefile
#subset.Spatial

# it was really tough

shape_new<-shape[shape$CNTR_ID %in% levels(y$GEO) ,]

shape_new$CNTR_ID<-na.omit(shape_new$CNTR_ID)
 
plot(shape_new)
shape_new$CNTR_ID
# plotting a map emplyment rate (to have visible borders) (Milano)

shape_new$CNTR_ID

c<-levels(y$GEO)

levels(x$GEO)
#shape_new<-filter(shape, CNTR_ID %in% y$GEO)

df<-merge(shape_new, y,  by.x="CNTR_ID" , by.y="GEO")

ggplot(shape_new, aes(long, lat))+   geom_polygon(aes(x=long, y=lat, group=group, fill = Employment_rate)) + coord_equal()
#m2 <- m1 + geom_path(aes(x=long, y=lat, group=group), color='black')

#shape$

#map <- get_map(location = 'Europe', zoom=4)
#ggmap(map) + geom_polygon(aes(x=long, y=lat, group=group, fill=Employment_rate), data=y, alpha=.9) + geom_path(aes(x=long, y=lat, group=group), data=y, color='black')
#library(zoo)


#time<-ts()

#x$TIME




#Moran's I


#Moran's scatterplot