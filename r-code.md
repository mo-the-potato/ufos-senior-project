# ufos
# R Code 
## Undergrad Statistics Thesis
## A Spatial Analysis of UFO Sightings in the Continental United States using the spatstat package

##Packages needed
library(maps)
library(maptools)
library(sf)
library(tmap)
library(tmaptools)
library(mapproj)
library(usmap)
library(ggplot2)
library(readxl)
library(rgdal)
library(maptools)
library(raster)
library(RColorBrewer)
library(rgeos)
library(geostatsp)
#spatstat
library(spatstat)

##Read in the data
dat <- read_excel("UFO-Sightings-US-Top-Ten-Shapes.xlsx")

#Limiting Lat and Long to Continental US
dat <- dat[dat$latitude < 50,]
dat <- dat[dat$latitude > 25,]
dat<- dat[dat$longitude > -125,]
dat <- dat[dat$longitude < -65,]
dat <- na.omit(dat) #MO: taking out na's and blanks

#Checking duration variable for outliers
source("http://goo.gl/UUyEzD")
outlierKD(dat, durationinseconds)
dat <-na.omit(dat) #MO: taking out na's that are in place of outliers
ufo <-dat

#Initial Plot
ufo_dat<-st_as_sf(ufo, coords=c("longitude","latitude"), crs=CRS("+proj=utm"))
state_map<-map('state',fill=T,plot=F)
class(state_map)
state_poly <-map2SpatialPolygons(state_map, IDs=state_map$names, proj4string=CRS("+proj=utm"))
tm_shape(ufo_dat)+tm_symbols(palette='YlOrRd', border.lwd=0.2, border.col='gray',alpha=0.9, scale=0.8) +
  tm_shape(state_poly)+tm_borders(lwd=1.1,col='gray')+tm_legend(position=c("left","bottom"),frame=TRUE,
                                                                main.title='UFO Sightings in the Continental US')
 
###PPP Model 
ufo$datetime<-NULL
ufo$city<-NULL
ufo$state<-NULL
ufo$country<-NULL
ufo$comments<-NULL
ufo$`date posted`<-NULL
ufo$shape <- as.factor(ufo$shape)
ufo$season <- as.factor(ufo$season)
ufo$month <- as.factor(ufo$month)
ufo$decade <-as.factor(ufo$decade)
ufo$year <-as.factor(ufo$year)
ufo1 <-SpatialPointsDataFrame(coords=ufo[,7:8],data=data.frame(ufo),proj4string = CRS("+proj=utm"))
state_map<-map('state',fill=T,plot=F)
class(state_map)
state_poly <-map2SpatialPolygons(state_map, IDs=state_map$names, proj4string=CRS("+proj=utm"))

#Density for Shape
ufo2 <-ppp(x=ufo1$longitude,y=ufo1$latitude,window=as(state_poly,"owin"))
ufo2 <-as.ppp(ufo1) #Change ufo2 window and rerun models
is.multitype(ufo2)
mark <-marks(ufo2)
mark$shape
marks(ufo2)<-mark$shape
ufo2
summary(ufo2)
plot.new()
par(mfrow=c(1,1))
UFOShape <- split(ufo2,which.marks="shape")
plot(UFOShape)
plot(density(UFOShape))

#Density for Seasons
ufo3 <-as.ppp(ufo1)
is.multitype(ufo3)
mark <-marks(ufo3)
mark$season
marks(ufo3)<-mark$season
ufo3
summary(ufo3)
plot.new()
par(mfrow=c(1,1))
UFOSeason <- split(ufo3,which.marks="season") #How do I split this? 
plot(UFOSeason)
plot(density(UFOSeason))

#Density for Months
ufo4 <-as.ppp(ufo1)
is.multitype(ufo4)
mark <-marks(ufo4)
mark$month
marks(ufo4)<-mark$month
ufo4
summary(ufo4)
plot.new()
par(mfrow=c(1,1))
UFOMonth <- split(ufo4,which.marks="month")
plot(UFOMonth)
plot(density(UFOMonth))#,leg.args=list(at))

#Density for Year
ufo5 <-as.ppp(ufo1)
is.multitype(ufo5)
mark <-marks(ufo5)
mark$year
marks(ufo5)<-mark$year
ufo5
summary(ufo5)
plot.new()
par(mfrow=c(1,1))
UFOYear <- split(ufo5,which.marks="year") #How do I split this? 
plot(UFOYear)
plot(density(UFOYear))

#Making year a factor and an image
is.factor(ufo2$year)
year <-as.factor(ufo$year)
is.factor(year)
y <- levels(year)
y <-as.matrix(y)
year.img <- as.im(y,W=as(state_poly,"owin"))

#Making month a categorical variable and imape
is.factor(ufo$month)
month <-as.factor(ufo$month)
is.factor(month)
m <- levels(month)
m <-as.matrix(m)
month.img <- as.im(m,W=as(state_poly,"owin"))


#Studying Intensity
#Population Density
city <- as.matrix(us.cities)
write.csv(city,"city.csv")
city <-read.csv("city.csv")
city <- city[city$lat < 50,]
city <- city[city$lat > 25,] 
city<- city[city$long > -125,]
city <- city[city$long < -65,]
city <- city[city$pop > 100000,]
city <- na.omit(city) #MO: taking out na's and blanks
city1 <-SpatialPointsDataFrame(coords=city[,5:6],data=data.frame(city),proj4string = CRS("+proj=utm"))

#Create ppp for Population Density
#city3 <- ppp(x=city$long,city$lat,window=as(US,"owin"))
city2 <-ppp(x=city1$long,y=city1$lat,window=as(state_poly,"owin"))
pop <-distmap(city2)

#Air Force Bases
#File containing lat and long of air force bases
air <-read_excel("Air Force Base Locations.xlsx")
#Limiting Lat and Long to Continental US
air <- air[air$Latitude < 50,]
air <- air[air$Latitude > 25,]
air<- air[air$Longitude > -125,]
air <- air[air$Longitude < -65,]
air <- na.omit(air) #MO: taking out na's and blanks
#Map of Air Force Bases
air_dat<-st_as_sf(air, coords=c("Longitude","Latitude"), crs=CRS("+proj=utm"))
state_map<-map('state',fill=T,plot=F)
class(state_map)
state_poly <-map2SpatialPolygons(state_map, IDs=state_map$names, proj4string=CRS("+proj=utm"))
tm_shape(air_dat)+tm_symbols(col="Air Force Base",palette='YlOrRd',title.col ='Air Force Bases', border.lwd=0.2, border.col='gray',alpha=0.9, scale=0.5, legend.col.show = FALSE) +
  tm_shape(state_poly)+tm_borders(lwd=1.1,col='gray')
#Create ppp for Air Force Bases
air1 <-SpatialPointsDataFrame(coords=air[,2:3],data=data.frame(air),proj4string = CRS("+proj=utm"))
air2 <-as.ppp(air1)
base <-distmap(air2)
air3 <-ppp(x=air1$Latitude,y=air1$Longitude,window=as(state_poly,"owin"))
base1 <-distmap(air3)

#Pop Culture Sites
#File containing lat and long of pop culture sites
site <-read_excel("Pop Culture Sites.xlsx")
#Limiting Lat and Long to Continental US
site <- site[site$Latitude < 50,]
site <- site[site$Latitude > 25,]
site <- site[site$Longitude > -125,]
site <- site[site$Longitude < -65,]
site <- na.omit(site) #MO: taking out na's and blanks
#Map of Pop Culture Sites
site_dat<-st_as_sf(site, coords=c("Longitude","Latitude"), crs=CRS("+proj=utm"))
state_map<-map('state',fill=T,plot=F)
class(state_map)
state_poly <-map2SpatialPolygons(state_map, IDs=state_map$names, proj4string=CRS("+proj=utm"))
tm_shape(site_dat)+tm_symbols(col="Place",palette='YlOrRd',title.col ='Pop Culture Site', border.lwd=0.2, border.col='gray',alpha=0.9, scale=0.8) +
  tm_shape(state_poly)+tm_borders(lwd=1.1,col='gray')+tm_legend(position=c("right","bottom"),frame=TRUE,
                                                                main.title='UFO Pop Culture Sites in the Continental US')
#Create ppp for Pop Culture Sites
site1 <-SpatialPointsDataFrame(coords=site[,2:3],data=data.frame(site),proj4string = CRS("+proj=utm"))
site2 <-as.ppp(site1)
culture <-distmap(site2)
site3 <-ppp(x=site1$Longitude,y=site1$Latitude,window=as(state_poly,"owin"))
culture1 <-distmap(site3)

#Models
#Base Model
Model0 <-ppm(ufo2~1)
summary(Model0)

#Hypothesis 1 - Intensity over time accounting for population density
#Use year(quantitative) and month (categorical) together
#If interesting month pattern
#Same model for Hypothesis 1 and Hypothesis 2
#Density Plots - density over time - one panel per decade - show that it gets denser as time progresses
#Model with everything in it - crashed R and my computer
#Density Plots
#Density for Decades
ufo6 <-as.ppp(ufo1)
is.multitype(ufo6)
mark <-marks(ufo6)
mark$decade
marks(ufo6)<-mark$decade
ufo6
summary(ufo6)
plot.new()
par(mfrow=c(1,1))
plot(ufo6,which.marks="decade",main="Decade") 
UFODecade <- split(ufo6,which.marks="decade")
plot(UFODecade)
plot(density(UFODecade))
Model1<- ppm(ufo6 ~ marks + Cov5, covariates = list(Cov5=pop))#+ Cov5, covariates = list(Cov5=pop))
summary(Model1)

#Hypothesis 2 - Venus/Planet/Star Correlation
#Account for pop density
#use mark-light from below model as another answer/interpretation
ModelA<- ppm(ufo4 ~ marks + Cov5 -1, covariates = list(Cov5=pop))
summary(ModelA)

#Hypothesis 3 - Shapes - 
#Relrisk map
#Population density, shapes as mark
#Location on left side
#Is intensity equal for all marks
Model2 <-ppm(ufo2 ~ marks + Cov5 -1, covariates = list(Cov5=pop))
summary(Model2) #Discuss the mark-saucer piece of this - highest intensity - influenced by pop culture (movies, tv shows, etc.)
#Relrisk map to illustrate
risk <-relrisk(ufo2,window=as(state_poly,"owin"))
plot(risk,main = "Relative Risk Plot")#,window=as(state_poly,"owin"))

#Hypothesis 4 - Distance from pop culture sites/air force bases
#Break into two models (account for pop density for both)
#Some bases are around population centers, but pop culture sites aren't 
#Model of culture sites
Model3 <-ppm(ufo2 ~ Cov5 + Cov6, covariates = list(Cov5=pop,Cov6=culture1))
summary(Model3)
#Model with air force bases 
Model4 <-ppm(ufo2 ~ Cov5 + Cov7, covariates = list(Cov5=pop,Cov7=base))
summary(Model4)
# Use maps of pop cutlure sites and air force bases as visual aids for this hypothesis
