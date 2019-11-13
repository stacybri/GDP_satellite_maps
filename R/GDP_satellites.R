#Purpose:  This is example code to produce a raster map of the GDP per Square km using World Bank raster map fro DECRG.
# This could be useful for examining lack of resources for schools for the Global Education Policy Dashboard project
#Author: Brian Stacy
#Date: 11/12/2019

#read in packages
library(tidyverse)
library(rgdal)
library(raster)
library(leaflet)

#Data downloaded from Here:
#https://datacatalog.worldbank.org/dataset/gross-domestic-product-2010 also see
#https://preview.grid.unep.ch/index.php?preview=data&events=socec&evcat=1&lang=eng
# In the distributed global GDP dataset sub-national GRP and national GDP data are allocated to 
# 30 arc second (approximately 1km) grid cells in proportion to the population residing in that cell. 
# The method also distinguishes between rural and urban population, assuming the latter to have a higher 
# GDP per capita. Input data are from 1) a global time-series dataset of GDP, with subnational gross regional 
# product (GRP) for 74 countries, compiled by the World Bank Development Economics Research Group (DECRG). 2) 
# Gridded population projections for the year 2009, based on a population grid for the year 2005 provided by 
# LandScanTM Global Population Database (Oak Ridge, TN: Oak Ridge National Laboratory). This dataset has been 
# extrapolated to year 2010 by UNEP/GRID-Geneva. Unit is estimated value of production per cell, in thousand of 
# constant 2000 USD. Cell level anomalies may occur due to poor alignment of multiple input data sources, and it 
# is strongly recommended that users attempt to verify information, or consult original sources, in order to determine 
# suitability for a particular application. This product was compiled by DECRG for the Global Assessment Report on Risk 
# Reduction (GAR). It was modeled using global data. Credit: GIS processing World Bank DECRG, Washington, DC, 
# extrapolation UNEP/GRID-Geneva.

######################
# Read in Data
######################

#file directory
download_dir <- "C:/Users/wb469649/Documents/Github/GDP_satellite_maps/Data"


#basic info on tiff file
GDALinfo(paste(download_dir, "GDP.tif", sep="/"))

#open the raster
gdp_raster <- raster(paste(download_dir, "GDP.tif", sep="/"))
summary(gdp_raster)

#convert to dataframe for ggplot
gdp_raster_df <- as.data.frame(gdp_raster, xy=TRUE)

###############################
#plot the GDP data using ggplot
###############################
gdp_plot <- ggplot() +
  geom_raster(data=gdp_raster_df, aes(x=x, y=y, 
                                      fill=cut(GDP, breaks=c(0,5,10,20,30,40,50,100,10000000)))) +
  scale_fill_brewer( palette='RdYlGn') +
  labs(fill="GDP per Sq km") +
  coord_quickmap() +
  ggtitle("Columbia - 2010 - GDP per Square km")

gdp_plot


###################
#plot using leaflet
###################
#color palette
values(gdp_raster)[values(gdp_raster)<=0]=NA

#create a pallete with specified bins
pal <- colorBin("RdYlBu", domain = c(0,100000), bins=c(0,5,10,20,30,40,50,100,10000000), 
                    na.color = "transparent")


gdp_leaflet <- leaflet() %>%
  addTiles() %>%
  addRasterImage(gdp_raster, colors=pal, opacity=0.4) %>%
  addLegend(pal=pal, values=values(gdp_raster), title="GDP per Square km") 

gdp_leaflet


####################
# Merge data to set of geo coordinates
####################

#create a random set of geo-codes plus a random variable to plot against
df <- data.frame(
  x = rnorm(n=25, mean=-72, sd=1),
  y = rnorm(n=25, mean=5, sd=1),
  regressor= rnorm(n=25, mean=0, sd=1)
)

# plot the data to see where the points are
gdp_plot2 <- ggplot() +
  geom_raster(data=gdp_raster_df, aes(x=x, y=y, 
                                      fill=cut(GDP, breaks=c(0,5,10,20,30,40,50,100,10000000)))) +
  geom_point(data=df, aes(x=x, y=y)) + 
  scale_fill_brewer( palette='RdYlGn') +
  labs(fill="GDP per Sq km") +
  coord_quickmap() +
  ggtitle("Columbia - 2010 - GDP per Square km")

gdp_plot2

# Now merge GDP data from satellites onto our made up data
coordinates(df) <- c("x","y")

df$GDP <- raster::extract(gdp_raster, df, 
                  buffer=1000, # 1000m radius
                  fun=mean,na.rm=T,
                  method='simple')

df <- as.data.frame(df) 
#plot the GDP data against the regressor

ggplot(data=df, aes(x=log(GDP), y=regressor)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle('Plot of Regressor on GDP Measured Using Satellite Data')
