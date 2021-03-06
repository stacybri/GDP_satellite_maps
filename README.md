# GDP_satellite_maps
This is some R code to produce maps of GDP/km^2.  Example data is for the country of Peru.  

This is a personal project and should not be seen as an official World Bank project.

This is example code to produce a raster map of the GDP per Square km using a World Bank raster map from DECRG.
This could be useful for examining lack of resources for schools for the Global Education Policy Dashboard project

Two basic plots are done: one using ggplot and another using leaflet

# Data downloaded from Here:
https://datacatalog.worldbank.org/dataset/gross-domestic-product-2010

also see

https://preview.grid.unep.ch/index.php?preview=data&events=socec&evcat=1&lang=eng

 In the distributed global GDP dataset sub-national GRP and national GDP data are allocated to
 30 arc second (approximately 1km) grid cells in proportion to the population residing in that cell.
 The method also distinguishes between rural and urban population, assuming the latter to have a higher
 GDP per capita. Input data are from 1) a global time-series dataset of GDP, with subnational gross regional
 product (GRP) for 74 countries, compiled by the World Bank Development Economics Research Group (DECRG). 2)
 Gridded population projections for the year 2009, based on a population grid for the year 2005 provided by
 LandScanTM Global Population Database (Oak Ridge, TN: Oak Ridge National Laboratory). This dataset has been
 extrapolated to year 2010 by UNEP/GRID-Geneva. Unit is estimated value of production per cell, in thousand of
 constant 2000 USD. Cell level anomalies may occur due to poor alignment of multiple input data sources, and it
 is strongly recommended that users attempt to verify information, or consult original sources, in order to determine
 suitability for a particular application. This product was compiled by DECRG for the Global Assessment Report on Risk
 Reduction (GAR). It was modeled using global data. Credit: GIS processing World Bank DECRG, Washington, DC,
 extrapolation UNEP/GRID-Geneva.
