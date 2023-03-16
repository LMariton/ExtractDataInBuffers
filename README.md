# ExtractDataInBuffers
Scripts to extract data in buffers around points

### ExtractingDensityLengthBuffers :
This script extracts the total length and the density of linear features 
(ex : roads or rivers) among buffers around points.
It returns a data table containing the point IDs and the values obtained
for the total length (in km) and the density (in km/km²) among buffers.

### ExtractingAreaBuffers
This script extracts the total area of polygone features 
(ex : lakes) among buffers around points.
It returns a data table containing the point IDs and the values obtained
for the area (in km²) among buffers.

### ExtractingLightPollution
This script extracts the radiance (nW/cm2/sr) value at given sites and the
mean radiance value in buffers around these sites.
It returns the table given as an argument with one new column for the radiance
value at the sites and as many new columns as the number of buffers given.

### ExtractingShannonIndex
This script extracts the Shannon index for given sites among different buffer size.
It returns the table given as an argument with new columns containing Shannon indexes
for each buffer size given (Shannon_XXX, XXX buffer size).

### Warning:
**These scripts are « functions »**, to use them you have to download them and keep the R. files somewhere on your computer.  

In theory, you don’t even need to open them with R, however I strongly advise you to do so to read the first lines presenting what the function does and how to use it (in particular what the arguments of the function are and how your dataset should be structured). 

In your script, simply add the following command:  
`source(“pathToTheRFile”)`  
Then you can use the function!  

Here is an example of what your script should look like: 

`sitesSurveyed <- read.csv(“C:/Users/Name/Document/sitesStudied.csv”)`  
`bufferSizes <- c(100,200,500,1000)`  
`pathToLayer <- “C:/Users/Name/Document/waterPolygons.shp”`  

`source("C:/Users/Name/Document/ExtractingAreaBuffers.R")`  
`areaWaterSites <- Calc_area_in_buffers(sitesSurveyed,”Code_site”,bufferSizes,pathToLayer)`
