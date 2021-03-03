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
