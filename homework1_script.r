library(sf)
library(tmap) 
library(tmaptools)
library(RSQLite)
library(tidyverse)
#read in the shapefile

shape <- st_read(
  '/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK1/homework/statsnz-territorial-authority-2018-generalised-SHP/territorial-authority-2018-generalised.shp')
# read in the csv
mycsv <- read_csv('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK1/homework/2018paidemployee.csv')  
# merge csv and shapefile
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="TA2018_V1_", 
        by.y="code")
# set tmap to plot
tmap_mode("plot")
# have a look at the map
#qtm(shape, fill = "2018 paid employee")
print(qtm(shape, fill = "2018 paid employee"))
# write to a .gpkg
shape %>%
  st_write(.,'/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK1/homework/NZland_paidemployee_R.gpkg',
           "NZland_paidemployee",
           delete_layer=TRUE)
# connect to the .gpkg
con <- dbConnect(SQLite(),dbname='/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK1/homework/NZland_paidemployee_R.gpkg')
# list what is in it
con %>%
  dbListTables()
# add the original .csv
con %>%
  dbWriteTable(.,
               "original_csv",
               mycsv,
               overwrite=TRUE)
# disconnect from it
con %>% 
  dbDisconnect()
