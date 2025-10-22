library(sf)
library(dplyr)
library(readr)
library(tmap)

#read shp
worldshp <- st_read('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK4/World_Countries_(Generalized)_-573431906301700955/World_Countries_Generalized.shp')

#Transform coordinate systems and review information
worldshp84 <- st_transform(worldshp, 4326)
st_crs(worldshp84)
names(worldshp84)

#Extract relevant columns
world_clean <- worldshp84 %>%
  select(iso = ISO, country = COUNTRY, geometry)
head(world_clean)

#read csv
data2010 <- read_csv('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK4/2010.csv',
                       locale = locale(encoding = "latin1"),
                       na = "n/a")
data2019 <- read_csv('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK4/2019.csv',
                     locale = locale(encoding = "latin1"),
                     na = "n/a")

#Extract the required columns from the CSV file
gii2010 <- data2010 %>%
  select(iso = countryIsoCode, country, GII_2010 = value)
gii2019 <- data2019 %>%
  select(iso = countryIsoCode, country, GII_2019 = value)

#join the two csv
differencedata <- gii2010 %>%
  merge(
    .,
    gii2019,
    by.x="iso",
    by.y="iso"
  ) %>%
  mutate(diff_2010_2019 = GII_2019 - GII_2010)

#connect with shp
mapdata <- world_clean %>%
  merge(
    .,
    differencedata,
    by.x="country",
    by.y="country.x"
  )

#mapping
tmap_mode("plot")
# change the fill to your column name if different
mapdata %>%
  qtm(.,fill = "diff_2010_2019")


#A more beautiful map
tmap_mode("plot")

tm_shape(mapdata) +
  tm_polygons(
    col = "diff_2010_2019",
    palette = "-RdYlBu",               
    border.col = "grey60",             
    lwd = 0.3,                        
    title = "Difference"
  ) +
  tm_layout(
    title = "Global Gender Inequality Index Change (2010–2019)",
    title.size = 1.2,
    title.position = c("center", "top"),
    frame = FALSE,
    legend.position = c("left", "bottom"), 
    legend.bg.color = "white",             
    legend.bg.alpha = 0.8,                 
    legend.text.size = 0.8,
    legend.title.size = 0.9,
    inner.margins = c(0.05, 0.05, 0.1, 0.1), 
    asp = 0                                
  ) +
  tm_compass(
    type = "8star",
    size = 2,
    position = c("left", "top")
  ) +
  tm_scale_bar(
    position = c("right", "bottom")
  )
