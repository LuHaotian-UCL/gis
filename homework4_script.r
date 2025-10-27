library(sf)
library(dplyr)
library(readr)
library(tmap)

#read the worldwide shp
worldshp <- st_read('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK4/World_Countries_(Generalized)_-573431906301700955/World_Countries_Generalized.shp')

#transform crs and check it out
worldshp84 <- st_transform(worldshp, 4326)
st_crs(worldshp84)
names(worldshp84)

#extract the necessary cols
world_clean <- worldshp84 %>%
  select(iso = ISO, country = COUNTRY, geometry)
head(world_clean)

#read the csv
data2010 <- read_csv('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK4/2010.csv',
                       locale = locale(encoding = "latin1"),
                       na = "n/a")
data2019 <- read_csv('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK4/2019.csv',
                     locale = locale(encoding = "latin1"),
                     na = "n/a")

#extract cols
gii2010 <- data2010 %>%
  select(iso = countryIsoCode, country, GII_2010 = value)
gii2019 <- data2019 %>%
  select(iso = countryIsoCode, country, GII_2019 = value)

#caculate the difference
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

#static map
tmap_mode("plot")

tm_shape(mapdata) +
  tm_polygons(
    col = "diff_2010_2019",
    palette = "-RdYlBu",               
    border.col = "grey60",           
    lwd = 0.3,                       
    title = "Difference",
    style = "jenks",
    showNA = FALSE
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




#dynamic map
tmap_mode("view")

tm_shape(mapdata) + 
  tm_polygons(
    fill = "diff_2010_2019",
    palette = "Reds",
    style = "jenks",
    alpha = 0.7,
    id = "country",
    popup.vars = c(
      "Country" = "country",
      "Data 2010" = "GII_2010",
      "Data 2019" = "GII_2019",
      "Diff" = "diff_2010_2019"
    ),
    title = "Diff (2010 −> 2019)"
  ) +
  tm_basemap(server = "OpenStreetMap") +
  tm_compass(type = "arrow", position = c("left", "bottom")) + 
  tm_scalebar(position = c("left", "bottom")) +
  tm_title("Change in Gender Inequality Index (2010 -> 2019)",
           size = 2,
           position = c("center", "top"))


