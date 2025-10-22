library(sf)
library(dplyr)
library(readr)
library(tmap)

#读取全球范围shp
worldshp <- st_read('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK4/World_Countries_(Generalized)_-573431906301700955/World_Countries_Generalized.shp')

#转换坐标系并查看基本信息
worldshp84 <- st_transform(worldshp, 4326)
st_crs(worldshp84)
names(worldshp84)

#提取有用的列
world_clean <- worldshp84 %>%
  select(iso = ISO, country = COUNTRY, geometry)
head(world_clean)

#读取csv
data2010 <- read_csv('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK4/2010.csv',
                       locale = locale(encoding = "latin1"),
                       na = "n/a")
data2019 <- read_csv('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK4/2019.csv',
                     locale = locale(encoding = "latin1"),
                     na = "n/a")

#取出csv中需要的列
gii2010 <- data2010 %>%
  select(iso = countryIsoCode, country, GII_2010 = value)
gii2019 <- data2019 %>%
  select(iso = countryIsoCode, country, GII_2019 = value)

#连接两个csv
differencedata <- gii2010 %>%
  merge(
    .,
    gii2019,
    by.x="iso",
    by.y="iso"
  ) %>%
  mutate(diff_2010_2019 = GII_2019 - GII_2010)

#和shp连接
mapdata <- world_clean %>%
  merge(
    .,
    differencedata,
    by.x="country",
    by.y="country.x"
  )

#简单出图
tmap_mode("plot")
# change the fill to your column name if different
mapdata %>%
  qtm(.,fill = "diff_2010_2019")


#
tmap_mode("plot")

tm_shape(mapdata) +
  tm_polygons(
    col = "diff_2010_2019",
    palette = "-RdYlBu",               # 漂亮的红-黄-蓝发散配色，红=增加，蓝=下降
    border.col = "grey60",             # 国家边界浅灰
    lwd = 0.3,                         # 边界线细一点
    title = "Difference"
  ) +
  tm_layout(
    title = "Global Gender Inequality Index Change (2010–2019)",
    title.size = 1.2,
    title.position = c("center", "top"),
    frame = FALSE,
    legend.position = c("left", "bottom"), # 图例放图内左下角
    legend.bg.color = "white",             # 图例背景白色
    legend.bg.alpha = 0.8,                 # 半透明背景
    legend.text.size = 0.8,
    legend.title.size = 0.9,
    inner.margins = c(0.05, 0.05, 0.1, 0.1), # 留一点边距
    asp = 0                                # 自适应比例
  ) +
  tm_compass(
    type = "8star",
    size = 2,
    position = c("left", "top")
  ) +
  tm_scale_bar(
    position = c("right", "bottom")
  )