library(sf)
library(raster)
library(terra)
library(fs)
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)

st_layers('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK3/homework_wk3/gadm41_CHN.gpkg')

#读入中国行政区边界并检查
CHNoutline <- st_read('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK3/homework_wk3/gadm41_CHN.gpkg', 
                      layer='ADM_ADM_0')
print(CHNoutline)
st_crs(CHNoutline)$proj4string

#读入两个最高温tif并检查
jan1<-terra::rast('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK3/homework_wk3/wc2.1_2.5m_tmax_HadGEM3-GC31-LL_ssp126_2081-2100.tif')
jan2<-terra::rast('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK3/homework_wk3/wc2.1_2.5m_tmax_HadGEM3-GC31-LL_ssp585_2081-2100.tif')
jan1
jan2
plot(jan1)
plot(jan2)

#两tif相减并保存到电脑
tmax_diff_tif <- jan2 - jan1
print(tmax_diff_tif)
terra::writeRaster(tmax_diff_tif,
                   "/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK3/homework_wk3/tmax_diff_global.tif",
                   overwrite = TRUE)

#查看文件夹
dir_info("/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK3/homework_wk3/")

#抓取所有tif的文件路径并查看
listfiles<-dir_info("/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK3/homework_wk3/") %>%
  #只保留路径中包含 .tif 的文件
  filter(str_detect(path, "tmax_diff_global.tif")) %>%
  #只保留 path 这一列
  dplyr::select(path)%>%
  #把数据框的一列提取出来，变成一个向量
  pull()
listfiles

#把多个tif合成一个
worldtmax <- listfiles %>%
  terra::rast()
#have a look at the raster stack
worldtmax

#给每个tif重命名并检查
categories <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(worldtmax) <- categories
worldtmax$Jan

#开始处理全球主要城市坐标数据
#读入全球城市 CSV
cities <- read_csv('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK3/homework_wk3/World_Cities.csv',
                   locale = locale(encoding = "latin1"),
                   na = "n/a")

#选出中国的城市（选择行）
CHNcities <- cities %>%
  filter(str_detect(`CNTRY_NAME`, "China"))

#提取有用的列
CHNcities_selected<-CHNcities %>% 
  dplyr::select("CITY_NAME", 
                "X",
                "Y") 

#先告诉 R 这些点是按米算的 → 把它们换算成地球经纬度 → 再把新坐标存进表格
#将原单位m转为经纬度
pts_3857 <- st_as_sf(CHNcities_selected, coords = c("X","Y"), crs = 3857)
pts_4326 <- st_transform(pts_3857, 4326)   # ← 现在是经纬度（度）

#保存为一个新的表格，没有城市名
lonlat <- st_coordinates(pts_4326)
CHNcities_lonlat <- CHNcities_selected %>%
  mutate(lon = lonlat[,1],
         lat = lonlat[,2]) %>%
  dplyr::select(lon, lat)

#将城市名称提取成变量
city_names <- CHNcities_selected$CITY_NAME

#根据刚才的经纬度表来提取对应位置的温度
CHNcitytempdiff <- terra::extract(worldtmax, CHNcities_lonlat)

#插入刚才提取的城市名称
CHNcitytempdiff_name <- CHNcitytempdiff %>% 
  as_tibble()%>% 
  add_column(City = city_names, .before = "Jan")

#计算每月的SSP1和SSP2温度差

#选出上海的数据并作图
Shanghaitemp <- CHNcitytempdiff_name %>%
  filter(City=="Shanghai")
#选出用于作图的列
t<-Shanghaitemp %>%
  dplyr::select(Jan:Dec)
#作图
hist((as.numeric(t)), 
     #breaks=userbreak, 
     col="red", 
     main="Histogram of Shanghai Temperature", 
     xlab="Temperature", 
     ylab="Frequency")

#简化中国的shp
CHNoutSIMPLE <- CHNoutline %>%
  st_simplify(., dTolerance = 1000) %>%
  st_geometry()%>%
  plot()

#打印查看坐标系是否相同
print(CHNoutline)
crs(worldtmax)

#将矢量根据shp范围剪裁
#外接矩形
CHNtemp <- CHNoutline %>%
  terra::crop(worldtmax,.)
  #terra::mask(worldtmax,.)
#掩膜
exactCHN <- terra::mask(CHNtemp, CHNoutline)
plot(exactCHN)

#中国三月气温可视化
hist(exactCHN[[3]], col="red", main ="March temperature")

#所有月份打印
exactCHNdf <- exactCHN %>%
  as.data.frame()

squishdata<-exactCHNdf%>%
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )

data_complete_cases <- squishdata %>%
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))

# Plot faceted histogram
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="green", fill="black", binwidth = 0.5)+
  labs(title="2081-2100 China temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))