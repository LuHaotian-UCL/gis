library(dplyr)
library(tidyverse)
library(janitor)
library(tmap)
library(tmaptools)
library(sf)

#read csv
SchoolReport <- read_csv('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK2/wk2_homework/Report_Card_Assessment_Data_2018-19_School_Year_20251007.csv',
                       locale = locale(encoding = "latin1"),
                       na = "NULL")

#检查数据
Datatypelist <- SchoolReport %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")
Datatypelist

#清洗名称
SchoolReport_nameclear <- SchoolReport %>%
  clean_names()

#取出参与考试的学生总数、达到要求的学生数
SchoolReport_contains<-SchoolReport_nameclear %>% 
  dplyr::select(contains("county"),
                contains("count_of_students_expected_to_test"), 
                contains("count_met_standard")) 

#计算通过学生的百分比
Passed_students_percent <- SchoolReport_contains %>% 
  #new column
  mutate(passedPercent= count_met_standard/count_of_students_expected_to_test_including_previously_passed)%>%
  #select only columns we want
  
  # dplyr::select(new_code,
  #               borough,
  #               averagelifeexpectancy, 
  #               normalisedlifeepectancy)%>%
  
  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)
  arrange(desc(passedPercent))

#打开shp并查看
WashingCounties <- st_read('/Volumes/ExFAT 400G/Documents/TERM1/05_GIS/DATA/WEEK2/wk2_homework/Washington_Counties_with_Natural_Shoreline___washsh_area/Washington_Counties_with_Natural_Shoreline___washsh_area.shp')
qtm(WashingCounties)

#再次清洗csv中的列名
Passed_students_percent <- clean_names(Passed_students_percent)

#对Passed_students_percent表进行分组，并计算百分比的平均值
Passed_students_percent_group <- Passed_students_percent %>%
  group_by(county) %>%
  summarise(
    avg_passed_percent = mean(passed_percent, na.rm = TRUE)
  )

#计算整个地区的平均值
overall_avg_passed_percent <- mean(Passed_students_percent_group$avg_passed_percent, na.rm = TRUE)

#得到每个county与平均值比较的结果
comparison <- Passed_students_percent_group %>%
  mutate(compare = case_when(
    avg_passed_percent >= overall_avg_passed_percent ~ 1,
    avg_passed_percent < overall_avg_passed_percent ~ 0
  ))

#连接数据
WashingDataMap <- WashingCounties %>%
  clean_names()%>%
  # the . here just means use the data already loaded
  #filter(str_detect(lad15cd, "^E09"))%>%
  merge(.,
        comparison, 
        by.x="countylabe", 
        by.y="county",
        no.dups = TRUE)%>%
  distinct(.,countylabe,
           .keep_all = TRUE)

# #进行简单的可视化
# tmap_mode("plot")
# qtm(WashingDataMap, 
#     fill = "compare")

tmap_mode("plot")
tm_shape(WashingDataMap) +
  tm_polygons(
    col = "compare",
    style = "fixed",
    breaks = c(-Inf, 0.5, Inf),            # 0 和 1 两段
    labels = c("Below avg", "At/Above avg"),
    palette = c("#FFA500", "#3182bd"),
    title = "Compare to regional avg",
  ) +
  tm_layout(legend.outside = TRUE)
