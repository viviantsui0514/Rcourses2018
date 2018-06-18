library(ggmap) 
library(mapproj)
library(ggplot2)

library(readxl)

dengue <- read_excel("C:/Users/vivian/Desktop/Dengue_Daily.xlsx")
as.numeric(dengue$lon)
as.numeric(dengue$lat)

dengueloc <- subset(dengue, Country == "中華民國" )
denguefor <-subset(dengue, Country != "中華民國" )

#2017年度全台本土病例分布
dengueloc2017 <- subset(dengueloc, Year == 2017)
library(leaflet)
library(magrittr)
leaflet(dengueloc2017) %>% 
  addTiles() %>% 
  addCircleMarkers(~dengueloc2017$lon, ~dengueloc2017$lat, radius = 1, fillOpacity = 0.5)

#2017年度全台境外病例分布
denguefor2017 <- subset(denguefor, Year == 2017)
leaflet(denguefor2017) %>% 
  addTiles() %>% 
  addCircleMarkers(~denguefor2017$lon, ~denguefor2017$lat, radius = 1, fillOpacity = 0.5)

#2016年度全台本土病例分布
dengueloc2016 <- subset(dengueloc, Year == 2016)
leaflet(dengueloc2016) %>% 
  addTiles() %>% 
  addCircleMarkers(~dengueloc2016$lon, ~dengueloc2016$lat, radius = 1, fillOpacity = 0.5)

#2016年度全台境外病例分布
denguefor2016 <- subset(denguefor, Year == 2016)
leaflet(denguefor2016) %>% 
  addTiles() %>% 
  addCircleMarkers(~denguefor2016$lon, ~denguefor2016$lat, radius = 1, fillOpacity = 0.5)

#2015年度全台本土病例分布
dengueloc2015 <- subset(dengueloc, Year == 2015)
leaflet(dengueloc2015) %>% 
  addTiles() %>% 
  addCircleMarkers(~dengueloc2015$lon, ~dengueloc2015$lat, radius = 1, fillOpacity = 0.5)

#2015年度全台境外病例分布
denguefor2015 <- subset(denguefor, Year == 2015)
leaflet(denguefor2015) %>% 
  addTiles() %>% 
  addCircleMarkers(~denguefor2015$lon, ~denguefor2015$lat, radius = 1, fillOpacity = 0.5)

#2014年度全台本土病例分布
dengueloc2014 <- subset(dengueloc, Year == 2014)
leaflet(dengueloc2014) %>% 
  addTiles() %>% 
  addCircleMarkers(~dengueloc2014$lon, ~dengueloc2014$lat, radius = 1, fillOpacity = 0.5)

#2014年度全台境外病例分布
denguefor2014 <- subset(denguefor, Year == 2014)
leaflet(denguefor2014) %>% 
  addTiles() %>% 
  addCircleMarkers(~denguefor2014$lon, ~denguefor2014$lat, radius = 1, fillOpacity = 0.5)

#2013年度全台本土病例分布
dengueloc2013 <- subset(dengueloc, Year == 2013)
leaflet(dengueloc2013) %>% 
  addTiles() %>% 
  addCircleMarkers(~dengueloc2013$lon, ~dengueloc2013$lat, radius = 1, fillOpacity = 0.5)

#2013年度全台境外病例分布
denguefor2013 <- subset(denguefor, Year == 2013)
leaflet(denguefor2013) %>% 
  addTiles() %>% 
  addCircleMarkers(~denguefor2013$lon, ~denguefor2013$lat, radius = 1, fillOpacity = 0.5)


#年度台南病例分布(2013-2017)

#2017年度台南境外病例分布
Tainanfor2017 <- subset(denguefor2017, City=="台南市")
leaflet(Tainanfor2017) %>% 
  addTiles() %>% 
  addCircleMarkers(~Tainanfor2017$lon, ~Tainanfor2017$lat, radius = 1, fillOpacity = 0.5)

#2017年度台南本土病例分布
Tainanloc2017 <- subset(dengueloc2017, City=="台南市")
leaflet(Tainanloc2017) %>% 
  addTiles() %>% 
  addCircleMarkers(~Tainanloc2017$lon, ~Tainanloc2017$lat, radius = 1, fillOpacity = 0.5)

#2016年度台南境外病例分布
Tainanfor2016 <- subset(denguefor2016, City=="台南市")
leaflet(Tainanfor2016) %>% 
  addTiles() %>% 
  addCircleMarkers(~Tainanfor2016$lon, ~Tainanfor2016$lat, radius = 1, fillOpacity = 0.5)

#2016年度台南本土病例分布
Tainanloc2016 <- subset(dengueloc2016, City=="台南市")
leaflet(Tainanloc2016) %>% 
  addTiles() %>% 
  addCircleMarkers(~Tainan2016loc$lon, ~Tainan2016loc$lat, radius = 1, fillOpacity = 0.5)
#2015年度台南境外病例分布
Tainanfor2015 <- subset(denguefor2015, City=="台南市")
leaflet(Tainanfor2015) %>% 
  addTiles() %>% 
  addCircleMarkers(~Tainanfor2015$lon, ~Tainanfor2015$lat, radius = 1, fillOpacity = 0.5)
#2015年度台南本土病例分布
Tainanloc2015 <- subset(dengueloc2015, City=="台南市")
leaflet(Tainanloc2015) %>% 
  addTiles() %>% 
  addCircleMarkers(~Tainanloc2015$lon, ~Tainanloc2015$lat, radius = 1, fillOpacity = 0.5)
#2014年度台南境外病例分布
Tainanfor2014 <- subset(denguefor2014, City=="台南市")
leaflet(Tainanfor2014) %>% 
  addTiles() %>% 
  addCircleMarkers(~Tainanfor2014$lon, ~Tainanfor2014$lat, radius = 1, fillOpacity = 0.5)
#2014年度台南本土病例分布
Tainanloc2014 <- subset(dengueloc2014, City=="台南市")
leaflet(Tainanloc2014) %>% 
  addTiles() %>% 
  addCircleMarkers(~Tainanloc2014$lon, ~Tainanloc2014$lat, radius = 1, fillOpacity = 0.5)
#2013年度台南境外病例分布
Tainanfor2013 <- subset(denguefor2013, City=="台南市")
leaflet(Tainanfor2013) %>% 
  addTiles() %>% 
  addCircleMarkers(~Tainanfor2013$lon, ~Tainanfor2013$lat, radius = 1, fillOpacity = 0.5)
#2013年度台南本土病例分布
Tainanloc2013 <- subset(dengueloc2013, City=="台南市")
leaflet(Tainanloc2013) %>% 
  addTiles() %>% 
  addCircleMarkers(~Tainanloc2013$lon, ~Tainanloc2013$lat, radius = 1, fillOpacity = 0.5)

