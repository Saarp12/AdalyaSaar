###MAP_VIEWER_RASHUIOT###
if (!require("pacman")) install.packages("pacman")
pacman::p_install(tidyverse,systemfit, esquisse, data.table, readxl, usethis)
remotes::install_github("r-spatial/mapview")

library(stringr)
library(haven)
library(readxl)
library(tidyverse)
library(mapview)
library(leafsync)
library(leaflet)
library(glue)
library(htmltools)
library(sf)
library(RColorBrewer)
library(leafpop)
library(Rcpp)

#install.packages("mapview", dependencies = TRUE, repos = "http://cran.rstudio.com/")
#changing mapview option & returning to default
#map types : https://leaflet-extras.github.io/leaflet-providers/preview/
#Data Organizing
database <- as.data.frame(read_xlsx("C:/Users/SaarP/Box/צוות תעסוקה/סוכנות לעסקים קטנים/סקרים/מדד ידידות רשויות מקומיות/2022/סקר שביעות רצון/DashBoard data.xlsx"))
data_big <- database %>% filter(size == "Big") %>% select(-size,-`ציון 2019`,-`ציון 2021`,-`ציון 2022`)
data_mid <- database %>% filter(size == "Mid") %>% select(-size,-`ציון 2019`,-`ציון 2021`,-`ציון 2022`)
data_small <- database %>% filter(size == "Small") %>% select(-size,-`ציון 2019`,-`ציון 2021`,-`ציון 2022`)

#Mapview options:
mapviewOptions(basemaps = c("CartoDB.Positron","OpenStreetMap","CartoDB.VoyagerLabelsUnder","CartoDB.VoyagerNoLabels","CartoDB.PositronNoLabels"), legend = TRUE,homebutton = FALSE)
##mapviewOptions(default=TRUE)##

mypalette <- colorRampPalette(c("green4", "gold1","firebrick3"))
data_small$ארנונה

#Map for small cities:
dirugmap<-mapview(data_small, xcol="longitude", ycol = "latitude",zcol = "דירוג כולל",crs = 4269, grid = FALSE,
                  label = "רשות", col.regions = mypalette(7),at = seq(0,35,5),layer.name = "דירוג רשויות", legend = FALSE, 
                  popup = popupTable(data_small, zcol = c("רשות","דירוג כולל", "שביעות רצון", "נגישות המידע","ארנונה", "מכרזים"),
                                     feature.id = FALSE,row.numbers = FALSE,className = "רשות"))
sviutmap<-mapview(data_small, xcol="longitude", ycol = "latitude",zcol = "שביעות רצון",crs = 4269, grid = FALSE,
                  label = "רשות", col.regions = mypalette(7),at = seq(0,35,5),layer.name = "דירוג שביעות רצון", legend = FALSE,
                  popup = popupTable(data_small, zcol = c("רשות","דירוג כולל", "שביעות רצון", "נגישות המידע","ארנונה", "מכרזים"),
                                     feature.id = FALSE,row.numbers = FALSE,className = "רשות"))
negishutmap<-mapview(data_small, xcol="longitude", ycol = "latitude",zcol = "נגישות המידע",crs = 4269, grid = FALSE,
                     label = "רשות", col.regions = mypalette(7),at = seq(0,35,5),layer.name = "דירוג נגישות המידע", legend = FALSE,
                     popup = popupTable(data_small, zcol = c("רשות","דירוג כולל", "שביעות רצון", "נגישות המידע","ארנונה", "מכרזים"),
                                        feature.id = FALSE,row.numbers = FALSE,className = "רשות"))
arnonamap<-mapview(data_small, xcol="longitude", ycol = "latitude",zcol = "ארנונה",crs = 4269, grid = FALSE,
                   label = "רשות", col.regions = mypalette(7),at = seq(0,35,5),layer.name = "דירוג ארנונה", legend = FALSE,
                   popup = popupTable(data_small, zcol = c("רשות","דירוג כולל", "שביעות רצון", "נגישות המידע","ארנונה", "מכרזים"),
                                      feature.id = FALSE,row.numbers = FALSE,className = "רשות"))
michrazimap<-mapview(data_small, xcol="longitude", ycol = "latitude",zcol = "מכרזים",crs = 4269, grid = FALSE,
                     label = "רשות", col.regions = mypalette(7),at = seq(0,35,5),layer.name = "דירוג מכרזים",legend = FALSE,
                     popup = popupTable(data_small, zcol = c("רשות","דירוג כולל", "שביעות רצון", "נגישות המידע","ארנונה", "מכרזים"),
                                        feature.id = FALSE,row.numbers = FALSE,className = "רשות"))


small_map <- dirugmap + sviutmap +  negishutmap + arnonamap + michrazimap



dirugmap1<-mapview(data_mid, xcol="longitude", ycol = "latitude",zcol = "דירוג כולל",crs = 4269, grid = FALSE,
                  label = "רשות", col.regions = mypalette(6),at = seq(0,30,5),layer.name = "דירוג רשויות", legend = FALSE,
                  popup = popupTable(data_mid, zcol = c("רשות","דירוג כולל", "שביעות רצון", "נגישות המידע","ארנונה", "מכרזים"),
                                     feature.id = FALSE,row.numbers = FALSE,className = "רשות"))


sviutmap1<-mapview(data_mid, xcol="longitude", ycol = "latitude",zcol = "שביעות רצון",crs = 4269, grid = FALSE,
                  label = "רשות", col.regions = mypalette(6),at = seq(0,30,5),layer.name = "דירוג שביעות רצון", legend = FALSE,
                  popup = popupTable(data_mid, zcol = c("רשות","דירוג כולל", "שביעות רצון", "נגישות המידע","ארנונה", "מכרזים"),
                                     feature.id = FALSE,row.numbers = FALSE,className = "רשות"))

negishutmap1<-mapview(data_mid, xcol="longitude", ycol = "latitude",zcol = "נגישות המידע",crs = 4269, grid = FALSE,
                     label = "רשות", col.regions = mypalette(6),at = seq(0,30,5),layer.name = "דירוג נגישות המידע", legend = FALSE,
                     popup = popupTable(data_mid, zcol = c("רשות","דירוג כולל", "שביעות רצון", "נגישות המידע","ארנונה", "מכרזים"),
                                        feature.id = FALSE,row.numbers = FALSE,className = "רשות"))

arnonamap1<-mapview(data_mid, xcol="longitude", ycol = "latitude",zcol = "ארנונה",crs = 4269, grid = FALSE,
                   label = "רשות", col.regions = mypalette(6),at = seq(0,30,5),layer.name = "דירוג ארנונה", legend = FALSE,
                   popup = popupTable(data_mid, zcol = c("רשות","דירוג כולל", "שביעות רצון", "נגישות המידע","ארנונה", "מכרזים"),
                                      feature.id = FALSE,row.numbers = FALSE,className = "רשות"))

michrazimap1<-mapview(data_mid, xcol="longitude", ycol = "latitude",zcol = "מכרזים",crs = 4269, grid = FALSE,
                     label = "רשות", col.regions = mypalette(6),at = seq(0,30,5),layer.name = "דירוג מכרזים",legend = FALSE,
                     popup = popupTable(data_mid, zcol = c("רשות","דירוג כולל", "שביעות רצון", "נגישות המידע","ארנונה", "מכרזים"),
                                        feature.id = FALSE,row.numbers = FALSE,className = "רשות"))

  
mid_map <- dirugmap1 + sviutmap1 +  negishutmap1 + arnonamap1 + michrazimap1



dirugmap2<-mapview(data_big, xcol="longitude", ycol = "latitude",zcol = "דירוג כולל",crs = 4269, grid = FALSE,
                   label = "רשות", col.regions = mypalette(3),at = seq(0,15,5),layer.name = "דירוג רשויות", legend = FALSE,
                   popup = popupTable(data_big, zcol = c("רשות","דירוג כולל", "שביעות רצון", "נגישות המידע","ארנונה", "מכרזים"),
                                      feature.id = FALSE,row.numbers = FALSE,className = "רשות"))


sviutmap2<-mapview(data_big, xcol="longitude", ycol = "latitude",zcol = "שביעות רצון",crs = 4269, grid = FALSE,
                   label = "רשות", col.regions = mypalette(3),at = seq(0,15,5),layer.name = "דירוג שביעות רצון", legend = FALSE,
                   popup = popupTable(data_big, zcol = c("רשות","דירוג כולל", "שביעות רצון", "נגישות המידע","ארנונה", "מכרזים"),
                                      feature.id = FALSE,row.numbers = FALSE,className = "רשות"))

negishutmap2<-mapview(data_big, xcol="longitude", ycol = "latitude",zcol = "נגישות המידע",crs = 4269, grid = FALSE,
                      label = "רשות", col.regions = mypalette(3),at = seq(0,15,5),layer.name = "דירוג נגישות המידע", legend = FALSE,
                      popup = popupTable(data_big, zcol = c("רשות","דירוג כולל", "שביעות רצון", "נגישות המידע","ארנונה", "מכרזים"),
                                         feature.id = FALSE,row.numbers = FALSE,className = "רשות"))

arnonamap2<-mapview(data_big, xcol="longitude", ycol = "latitude",zcol = "ארנונה",crs = 4269, grid = FALSE,
                    label = "רשות", col.regions = mypalette(3),at = seq(0,15,5),layer.name = "דירוג ארנונה", legend = FALSE,
                    popup = popupTable(data_big, zcol = c("רשות","דירוג כולל", "שביעות רצון", "נגישות המידע","ארנונה", "מכרזים"),
                                       feature.id = FALSE,row.numbers = FALSE,className = "רשות"))

michrazimap2<-mapview(data_big, xcol="longitude", ycol = "latitude",zcol = "מכרזים",crs = 4269, grid = FALSE,
                      label = "רשות", col.regions = mypalette(3),at = seq(0,15,5),layer.name = "דירוג מכרזים",legend = FALSE,
                      popup = popupTable(data_big, zcol = c("רשות","דירוג כולל", "שביעות רצון", "נגישות המידע","ארנונה", "מכרזים"),
                                         feature.id = FALSE,row.numbers = FALSE,className = "רשות"))


big_map <- dirugmap2 + sviutmap2 +  negishutmap2 + arnonamap2 + michrazimap2







#----'worksheet----
database <- as.data.frame(read_xlsx("C:/Users/SaarP/Box/צוות תעסוקה/סוכנות לעסקים קטנים/סקרים/מדד ידידות רשויות מקומיות/2022/סקר שביעות רצון/DashBoard data.xlsx"))
database$`ציון 2019` <- replace(database$`ציון 2019`,database$`ציון 2019`==0,NA)

data_big <- database %>% filter(size == "Big")
data_mid <- database %>% filter(size == "Mid")
data_small <- database %>% filter(size == "Small")

#----small popup ---

popup22small <- glue::glue(" <strong>{data_small$רשות}</strong><br />
                    <br />
                    <strong>{data_small$`דירוג כולל`} :דירוג כולל</strong><br />
                    {data_small$`שביעות רצון`} :שביעות רצון<br />
                    {data_small$`נגישות המידע`} :נגישות המידע<br />
                    {data_small$ארנונה} :ארנונה<br />
                    {data_small$מכרזים} :מכרזים<br />") %>% lapply(htmltools::HTML) 


popup22mid <- glue::glue("<strong>{data_mid$רשות}</strong><br />
                      ציון 2022: {round(data_mid$`ציון 2022`, 2)}") %>% lapply(htmltools::HTML)

popup22big <- glue::glue("<strong>{data_big$רשות}</strong><br />
                      ציון 2022: {round(data_big$`ציון 2022`, 2)}") %>% lapply(htmltools::HTML)

popup_21_22_small <- glue::glue("<strong>{data_small$רשות}</strong><br />
                      ציון 2022: {round(data_small$`ציון 2022`, 2)}<br />
                      ציון 2021: {round(data_small$`ציון 2021`, 2)}") %>% lapply(htmltools::HTML)


mypalette <- colorRampPalette(c("firebrick3", "gold1", "green4"))

mapview(data_big, xcol="longitude", ycol = "latitude", zcol = "ציון 2022",
        crs = 4269, grid = FALSE,layer.name = "רשויות עם מעל 150 אלף תושבים",label = "רשות",
        col.regions = mypalette(15), popup = popup22big,legend.title = "שביעות רצון 2022",
        map.type = c("CartoDB.Positron","OpenStreetMap","Esri.WorldImagery")) +
  mapview(data_mid, xcol="longitude",ycol = "latitude", zcol = "ציון 2022",
          crs = 4269, grid = FALSE,layer.name = "רשויות עם 50 עד 150 אלף תושבים",label = "רשות",
          col.regions = mypalette(34),legend = FALSE,popup = popup22mid,
          map.type = c("CartoDB.Positron","OpenStreetMap","Esri.WorldImagery")) +
  mapview(data_small, xcol="longitude", ycol = "latitude", zcol = "ציון 2022",
          crs = 4269, grid = FALSE,layer.name = "רשויות עם עד 50 אלף תושבים",label = "רשות",
          col.regions = mypalette(29), legend = FALSE,popup = popup22small,
          map.type = c("CartoDB.Positron","OpenStreetMap","Esri.WorldImagery"))


#-----?------
mymap <- leaflet()
mymap <- addProviderTiles(mymap, provider = "CartoDB.Positron")
mymap <- addProviderTiles(mymap, provider = "OpenStreetMap")
mymap <- mapview(data_small, xcol = "longitude", ycol = "latitude", zcol = "ציון 2022", crs = 4269, grid = FALSE)
mymap <- addLayersControl(map, baseGroups = c("CartoDB.Positron", "OpenStreetMap"))
#----sf----
map_by_sf <- st_as_sf(x = database,coords=c("longitude","latitude"), crs = 4326)
mapview(mapdata, map.type = "Jawg.Light") 

#map types : https://leaflet-extras.github.io/leaflet-providers/preview/

#----leaflet----

basemaps = c("CartoDB.Positron","OpenStreetMap")

leaflet() %>% addPolygons(data = database_spatial,  weight = 2, color = "white",  fillColor = "blue",  fillOpacity = 0.7)

#converting points to areas with radios = 800
mapdata <- st_as_sf(database, coords = c("longitude", "latitude"),  crs = 4269)
mapdata <- st_transform(mapdata, crs = 4326)
mapdata <- st_as_sf(mapdata,  geometry = "polygon")
st_geometry_type(mapdata)
mapdata <- st_buffer(mapdata, dist = 800)

leaflet() %>% 
  addProviderTiles(basemaps[1], group = basemaps[1]
  ) %>% 
  addProviderTiles(basemaps[2], group = basemaps[2]
  ) %>% 
  setView(lng = 34.99, lat = 32.2 , zoom = 7
  ) %>% 
  addLayersControl(baseGroups = basemaps
  ) %>%
  addPolygons(data = mapdata)

data_small$`דירוג כולל`
leaflet(data_small) %>% addTiles() %>% 
  addCircles(lng = ~longitude, lat= ~latitude, weight = 5, radius = 800, fillColor = mypalette(33)) %>%
  addLegend(position = "bottomright", pal = mypalette(), values = ~`דירוג כולל`,title = "דירוג",labFormat = labelFormat(prefix = "$"),opacity = 1)


