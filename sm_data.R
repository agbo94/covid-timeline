library(dplyr)
library(RColorBrewer)
library(zoo)
library(lubridate)
require(tidyr)
library(shinythemes)
library(sf)
library(leaflet)
library(rgdal)

set.seed(1234)

dat<-read.csv("All_Info_Merged.csv") %>% mutate(Date =mdy(Date))
dat$Types_EO <- as.factor(dat$Types_EO)

# Pull latest map data
content <- RCurl::getBinaryURL("https://covid.geography.uconn.edu/wp-content/uploads/sites/3199/2021/02/COVID_19_Master.gdb_.zip")
writeBin(content, con = "COVID_19_Master.gdb.zip")
unzip("COVID_19_Master.gdb.zip")
dat_cases <- sf::st_read(dsn = "COVID_19_Master.gdb", layer = 'COVID_19_cases')
dat_cases <- st_transform(dat_cases, "+init=epsg:4326")
dat_deaths <- sf::st_read(dsn = "COVID_19_Master.gdb", layer = 'COVID_19_deaths')
dat_deaths <- st_transform(dat_deaths, "+init=epsg:4326")
unlink("COVID_19_Master.gdb", recursive=TRUE, force=TRUE)
unlink("COVID_19_Master.gdb.zip", recursive=TRUE, force=TRUE)

# col_name = 'March_30_2020'
# pal = colorQuantile("OrRd", jitter(dat_t[[col_name]]), n=10)
# #pal = colorBin("OrRd", dat_t[[col_name]], bins=9)
# m <- leaflet(dat_t) %>%
#   addEsriBasemapLayer(esriBasemapLayers$Gray) %>%
#   setView(-72.699997, 41.599998, 8) %>%
#   addPolygons(stroke=TRUE, weight=1, color='grey', fillOpacity = 0.8, smoothFactor = 0.2, fillColor = pal(dat_t[[col_name]])) %>%
#   addLegend(pal = pal, values = dat_t[[col_name]], opacity=1)
# m


# dat <- read.csv("CTfromJune1.csv", header=TRUE) %>% 
#   dplyr::select(Date, Daily_Cases, Daily_Deaths) %>% 
#   mutate(Date = mdy(Date), 
#          Daily_Cases_ma = rollmean(Daily_Cases, k=3, na.pad=TRUE),
#          Daily_Cases_cum = cumsum(Daily_Cases),
#          Daily_Deaths_ma = rollmean(Daily_Deaths, k=3, na.pad=TRUE),
#          Daily_Deaths_cum = cumsum(Daily_Deaths))

# data<-read.csv("DataMarch-Feb10.csv")%>%
#   dplyr::select(Date, Daily_Cases, Daily_Deaths, Total_Case, Total_Death) %>%
#   mutate(Date = mdy(Date),
#          Daily_Cases_ma = rollmean(Daily_Cases, k=3, na.pad=TRUE),
#          Daily_Cases_cum = cumsum(Daily_Cases),
#          Daily_Deaths_ma = rollmean(Daily_Deaths, k=3, na.pad=TRUE),
#          Daily_Deaths_cum = cumsum(replace_na(Daily_Deaths, 0)))
# 
# 
# events <- read.csv("ExeOrder_Jan_5.csv")%>%
#   dplyr::select(Date, Types_EO, Details_1) %>%
#   mutate(Date=mdy(Date))
# 
# dat <- merge(data, events, by="Date", all.x = TRUE)


##map
# set.seed(123456)
# 
# df <- data.frame(latitude = sample(seq(-38.5, -37.5, by = 0.01), 100),
#                  longitude = sample(seq(144.0, 145.0, by = 0.01), 100),
#                  value = seq(1,100))

# library(rgdal)
# ct<-readOGR("CT_towns/CT_Towns.shp")
# mapdat<-read.csv("~/Desktop/ash-covid-timeline-master/CT_Vis/New_Viz/tabula-CTDPHCOVID19summary12312020.csv")
# 
# case_date<-merge(ct, mapdat, by.x="TOWN", by.y="Town")
# 
# case_date_sf<-st_as_sf(case_date)
# 
# case_date_st <- sf::st_transform(case_date_sf, 4326)
# 
# # Create a color palette with handmade bins.
# library(RColorBrewer) 
# mybins <- c(0,1000,2000,5000,6000,10000,Inf)
# mypalette <- colorBin(palette="YlOrBr", domain=case_date@data$Total_cases, na.color="transparent")
# 
# # Prepare the text for tooltips:
# mytext <- paste(
#   "Town: ", case_date@data$TOWN, "<br/>", 
#   "Cases: ", case_date@data$Total_cases, 
#   sep="") %>%
#   lapply(htmltools::HTML)

# Final Map
# leaflet(case_date_st) %>% 
#   addTiles()  %>% 
#   setView( lat=41.599998, lng=-72.699997 , zoom=8) %>%
#   addPolygons( 
#     color = ~mypalette(case_date@data$Total_cases), 
#     stroke=TRUE, 
#     fillOpacity = 0.9, 
#     weight=0.3,
#     label = mytext,
#     labelOptions = labelOptions( 
#       style = list("font-weight" = "normal", padding = "3px 8px"), 
#       textsize = "8px", 
#       direction = "auto"
#     )
#   ) %>%
#   addLegend( pal=mypalette, values=~Total_cases, opacity=0.9, title = "Total Cases", position = "bottomleft" )

 


