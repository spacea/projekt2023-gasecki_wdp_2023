#wojewodztwa
getwd()
library(climate)
library(ggplot2)
library(dplyr)
install.packages(geos)
library(rgdal)
library(rgeos)
library(tidyverse)
library(broom)
install.packages("sf")
library(sf)
library(tmap)
pliki/wojewodztwa.shp


wojewodztwa <- st_read("Dane/wojewodztwa.shp")

dane = meteo_imgw(interval = "monthly", rank = "synop", year = 2021, coords = TRUE)
plot(dane$X, dane$Y)
tm_shape("Woj/lubel.gpkg")


getwd()
lubel = st_read("Woj/lubel.gpkg")
tm
class(lubel)
data()
plot(lubel$geom)
read_sf("Woj/lubel.gpkg")
tm_shape(lubel) + 
  tm_borders() +
  tm_shape(stacje)+
  tm_symbols(col = "black", border.col = "white")

dane_lubel = dane_woj_daily("lubel", 2020, 5, 1)
stacje = st_as_sf(dane_lubel, coords = c("X", "Y"))
