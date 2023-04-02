library(climate)
library(ggplot2)
library(dplyr)
library(rgdal)
library(rgeos)
library(tidyverse)
library(broom)
library(sf)
library(tmap)
library(tidyverse)
library(climatol)

#obiekty uzywane przy wszelkich funkcjach
#konkretne wojewodztwa, znajduja sie w dane.zip na github
dol = st_read("Dane/dol.gpkg")
kpom = st_read("Dane/kpom.gpkg")
lodz = st_read("Dane/lodz.gpkg")
lubel = st_read("Dane/lubel.gpkg")
lubus = st_read("Dane/lubus.gpkg")
mal = st_read("Dane/mal.gpkg")
maz = st_read("Dane/maz.gpkg")
opol = st_read("Dane/opol.gpkg")
podk = st_read("Dane/podk.gpkg")
podl = st_read("Dane/podl.gpkg")
pom = st_read("Dane/pom.gpkg")
sla = st_read("Dane/sla.gpkg")
swiet = st_read("Dane/swiet.gpkg")
warmaz = st_read("Dane/warmaz.gpkg")
wiel = st_read("Dane/wiel.gpkg")
zpom = st_read("Dane/zpom.gpkg")
pol = st_read("Dane/polska.gpkg")
woj = data.frame() #zrobic ramke danych z wszystki jednostkami

#argumenty
#woj - wojewodztwo
#year - rok
#mon - miesiac
#day - dzien
#interval - rodzaj interwalu czasowego
#rank - typ stacji


#funkcja pobierająca miesieczne dane meteo z danego wojewodztwa i przedzialu czasowego dla danego rodzaju stacji
dane_woj_monthly = function (woj, year, mon = 1:12, rank = "synop") {
  meteo_woj = meteo_imgw(interval = "monthly", rank = rank, year = year, coords = TRUE)
  if (!(is.numeric(c(year, mon)))) {
    stop("Podana wartość nie jest wartością numeryczną")
  } 
  meteo_mon = filter(meteo_woj, mm %in% mon)
  meteo_mon = meteo_mon[!is.na(meteo_mon$X) == TRUE,]
  meteo_cord = st_as_sf(meteo_mon, coords = c("X", "Y"))#tu tworzy sie obiekt klasy sf, czyli przestrzenny
  meteo_cords = st_set_crs(meteo_cord, 4326)#wczesniej nadano mu koordy, tutaj uklad odniesienia
  #w tej czesci sprawdza ktore z pobranych danych naleza do wybranego woj
  if (woj == "dol") {
    przyn = st_within(meteo_cords, dol)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "kpom") {
    przyn = st_within(meteo_cords, kpom)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "lodz") {
    przyn = st_within(meteo_cords, lodz)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "lubes") {
    przyn = st_within(meteo_cords, lubel)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "lubus") {
    przyn = st_within(meteo_cords, lubus)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "mal") {
    przyn = st_within(meteo_cords, mal)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "maz") {
    przyn = st_within(meteo_cords, maz)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "opol") {
    przyn = st_within(meteo_cords, opol)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "podk") {
    przyn = st_within(meteo_cords, podk)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "podl") {
    przyn = st_within(meteo_cords, podl)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "pom") {
    przyn = st_within(meteo_cords, pom)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "sla") {
    przyn = st_within(meteo_cords, sla)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "swiet") {
    przyn = st_within(meteo_cords, swiet)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "warmaz") {
    przyn = st_within(meteo_cords, warmaz)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "wiel") {
    przyn = st_within(meteo_cords, wiel)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "zpom") {
    przyn = st_within(meteo_cords, zpom)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "pol") {
    meteo_cords
  }
} 
#trzeba jeszcze zrobic troche programowania defensywnego
#dane_dol = dane_woj_monthly("pol", 2022, rank = "synop")

#podobnie jak wyzej z tym ze dane dzienne
dane_woj_daily = function (woj, year, mon = 1:12, day = 1:31, rank = "synop") {
  meteo_woj = meteo_imgw(interval = "daily", rank = rank, year = year, coords = TRUE, allow_failure = FALSE)
  if (!(is.numeric(c(year, mon, day)))) {
    stop("Podana wartość nie jest wartością numeryczną")
  } 
  if (rank == "precip") {
    stop("Brak danych dziennych dla stacji opadowych")
  } #sprawdzic czemu wciaz to liczy mimo stopa
  meteo_mon = filter(meteo_woj, mm %in% mon)
  meteo_day = meteo_mon[meteo_mon$day %in% day, ]
  meteo_day = meteo_day[!is.na(meteo_day$X) == TRUE,]
  meteo_cord = st_as_sf(meteo_day, coords = c("X", "Y"))
  meteo_cords = st_set_crs(meteo_cord, 4326)
  
  if (woj == "dol") {
    przyn = st_within(meteo_cords, dol)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "kpom") {
    przyn = st_within(meteo_cords, kpom)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "lodz") {
    przyn = st_within(meteo_cords, lodz)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "lubes") {
    przyn = st_within(meteo_cords, lubel)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "lubus") {
    przyn = st_within(meteo_cords, lubus)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "mal") {
    przyn = st_within(meteo_cords, mal)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "maz") {
    przyn = st_within(meteo_cords, maz)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "opol") {
    przyn = st_within(meteo_cords, opol)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "podk") {
    przyn = st_within(meteo_cords, podk)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "podl") {
    przyn = st_within(meteo_cords, podl)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "pom") {
    przyn = st_within(meteo_cords, pom)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "sla") {
    przyn = st_within(meteo_cords, sla)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "swiet") {
    przyn = st_within(meteo_cords, swiet)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "warmaz") {
    przyn = st_within(meteo_cords, warmaz)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "wiel") {
    przyn = st_within(meteo_cords, wiel)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "zpom") {
    przyn = st_within(meteo_cords, zpom)
    meteo_cords$woj = przyn
    dane = filter(meteo_cords, woj %in% 1)
    dane
  } else if (woj == "pol") {
    meteo_cords
  }
} 
#dane_pom2 = dane_woj_daily("pom", 2010:2011, 5, day = 1:7, rank = "synop")

#funkcja, która uśrednia dane dla kazdej stacji, dane_woj to dane uzyskane za pomoca poprzedniej funkcji
mean_woj = function(dane_woj, rank, interval) { 
  if (rank == "synop") {#ta funkcja usrednia najwazniejsze dane, jest duzo if-ow bo w zaleznosci od klasy stacji i przedzialu czasowego sa rozne dane w tych df
    if (interval == "daily") {
  mean_stacje = group_by(dane_woj, station) %>%
  summarise(mean_temp = mean(t2m_mean_daily, na.rm = T),
            min_temp = min(tmin_daily, na.rm = T),
            mean_temp_min = mean(tmin_daily, na.rm = T),
            max_temp = max(tmax_daily, na.rm = T),
            mean_temp_max = mean(tmax_daily, na.rm = T),
            mean_rr = mean(rr_daily, na.rm = T),
            mean_ins_h = mean(insolation_hours, na.rm = T),
            mean_rain_h = mean(rain_hours, na.rm = T),
            mean_snow_h = mean(snow_hours, na.rm = T),
            mean_press = mean(press_mean_daily, na.rm = T))
    } else if (interval == "monthly") {
      mean_stacje = group_by(dane_woj, station) %>%
      summarise(mean_temp = mean(t2m_mean_mon, na.rm = T),
            min_temp = min(tmin_abs, na.rm = T),
            mean_temp_min = mean(tmin_mean, na.rm = T),
            max_temp = max(tmax_abs, na.rm = T),
            mean_temp_max = mean(tmax_mean, na.rm = T),
            mean_rr = mean(rr_monthly, na.rm = T),
            mean_ins_d = mean(insolation_monthly, na.rm = T),
            mean_rain_d = mean(rain_days, na.rm = T),
            mean_snow_d = mean(snow_days, na.rm = T),
            mean_press = mean(press_mean_mon, na.rm = T))
    }else {
      stop("Podaj odpowiedni przedział czasowy:
           'daily' = dzienne
           'monthly' = miesięczne")
    }
  } else if (rank == "climate") {
    if (interval == "daily") {
      mean_stacje = group_by(dane_woj, station) %>%
        summarise(mean_temp = mean(t2m_mean_daily, na.rm = T),
                  min_temp = min(tmin_daily, na.rm = T),
                  mean_temp_min = mean(tmin_daily, na.rm = T),
                  max_temp = max(tmax_daily, na.rm = T),
                  mean_temp_max = mean(tmax_daily, na.rm = T),
                  mean_rr = mean(rr_daily, na.rm = T))
    } else if (interval == "monthly") {
      mean_stacje = group_by(dane_woj, station) %>%
        summarise(mean_temp = mean(t2m_mean_mon, na.rm = T),
                  min_temp = min(tmin_abs, na.rm = T),
                  mean_temp_min = mean(tmin_mean, na.rm = T),
                  max_temp = max(tmax_abs, na.rm = T),
                  mean_temp_max = mean(tmax_mean, na.rm = T),
                  mean_rr = mean(rr_monthly, na.rm = T),
                  mean_rain_d = mean(rain_days, na.rm = T),
                  mean_snow_d = mean(snow_days, na.rm = T))
    } else {
      stop("Podaj odpowiedni przedział czasowy:
           'daily' = dzienne
           'monthly' = miesięczne")
    }
  } else if (rank == "precip"){
    mean_stacje = group_by(dane_woj, station) %>%
      summarise(mean_rr = mean(rr_monthly, na.rm = T),
                mas_rr = max(rr_max, na.rm = T),
                mean_snow_d = mean(snow_days, na.rm = T))
  } else {
    stop("Podaj właściwy typ stacji:
         'synop' = synoptyczna
         'climate' = klimatyczna
         'precip' = opadowa")
  }
}

#dane_pol = mean_woj(dane_pol, "synop", interval = "monthly")

#funkcja, która na interaktywnej mapie przedstawia stacje z danego wojewodztwa, dane_mean_woj to
#dane uzyskane za pomoca funkcji mean_woj. Po klilknieciu w dana stacje pojawiaja sie informacje
#o roznych rzeczach z wczesniej przetworzomnego okresu
#bede probowal to jescze polaczyc, zeby danymi wejsciowymi byl czas i wojewodztwo
map_woj = function(woj, dane_mean_woj) {
  tmap_mode("view")
  tm_shape(woj) + 
    tm_borders()+  
    tm_shape(dane_mean_woj) + 
    tm_symbols(col = "blue", border.col = "white")+
    tm_bubbles(
      size = 2,
      shape = 20,
      scale = 5/3
    )
}
#map_woj("zpom", dane_pol)

#klimatogram dla danego wojewodztwa, w danym przedziale czasowym, dla wybranego rodzaju stacji
klim_woj = function(woj, year, rank = "synop") {
  klim_dane = dane_woj_monthly(woj = woj, year = year, rank = rank)
  klim_sel = select(klim_dane, station:t2m_mean_mon, rr_monthly)
  
  mon_sum = klim_sel %>% 
    group_by(mm) %>% 
    summarise(tmax = mean(tmax_abs, na.rm = TRUE), 
              tmin = mean(tmin_abs, na.rm = TRUE),
              tavg = mean(t2m_mean_mon, na.rm = TRUE), 
              prec = sum(rr_monthly) / n_distinct(yy))            
  
  mon_sum = dplyr::select(as.data.frame(mon_sum), -geometry)
  mon_sum = round(mon_sum, 1)
  mon_sum = as.data.frame(t(mon_sum[, c(5,2,3,4)]))
  colnames(mon_sum) = month.abb
  climatol::diagwl(mon_sum, mlab = "en", 
                   est = woj, alt = NA, 
                   per = "2022", p3line = FALSE)
}  #Maciej: rok na klimatogramie się nie zmienia
#klim_woj("zpom", 2000:2020)
         