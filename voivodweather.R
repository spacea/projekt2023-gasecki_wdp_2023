library(climate)
library(ggplot2)
library(dplyr)
library(rgdal)
library(rgeos)
library(broom)
library(sf)
library(tmap)

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

# argumenty
# woj - województwo
# year - rok
# mon - miesiąc
# day - dzień
# interval - rodzaj interwału czasowego
# rank - typ stacji

# Funkcja pobierająca miesięczne dane meteo z danego województwa i przedziału 
# czasowego dla danego rodzaju stacji.
dane_woj_monthly = function (woj, year, mon = 1:12, rank = "synop") {
  meteo_woj = meteo_imgw(interval = "monthly", rank = rank, year = year, coords = TRUE)
  if (!(is.numeric(c(year, mon)))) {
    stop("Podana wartość nie jest wartością numeryczną")
  } 
  meteo_mon = filter(meteo_woj, mm %in% mon)
  meteo_mon = meteo_mon[!is.na(meteo_mon$X) == TRUE,]
  meteo_cord = st_as_sf(meteo_mon, coords = c("X", "Y"))# Tu tworzy sie obiekt klasy sf, czyli przestrzenny
  meteo_cords = st_set_crs(meteo_cord, 4326)# Wcześniej nadano mu koordynaty, tutaj układ odniesienia
  # W tej cześci sprawdza, które z pobranych danych należa do wybranego woj
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

# Podobnie jak wyżej, tylko dane dzienne.
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

# Funkcja, która uśrednia dane dla każdej stacji, 
# dane_woj to dane uzyskane za pomocą poprzedniej funkcji.
mean_woj = function(dane_woj, rank, interval) { 
  if (rank == "synop") {# Ta funkcja uśrednia najważniejsze dane, jest dużo if-ów bo w zależności od klasy stacji i przedzialu czasowego sa rożne dane w tych df
    if (interval == "daily") {
  mean_stacje = group_by(dane_woj, station) %>%
  summarise(Średnia_dzienna_temperatur = mean(t2m_mean_daily, na.rm = T),
            Minimalna_temperatura_dzienna = min(tmin_daily, na.rm = T),
            Średnia_minimalnych_dziennych_temperatur = mean(tmin_daily, na.rm = T),
            Maksymalna_temperatura_dzienna = max(tmax_daily, na.rm = T),
            Średnia_maksymalnych_dziennych_temperatur = mean(tmax_daily, na.rm = T),
            Średnia_dziennych_opadów = mean(rr_daily, na.rm = T),
            Średnia_godzin_słonecznych_w_ciagu_dnia = mean(insolation_hours, na.rm = T),
            Średnia_godzin_deszczowych_w_ciagu_dnia = mean(rain_hours, na.rm = T),
            Średnia_godzin_śnieżnych_w_ciagu_dnia = mean(snow_hours, na.rm = T),
            Średnie_dzienne_ciśnienie = mean(press_mean_daily, na.rm = T))
    } else if (interval == "monthly") {
      mean_stacje = group_by(dane_woj, station) %>%
      summarise(Średnia_temperatur_miesiecznych = mean(t2m_mean_mon, na.rm = T),
            Minimalna_temperatura_dzienna = min(tmin_abs, na.rm = T),
            Średnia_minimalnych_dziennych_temperatur = mean(tmin_mean, na.rm = T),
            Maksymalna_temperatura_dzienna = max(tmax_abs, na.rm = T),
            Średnia_maksymalnych_dziennych_temperatur = mean(tmax_mean, na.rm = T),
            Średnia_opadów_miesiecznych = mean(rr_monthly, na.rm = T),
            Średnia_dni_słonecznych_w_miesiacu = mean(insolation_monthly, na.rm = T),
            Średnia_dni_deszczowych_w_miesiacu = mean(rain_days, na.rm = T),
            Średnia_dni_śnieżnych_w_miesiacu = mean(snow_days, na.rm = T),
            Średnie_ciśnienie_w_miesiacu = mean(press_mean_mon, na.rm = T))
    }
  } else if (rank == "climate") {
    if (interval == "daily") {
      mean_stacje = group_by(dane_woj, station) %>%
           summarise(Średnia_dziennych_temperatur = mean(t2m_mean_daily, na.rm = T),
           Minimalna_dzienna_temperatura = min(tmin_daily, na.rm = T),
           Średnia_minimalnych_dziennych_temperatur = mean(tmin_daily, na.rm = T),
           Maksymalna_dziennych_temperatura = max(tmax_daily, na.rm = T),
           Średnia_maksymalnych_dziennych_temperatur = mean(tmax_daily, na.rm = T),
           Średnia_dziennych_opadów = mean(rr_daily, na.rm = T))
    } else if (interval == "monthly") {
      mean_stacje = group_by(dane_woj, station) %>%
        summarise(Średnia_miesiecznych_temperatur = mean(t2m_mean_mon, na.rm = T),
                  Minimalna_dzienna_temperatura = min(tmin_abs, na.rm = T),
                  Średnia_minimalnych_miesięcznych_temperatur = mean(tmin_mean, na.rm = T),
                  Maksymalna_dzienna_temperatura = max(tmax_abs, na.rm = T),
                  Średnia_maksymalnych_miesięcznych_temperatur = mean(tmax_mean, na.rm = T),
                  Średnia_miesiecznych_opadów = mean(rr_monthly, na.rm = T),
                  Średnia_dni_deszczowych_w_miesiacu = mean(rain_days, na.rm = T),
                  Średnia_dni_śnieżnych_w_miesiacu = mean(snow_days, na.rm = T))
    } else {
      stop("Podaj odpowiedni przedział czasowy:
           'daily' = dzienne
           'monthly' = miesięczne")
    }
  } else if (rank == "precip"){
    mean_stacje = group_by(dane_woj, station) %>%
      summarise(Średnia_miesiecznych_opadów = mean(rr_monthly, na.rm = T),
                Maksymalne_dzienne_opady = max(rr_max, na.rm = T),
                Średnia_dni_śnieżnych_w_miesiacu = mean(snow_days, na.rm = T))
  } else {
    stop("Podaj właściwy typ stacji:
         'synop' = synoptyczna
         'climate' = klimatyczna
         'precip' = opadowa")
  }
}

# Funkcja, która na interaktywnej mapie przedstawia stacje z danego województwa, 
# dane_mean_woj to dane uzyskane za pomoca funkcji mean_woj. 
# Po klilknięciu w daną stację pojawiają sie informacje o rożnych danych z 
# wcześniej przetworzomnych okresów.
map_woj = function(woj, year, mon = 1:12, day = 1:31, rank = "synop") {
  if (rank == "precip") {
    dane = dane_woj_monthly(woj = woj, year = year, mon = mon, rank = rank)
    dane_mean = mean_woj(dane, rank = "precip", interval = "monthly")
  } else {
  dane = dane_woj_daily(woj = woj, year = year, mon = mon, day = day, rank = rank)
  dane_mean = mean_woj(dane, rank = rank, interval = "daily")
  }
  tmap_mode("view")
  if (woj == "dol"){
    wojmap = dol
  } else if (woj == "kpom") {
    wojmap = kpom
  } else if (woj == "kpom") {
    wojmap = kpom
  } else if (woj == "lodz") {
    wojmap = lodz
  } else if (woj == "lubel") {
    wojmap = lubel
  } else if (woj == "lubus") {
    wojmap = lubus
  } else if (woj == "mal") {
    wojmap = mal
  } else if (woj == "maz") {
    wojmap = maz
  } else if (woj == "opol") {
    wojmap = opol
  } else if (woj == "podk") {
    wojmap = podk
  } else if (woj == "podl") {
    wojmap = podl
  } else if (woj == "pom") {
    wojmap = pom
  } else if (woj == "sla") {
    wojmap = sla
  } else if (woj == "swiet") {
    wojmap = swiet
  } else if (woj == "warmaz") {
    wojmap = warmaz
  } else if (woj == "wiel") {
    wojmap = wiel
  } else if (woj == "zpom") {
    wojmap = zpom
  } else if (woj == "pol") {
    wojmap = pol
  }
  
  tm_shape(wojmap) + 
    tm_borders()+  
    tm_shape(dane_mean) + 
    tm_symbols(col = "blue", border.col = "white")+
    tm_bubbles(
      size = 2,
      shape = 20,
      scale = 5/3
    )
}

# Klimatogram dla danego województwa, w danym przedziale czasowym, 
# dla wybranego rodzaju stacji.
klim_woj = function(woj, year, rank = "synop") {
  klim_dane = dane_woj_monthly(woj = woj, year = year, rank = rank)
  klim_sel = select(klim_dane, station:t2m_mean_mon, rr_monthly)
  
  mon_sum = klim_sel %>% 
    group_by(mm) %>% 
    summarise(tmax = mean(tmax_abs, na.rm = TRUE), 
              tmin = mean(tmin_abs, na.rm = TRUE),
              tavg = mean(t2m_mean_mon, na.rm = TRUE), 
              prec = sum(rr_monthly) / n_distinct(yy))            
  
  if (woj == "dol"){
    title = "dolnośląskie"
  } else if (woj == "kpom") {
    title = "kujawsko-pomorskie"
  } else if (woj == "lodz") {
    title = "łódzkie"
  } else if (woj == "lubel") {
    title = "lubelskie"
  } else if (woj == "lubus") {
    title = "lubuskie"
  } else if (woj == "mal") {
    title = "małopolskie"
  } else if (woj == "maz") {
    title = "mazowieckie"
  } else if (woj == "opol") {
    title = "opolskie"
  } else if (woj == "podk") {
    title = "podkarpackie"
  } else if (woj == "podl") {
    title = "podlaskie"
  } else if (woj == "pom") {
    title = "pomorskie"
  } else if (woj == "sla") {
    title = "śląskie"
  } else if (woj == "swiet") {
    title = "świętokrzyskie"
  } else if (woj == "warmaz") {
    title = "warmińsko-mazurskie"
  } else if (woj == "wiel") {
    title = "wielkopolskie"
  } else if (woj == "zpom") {
    title = "zachodniopomorskie"
  } else if (woj == "pol") {
    title = "Polska"
  }
  
  mon_sum = dplyr::select(as.data.frame(mon_sum), -geometry)
  mon_sum = round(mon_sum, 1)
  mon_sum = as.data.frame(t(mon_sum[, c(5,2,3,4)]))
  colnames(mon_sum) = month.abb
  climatol::diagwl(mon_sum, mlab = "en", 
                   est = title, alt = NA, 
                   per = year , p3line = F)
}



