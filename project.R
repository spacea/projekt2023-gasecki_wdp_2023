library(climate)
library(ggplot2)
library(dplyr)
install.packages(geos)
library(rgdal)
library(rgeos)
library(tidyverse)
library(broom)

#pobieranie danych z imgw dla wojewodztw dla podanych danych czasowych
dane_woj_daily = function (woj, year, mon = 1:12, day = 1:31, rank = "synop") {
  meteo_woj = meteo_imgw(interval = "daily", rank = rank, year = year, coords = TRUE)
  if (!(is.numeric(c(year, mon, day)))) {
    stop("Podana wartość nie jest wartością numeryczną")
  } 
  meteo_mon = filter(meteo_woj, mm %in% mon)
  meteo_day = meteo_mon[meteo_mon$day %in% day, ]
  meteo_day
  if (rank == "synop") {
  if (woj == "zpom") {
    meteo_zp = filter(meteo_day, station %in% c("SZCZECIN", "ŚWINOUJŚCIE", "KOSZALIN", "KOŁOBRZEG-DŹWIRZYNO"))  
  } else if (woj == "pom") {
    meteo_pom = filter(meteo_day, station %in% c("CHOJNICE", "GDAŃSK-ŚWIBNO", "HEL", "LĘBORK", "ŁEBA", "RESKO-SMÓLSKO", "USTKA"))  
  } else if (woj == "warmaz") {
    meteo_warmaz = filter(meteo_day, station %in% c("ELBLĄG-MILEJEWO", "KĘTRZYN", "MIKOŁAJKI", "OLSZTYN"))  
  } else if (woj == "podl") {
    meteo_podl = filter(meteo_day, station %in% c("BIAŁYSTOK", "SUWAŁKI"))  
  } else if (woj == "lubus") {
    meteo_lubus = filter(meteo_day, station %in% c("GORZÓW WIELKOPOLSKI", "ZIELONA GÓRA", "SŁUBICE"))  
  } else if (woj == "wiel") {
    meteo_wiel = filter(meteo_day, station %in% c("KALISZ", "PIŁA", "LESZNO", "POZNAŃ-ŁAWICA"))  
  } else if (woj == "kpom") {
    meteo_kpom = filter(meteo_day, station %in% c("TORUŃ"))
  } else if (woj == "maz") {
    meteo_maz = filter(meteo_day, station %in% c("KOZIENICE", "MŁAWA", "PŁOCK", "SIEDLCE", "WARSZAWA-OKĘCIE"))
  } else if (woj == "lubel") {
    meteo_lubel = filter(meteo_day, station %in% c("LUBLIN-RADAWIEC", "TERESPOL", "WŁODAWA"))
  } else if (woj == "lodz") {
    meteo_lodz = filter(meteo_day, station %in% c("ŁÓDŹ-LUBLINEK", "SULEJÓW", "WIELUŃ", "ZAMOŚĆ"))
  } else if (woj == "dol") {
    meteo_dol = filter(meteo_day, station %in% c("JELENIA GÓRA", "LEGNICA", "ŚNIEŻKA", "WROCŁAW-STRACHOWICE"))
  } else if (woj == "opol") {
    meteo_opol = filter(meteo_day, station %in% c("OPOLE"))
  } else if (woj == "sla") {
    meteo_sla = filter(meteo_day, station %in% c("CZĘSTOCHOWA", "RACIBÓRZ", "KATOWICE-MUCHOWIEC", "BIELSKO-BIAŁA"))
  } else if (woj == "mal") {
    meteo_mal = filter(meteo_day, station %in% c("HALA GĄSIENICOWA", "KASPROWY WIERCH", "KRAKÓW-BALICE", "NOWY SĄCZ", "TARNÓW", "ZAKOPANE"))
  } else if (woj == "swiet") {
    meteo_swiet = filter(meteo_day, station %in% c("KIELCE-SUKÓW", "SANDOMIERZ"))
  } else if (woj == "podk") {
    meteo_podk = filter(meteo_day, station %in% c("KROSONO", "LESKO", "RZESZÓW-JASIONKA"))
  }  
  } else if (rank == "climate") {
    if (woj == "zpom") {
      
    }
  }
}
meteo = dane_woj_daily(zp, 2022, 5, 12)
meteo_sort = dane_woj_daily("mal", 2022, 9, 20)
dane_woj_daily("sla", 1:8, 2020:2021)

meteo_imgw(interval = "daily", rank = rank, year = year, coords = TRUE)
prec = meteo_imgw(interval = "monthly", rank = "precip", year = 2022)
