# VoivodWeather

  **Autorzy**: Kacper Gąsecki, Maciej Rosada, Mateusz Ren

Aplikacja ta pozwala użytkownikowi na generowanie interaktywnych map Polski i województw z zaznaczonymi nań stacjami meteorologicznymi. Kliknięcie w daną stację ujawnia pobrane z imgw dane meteorolgiczne zarejestrowane przez daną stację w podanym przedziale czasowym. Umożliwia też generowanie klimatogramów dla Polski i województw zgodnie z podanymi kryteriami czasowymi.

 > Poszczególne funckje i elementy kodu opisane są w komentarzach.

### Instrukcja uruchomienia bez pobierania repozytorium

### Instrukcja z pobieraniem repozytorium
  1. Zaczynamy od pobrania i rozpakowania całej zawartości brancha main.
  2. Następnie najlepiej otworzyć voivodweather.R w RStudio by upewnić się czy mamy zainstalowane wszystkie potrzebne pakiety. Jeżeli nie to RStudio powinno zasgurować      zainstalowanie ich i tak też należy postąpić. 
  3. Po zainstalowaniu pakietów przychodzi pora na otworzenie ui.R, w którym możemy być dodatkowo poinformowani o konieczności zainstalowania pakietu Shiny. Jeżeli nie      jest zainstalowany to jak najbardziej należy go zainstalować.
  4. Gdy już mamy pakiet Shiny, to możemy uruchomić aplikację poprzez otworzenie ui.R lub server.R i kliknięcie w "RunApp" w prawym górnym rogu okienka skryptu.
  5. Pokaże się okienko aplikacji, przy którym należy poczekać z wykonywaniem jakichkolwiek akcji, aż do pojawienia się mapy stacji wygenerowanej na 
     podstawie domyślnych danych.
  6. Gdy aplikacja jest już w pełni załadowana możemy zmieniać wartości kontrolek w lewym panelu, na podstawie których będzimy generować mapy. W celu 
     zatwierdzenia wybranych kryteriów należy kliknąć przycisk "Generuj" i chwilę odczekać. 
  
   > **UWAGA!** Może się zdarzyć, że będzie brakowało danych dla konkretnego zestawu kryteriów, co spowoduje błąd. Błąd ten jednakże nie zatrzymuje aplikacji, można więc zmienić kryteria i spróbowac ponownie.
 
  7. Te same czynności możemy wykonywać dla gneratora klimatogramów dostępnego w osobnej zakładce.
  
### Zawartość folderu <br>
  * **Dane** - folder zawierający pliki geopackage, które są używane przez progam do wydzielania odpowiednich dla stacji województw. <br>
  * **projekt2023-gasecki_wdp_2023.Rproj** - plik projektu RStudio. <br>
  * **server.R** - skrypt R przetwarzający zasady zachowywania się interfejsu Shiny z pliku ui.R. <br>
  * **ui.R** - skrypt R zawierający wygląd aplikacji stworzoną przy użyciu pakietu Shiny. <br>
  * **voivodweather.R** - skrypt główny zawirający wszystkie najważniejsze funkcje pobierające dane, przetwarzające je i tworzące z nich mapy i wykresy. 
  
### Wykorzystane pakiety:
  * climate
  * ggplot2
  * dplyr
  * rgdal
  * rgeos
  * broom
  * sf
  * tmap
  * tidyverse
  * shiny
  
[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-8d59dc4de5201274e310e4c54b9627a8934c3b88527886e3b421487c677d23eb.svg)](https://classroom.github.com/a/tauthlex)
