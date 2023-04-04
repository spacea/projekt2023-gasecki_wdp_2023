library(shiny)
library(tmap)

# Listy nazwanych argumentów, które są wyświetlane w kontrolkach input.
woj_list = list("dol", "kpom", "lodz", "lubel", "lubus", "mal", "maz", "opol", 
                "podk", "podl", "pom", "sla", "swiet", "warmaz", "wiel", "zpom", 
                "pol")
names(woj_list) = c("dolnośląskie", "kujawsko-pomorskie", "łódzkie", 
                    "lubelskie", "lubuskie", "małopolskie", "mazowieckie",
                    "opolskie", "podkarpackie", "podlaskie", "pomorskie",
                    "śląskie", "świętokrzyskie", "warmińsko-mazurskie",
                    "wielkopolskie", "zachodniopomorskie", "Polska")
rank_list = list("synop", "climate", "precip")
names(rank_list) = c("synoptyczne", "klimatyczne", "opadowe")

# Interfejs Shiny zawierający podstawowy layout strony i kontrolki input.
shinyUI(fluidPage(
  titlePanel("VoivodWeather"),
  
  tabsetPanel(
    tabPanel("Interaktywna mapa stacji",
             sidebarLayout(
               sidebarPanel(  
                 #wyświetlanie interaktywnych map stacji klimatycznych
                 h3("Interaktywna mapa stacji"),
                 selectInput(inputId = "swoj", 
                             label = "Województwo",
                             choices = woj_list,
                             selected = "pol"),
                 selectInput(inputId = "srank",
                             label = "Rodzaj stacji",
                             choices = rank_list,
                             selected = ""),
                 sliderInput(inputId = "syear",
                             label = "Lata",
                             min = 1960,
                             max = 2022,
                             value = c(2022, 2022),
                             sep = ""),
                 sliderInput(inputId = "smon",
                             label = "Miesiące",
                             min = 1,
                             max = 12,
                             value = c(1, 12),
                             sep = ""),
                 sliderInput(inputId = "sday",
                             label = "Dni",
                             min = 1,
                             max = 31,
                             value = c(1, 31),
                             sep = ""),
                 actionButton("sgen", "Generuj")
               ),
               mainPanel(
                 h3("Mapa stacji"),
                 tmapOutput("stacje")
               )
              )
    ),
    tabPanel("Klimatogram",
             sidebarLayout(
               sidebarPanel(
                 #wyświetlanie klimatogramów
                 h3("Klimatogram"),
                 selectInput(inputId = "woj", 
                             label = "Województwo",
                             choices = woj_list,
                             selected = "pol"),
                 sliderInput(inputId = "year", 
                             label = "Lata",
                             min = 1960,
                             max = 2022,
                             value = c(2022, 2022),
                             sep = ""),
                 selectInput(inputId = "rank",
                             label = "Rodzaj stacji",
                             choices = rank_list[c(1, 2)],
                             selected = "synop"),
                 actionButton("gen", "Generuj")
               ),
               mainPanel(  
                 h3("Klimatogram"),
                 plotOutput("klimatogram")
               )
             )
    )
  )
))

