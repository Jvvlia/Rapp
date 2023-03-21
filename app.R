
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(tidyr)
library(gapminder)
library(stringr)
library(bslib)
library(dashboardthemes)
library(ggplot2)

dane1<-read.csv(file = "C:/Users/julia/OneDrive/Pulpit/Programowanie w R/Aplikacja/dlugosc_zycia.csv")
dane2<-read.csv(file = "C:/Users/julia/OneDrive/Pulpit/Programowanie w R/Aplikacja/men34.44.csv")
dane3<-read.csv(file = "C:/Users/julia/OneDrive/Pulpit/Programowanie w R/Aplikacja/SDI.csv")
dane4<-read.csv(file = "C:/Users/julia/OneDrive/Pulpit/Programowanie w R/Aplikacja/HDI.csv")
dane5<-read.csv(file = "C:/Users/julia/OneDrive/Pulpit/Programowanie w R/Aplikacja/PKB_per_capita.csv")

czyszczenie1<-separate(gather(dane1,year, dl_zycia, -country), 
                       col=year,into = c("X", "year"),sep="X")[,-2]
czyszczenie2<-separate(gather(dane2,year, mezczyzni, -country), 
                       col=year,into = c("X", "year"),sep="X")[,-2]
czyszczenie3<-separate(gather(dane3,year, SDI, -country), 
                       col=year,into = c("X", "year"),sep="X")[,-2]
czyszczenie4<-separate(gather(dane4,year, HDI, -country), 
                       col=year,into = c("X", "year"),sep="X")[,-2]
czyszczenie5<-separate(gather(dane5,year, PKB_per_capita, -country), 
                       col=year,into = c("X", "year"),sep="X")[,-2]


dane_1<- inner_join(czyszczenie1,czyszczenie2, by=c("country", "year"))
dane_2<- inner_join(dane_1,czyszczenie3, by=c("country", "year"))
dane_3<- inner_join(dane_2,czyszczenie4, by=c("country", "year"))
tabela<- inner_join(dane_3,czyszczenie5, by=c("country", "year"))
warM1 <- str_detect(tabela$PKB_per_capita, "M")
wark1 <- str_detect(tabela$PKB_per_capita, "k")

tabela$PKB_per_capita<- ifelse(warM1, as.numeric(str_replace(tabela$PKB_per_capita, "M", ""))*1000000, ifelse(wark1, as.numeric(str_replace(tabela$PKB_per_capita, "k", ""))*1000, tabela$PKB_per_capita))

dane<-tabela %>%
  group_by(country) %>% 
  filter(year %in% c("2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009", "2010"), 
         country %in% c("Afghanistan", "Albania", "United Arab Emirates", "Argentina", "Armenia", "Australia", 
                        "Austria", "Burundi", "Belgium", "Benin", "Bangladesh", "Bulgaria", "Bahrain", "Bolivia",
                        "Brazil", "Barbados", "Botswana", "Central African Republic", "Canada", "Switzerland", "Chile", 
                        "China", "Cote d'Ivoire", "Cameroon", "Congo, Dem. Rep.", "Congo, Rep.", "Colombia", "Costa Rica", 
                        "Cuba", "Cyprus", "Germany", "Denmark", "Dominican Republic", "Algeria", "Ecuador", "Egypt", "Spain", 
                        "Estonia", "Finland", "France", "Gabon", "United Kingdom", "Ghana", "Guinea", "Gambia", "Greece", 
                        "Guatemala", "Honduras", "Croatia", "Haiti", "Hungary", "Indonesia", "India", "Ireland", "Iran", 
                        "Iraq", "Iceland", "Israel", "Italy", "Jamaica", "Jordan", "Japan", "Kazakhstan", "Kenya", 
                        "Kyrgyz Republic", "Cambodia", "South Korea", "Kuwait", "Lao", "Libya", "Sri Lanka", "Lesotho", 
                        "Lithuania", "Latvia", "Morocco", "Moldova", "Mexico", "Mali", "Myanmar", "Mongolia", "Mozambique", 
                        "Mauritania", "Malawi", "Malaysia", "Namibia", "Niger", "Nicaragua", "Netherlands", "Norway", "Nepal", 
                        "New Zealand", "Pakistan", "Panama", "Peru", "Philippines", "Papua New Guinea", "Poland", "Portugal", 
                        "Paraguay", "Qatar", "Romania", "Russia", "Rwanda", "Saudi Arabia", "Senegal", "Singapore", 
                        "Sierra Leone", "El Salvador", "Turkey", "Tanzania", "Uganda", "Ukraine", "Uruguay", "United States", 
                        "Venezuela", "Vietnam", "Yemen", "South Africa", "Zambia", "Zimbabwe")) %>%
  mutate(PKB=as.numeric(PKB_per_capita), kontynent=case_when(country %in% c("Albania", "Austria", "Belgium", "Bulgaria", 
                                                                            "Switzerland", "Germany", "Denmark", "Spain", "Estonia", "Finland", "France", "United Kingdom","Cyprus",  "Greece","Ireland", 
                                                                            "Croatia", "Hungary", "Iceland", "Italy", "Lithuania", "Latvia", "Moldova", "Netherlands", "Norway","Poland", "Portugal", 
                                                                            "Romania", "Turkey", "Ukraine")~'Europa', 
                                                             country %in% c("Argentina", "Australia", "Bolivia", "Brazil", "Barbados", "Canada", 
                                                                            "Chile", "Colombia", "Costa Rica", "Cuba", "Dominican Republic", "Ecuador", "Guatemala", "Honduras", "Haiti", "Jamaica", 
                                                                            "Mexico", "Nicaragua", "Panama", "Peru", "Paraguay", "El Salvador", "Uruguay", "United States", "Venezuela")~'Ameryki', 
                                                             country %in% c("Afghanistan", "United Arab Emirates", "Armenia", "Australia", "Bangladesh", "Bahrain",  "China", "Cyprus", 
                                                                            "Indonesia", "India", "Iran", "Iraq", "Israel", "Jordan", "Japan", "Kazakhstan", "Kyrgyz Republic", "Cambodia", "South Korea", 
                                                                            "Kuwait", "Lao", "Sri Lanka", "Myanmar", "Mongolia", "Malaysia", "Nepal", "Pakistan", "Philippines", "Qatar",  "Russia", 
                                                                            "Saudi Arabia", "Singapore", "Vietnam", "Yemen")~ 'Azja', 
                                                             country %in% c("Australia", "Cuba", "New Zealand")~'Oceania' , TRUE~'Afryka')) %>%
  arrange(country)

ui <- dashboardPage(
  skin="purple",
  dashboardHeader(title = "Analiza wybranych wskaźników", 
                  titleWidth = 500),
  dashboardSidebar(
    width = 275,
    sidebarMenu(
    sidebarSearchForm("Wyszukiwanie", "WyszukiwaniePrzycisk",label = "Szukaj..."),
    menuItem("Informacje o danych", tabName = "infodane"),
    menuItem("Średnia długość życia", tabName = "dl_zycia"),
    menuItem("Średnia liczba lat edukacji mężczyzn", tabName = "lata_edu"),
    menuItem("SDI", tabName = "SDI"),
    menuItem("HDI", tabName = "HDI"),
    menuItem("PKB", tabName = "PKB")),
    menuItem("Kontynenty", tabName = "kontynenty"),
    selectInput("kontynent", 
                h5("Wybierz kontynent: ", style="color:#FFFFFF;"), 
                choices=c("Europa", "Ameryki", "Azja", "Oceania", "Afryka"),
                multiple = FALSE),
    uiOutput("secondSelection")
    ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "infodane",
              h1("Analiza przeprowadzona dla wybranych krajów."),
              br(),
              h4("Analizowane państwo lub kontynent opisane będzie za pomocą następujących wskaźników w latach 2000-2010."),
              br(),
              h3("Wskaźniki:"),
              h4("1: Średnia długość życia w krajach."),
              h4("2: Średnia liczba lat uczęszczania do szkoły przez mężczyzn w wieku 35-44 lat."),
              h4("3: Indeks zrównoważonego rozwoju."),
              h4("4: Miernik opisujący stopień rozwoju społeczno-ekonomicznego."),
              h4("5: Produkt krajowy brutto na osobę"),
      ),

      tabItem(tabName = "dl_zycia",
              h3("Średnia długość życia"),
              fluidRow(
                box(plotOutput("plot1", height = 275), background="light-blue",width=6),
                box(
                  tableOutput("op1"), width=6
                ),
                box(plotOutput("plot2"), width=12)
              )
              
      ),
      
      tabItem(tabName = "lata_edu",
              h3("Średnia liczba lat uczęszczania do szkoły przez mężczyzn w wieku 35-44 lat"),
              fluidRow(box(plotOutput("plot3", height = 275), background="light-blue",width=6),
                       box(
                         tableOutput("op2"), width=6
                       ),
                       box(plotOutput("plot4"), width=12)
      )
      ),
      
      tabItem(tabName = "SDI",
              h3("Indeks zrównoważonego rozwoju"),
              fluidRow(box(plotOutput("plot5", height = 275), background="light-blue",width=6),
                       box(
                         tableOutput("op3"), width=6
                       ),
                       box(plotOutput("plot6"), width=12)
              )
      ),
      
      tabItem(tabName = "HDI",
              h3("Miernik opisujący stopień rozwoju społeczno-ekonomicznego"),
              fluidRow(box(plotOutput("plot7", height = 275), background="light-blue",width=6),
                       box(
                         tableOutput("op4"), width=6
                       ),
                       box(plotOutput("plot8"), width=12)
              )
      ),
      
      tabItem(tabName = "PKB",
              h3("Produkt krajowy brutto na osobę"),
              fluidRow(box(plotOutput("plot9", height = 275), background="light-blue",width=6),
                       box(
                         tableOutput("op5"), width=6
                       ),
                       box(plotOutput("plot10"), width=12)
              )
      ),
      tabItem(tabName = "kontynenty",
              h3("Wartości podstawowych statystyk dla danego kontynentu:"),
              h4("Średnia długość życia"),
              fluidRow(
                box(tableOutput("op1K"), width=12),
                h4("Średnia liczba lat edukacji"),
                box(tableOutput("op2K"), width = 12),
                h4("Średnia wartość SDI"),
                box(tableOutput("op3K"), width=12),
                h4("Średnia wartość HDI"),
                box(tableOutput("op4K"), width=12),
                h4("Średnia wartość PKB"),
                box(tableOutput("op5K"), width=12),
              )
      )
    )
  )
)


server <- function(input, output) {
  dat<-list(Europa=c("Albania", "Austria", "Belgium", "Bulgaria", 
                     "Switzerland", "Germany", "Denmark", "Spain", "Estonia", "Finland", "France", "United Kingdom","Cyprus",  "Greece","Ireland", 
                     "Croatia", "Hungary", "Iceland", "Italy", "Lithuania", "Latvia", "Moldova", "Netherlands", "Norway","Poland", "Portugal", 
                     "Romania", "Turkey", "Ukraine"),
            Ameryki=c("Argentina", "Australia", "Bolivia", "Brazil", "Barbados", "Canada", 
                      "Chile", "Colombia", "Costa Rica", "Cuba", "Dominican Republic", "Ecuador", "Guatemala", "Honduras", "Haiti", "Jamaica", 
                      "Mexico", "Nicaragua", "Panama", "Peru", "Paraguay", "El Salvador", "Uruguay", "United States", "Venezuela"),
            Azja=c("Afghanistan", "United Arab Emirates", "Armenia", "Australia", "Bangladesh", "Bahrain",  "China", "Cyprus", 
                   "Indonesia", "India", "Iran", "Iraq", "Israel", "Jordan", "Japan", "Kazakhstan", "Kyrgyz Republic", "Cambodia", "South Korea", 
                   "Kuwait", "Lao", "Sri Lanka", "Myanmar", "Mongolia", "Malaysia", "Nepal", "Pakistan", "Philippines", "Qatar",  "Russia", 
                   "Saudi Arabia", "Singapore", "Vietnam", "Yemen"),
            Oceania=c("Australia", "Cuba", "New Zealand"),
            Afryka=c("Burundi", "Benin", "Botswana", "Central African Republic", "Cote d'Ivoire", "Cameroon", "Congo, Dem. Rep.", "Congo, Rep.", 
                     "Algeria", "Egypt", "Gabon", "Ghana", "Guinea", "Gambia", "Kenya", "Libya", "Lesotho", "Morocco", "Mali", "Mozambique", 
                     "Mauritania", "Malawi", "Namibia", "Niger", "Papua New Guinea", "Rwanda", "Senegal", "Sierra Leone", "Tanzania", "Uganda", 
                     "Zambia", "Zimbabwe","South Africa"))
  output$secondSelection <- renderUI({
    selectInput("Kraj", h5("Wybierz kraj: ", style="color:#FFFFFF;"), choices = as.character(dat[[input$kontynent]]))
  })
  output$plot1 <- renderPlot({
    data <- dane %>%
      filter(country==as.character(input$Wyszukiwanie))
    boxplot(data$dl_zycia,horizontal = TRUE,gray="col")
    stripchart(data$dl_zycia,         
               method = "jitter", 
               pch = 19,          
               col = "purple",           
               add = TRUE) 
  })
  output$plot2 <- renderPlot({
    data <- dane %>%
      filter(country==as.character(input$Wyszukiwanie))
    plot(data$year, data$dl_zycia, 
         xlab = "lata",
         ylab = "Średnia długość życia",
         pch=19)
  })
  output$op1 <-renderTable({ 
    data <- dane %>%
      filter(country==as.character(input$Wyszukiwanie))
    data.frame(cbind("Statystyka"=c("Kraj","Minimum","Średnia","Odchylenie standardowe",
                       "Mediana","Medianowe odchylenie bezwzgledne","Maksimum"),
                     "Wartość"=c(
                       input$Wyszukiwanie,
                       round(min(data$dl_zycia), 2),
                       round(mean(data$dl_zycia), 2),
                       round(sd(data$dl_zycia), 2),
                       round(median(data$dl_zycia), 2),
                       round(mad(data$dl_zycia), 2),
                       round(max(data$dl_zycia), 2)
                     )
    )
    )
  })
  output$plot3 <- renderPlot({
    data <- dane %>%
      filter(country==as.character(input$Wyszukiwanie))
    boxplot(data$mezczyzni,horizontal = TRUE,gray="col")
    stripchart(data$mezczyzni,         
               method = "jitter", 
               pch = 19,          
               col = "purple",           
               add = TRUE) 
  })
  output$plot4 <- renderPlot({
    data <- dane %>%
      filter(country==as.character(input$Wyszukiwanie))
    plot(data$year, data$mezczyni, 
         xlab = "lata",
         ylab = "Średnia liczba lat uczęszczania do szkoły przez mężczyzn",
         pch=19)
  })
  output$plot5 <- renderPlot({
    data <- dane %>%
      filter(country==as.character(input$Wyszukiwanie))
    boxplot(data$SDI,horizontal = TRUE,col="gray")
    stripchart(data$SDI,         
               method = "jitter", 
               pch = 19,          
               col = "purple",           
               add = TRUE) 
  })
  output$plot6 <- renderPlot({
    data <- dane %>%
      filter(country==as.character(input$Wyszukiwanie))
    plot(data$year, data$SDI, 
         xlab = "lata",
         ylab = "Indeks zrównoważonego rozwoju",
         pch=19)
  })
  output$plot7 <- renderPlot({
    data <- dane %>%
      filter(country==as.character(input$Wyszukiwanie))
    boxplot(data$HDI,horizontal = TRUE,col="gray")
    stripchart(data$HDI,         
               method = "jitter", 
               pch = 19,          
               col = "purple",           
               add = TRUE) 
  })
  output$plot8 <- renderPlot({
    data <- dane %>%
      filter(country==as.character(input$Wyszukiwanie))
    plot(data$year, data$HDI, 
         xlab = "lata",
         ylab = "Miernik opisujący stopień rozwoju społeczno-ekonomicznego",
         pch=19)
  })
  output$plot9 <- renderPlot({
    data <- dane %>%
      filter(country==as.character(input$Wyszukiwanie))
    boxplot(data$PKB,horizontal = TRUE,col="gray") 
    stripchart(data$PKB,         
               method = "jitter", 
               pch = 19,          
               col = "purple",           
               add = TRUE) 
  })
  output$plot10 <- renderPlot({
    data <- dane %>%
      filter(country==as.character(input$Wyszukiwanie))
    plot(data$year, data$PKB, 
         xlab = "lata",
         ylab = "Produkt krajowy brutto na osobę",
         pch=19)
  })
  output$op1K <-renderTable({
    data <- dane %>%
      filter(kontynent==as.character(input$kontynent))
    data.frame(cbind("Statystyka"=c("Kontynent","Minimum","Średnia","Odchylenie standardowe",
                                    "Mediana","Medianowe odchylenie bezwzgledne","Maksimum"),
                     "Wartość"=c(
                       input$kontynent,
                       round(min(data$dl_zycia), 2),
                       round(mean(data$dl_zycia), 2),
                       round(sd(data$dl_zycia), 2),
                       round(median(data$dl_zycia), 2),
                       round(mad(data$dl_zycia), 2),
                       round(max(data$dl_zycia), 2)
                     )
    )
    )
  })
  output$op2K <-renderTable({
    data <- dane %>%
      filter(kontynent==as.character(input$kontynent))
    data.frame(cbind("Statystyka"=c("Kontynent","Minimum","Średnia","Odchylenie standardowe",
                                    "Mediana","Medianowe odchylenie bezwzgledne","Maksimum"),
                     "Wartość"=c(
                       input$kontynent,
                       round(min(data$mezczyzni), 2),
                       round(mean(data$mezczyzni), 2),
                       round(sd(data$mezczyzni), 2),
                       round(median(data$mezczyzni), 2),
                       round(mad(data$mezczyzni), 2),
                       round(max(data$mezczyzni), 2)
                     )
    )
    )
  })
  output$op3K <-renderTable({
    data <- dane %>%
      filter(kontynent==as.character(input$kontynent))
    data.frame(cbind("Statystyka"=c("Kontynent","Minimum","Średnia","Odchylenie standardowe",
                                    "Mediana","Medianowe odchylenie bezwzgledne","Maksimum"),
                     "Wartość"=c(
                       input$kontynent,
                       round(min(data$SDI), 2),
                       round(mean(data$SDI), 2),
                       round(sd(data$SDI), 2),
                       round(median(data$SDI), 2),
                       round(mad(data$SDI), 2),
                       round(max(data$SDI), 2)
                     )
    )
    )
  })
  output$op4K <-renderTable({
    data <- dane %>%
      filter(kontynent==as.character(input$kontynent))
    data.frame(cbind("Statystyka"=c("Kontynent","Minimum","Średnia","Odchylenie standardowe",
                                    "Mediana","Medianowe odchylenie bezwzgledne","Maksimum"),
                     "Wartość"=c(
                       input$kontynent,
                       round(min(data$HDI), 2),
                       round(mean(data$HDI), 2),
                       round(sd(data$HDI), 2),
                       round(median(data$HDI), 2),
                       round(mad(data$HDI), 2),
                       round(max(data$HDI), 2)
                     )
    )
    )
  })
  output$op5K <-renderTable({
    data <- dane %>%
      filter(kontynent==as.character(input$kontynent))
    data.frame(cbind("Statystyka"=c("Kontynent","Minimum","Średnia","Odchylenie standardowe",
                                    "Mediana","Medianowe odchylenie bezwzgledne","Maksimum"),
                     "Wartość"=c(
                       input$kontynent,
                       round(min(as.numeric(data$PKB_per_capita)), 2),
                       round(mean(as.numeric(data$PKB_per_capita)), 2),
                       round(sd(as.numeric(data$PKB_per_capita)), 2),
                       round(median(as.numeric(data$PKB_per_capita)), 2),
                       round(mad(as.numeric(data$PKB_per_capita)), 2),
                       round(max(as.numeric(data$PKB_per_capita)), 2)
                     )
    )
    )
  })
  output$op2 <-renderTable({ 
    data <- dane %>%
      filter(country==as.character(input$Wyszukiwanie))
    data.frame(cbind("Statystyka"=c("Kraj","Minimum","Średnia","Odchylenie standardowe",
                                    "Mediana","Medianowe odchylenie bezwzgledne","Maksimum"),
                     "Wartość"=c(
                       input$Wyszukiwanie,
                       round(min(data$mezczyzni), 2),
                       round(mean(data$mezczyzni), 2),
                       round(sd(data$mezczyzni), 2),
                       round(median(data$mezczyzni), 2),
                       round(mad(data$mezczyzni), 2),
                       round(max(data$mezczyzni), 2)
                     )
    )
    )
  })
  output$op3 <-renderTable({ 
    data <- dane %>%
      filter(country==as.character(input$Wyszukiwanie))
    data.frame(cbind("Statystyka"=c("Kraj","Minimum","Średnia","Odchylenie standardowe",
                                    "Mediana","Medianowe odchylenie bezwzgledne","Maksimum"),
                     "Wartość"=c(
                       input$Wyszukiwanie,
                       round(min(data$SDI), 2),
                       round(mean(data$SDI), 2),
                       round(sd(data$SDI), 2),
                       round(median(data$SDI), 2),
                       round(mad(data$SDI), 2),
                       round(max(data$SDI), 2)
                     )
    )
    )
  })
  output$op4 <-renderTable({ 
    data <- dane %>%
      filter(country==as.character(input$Wyszukiwanie))
    data.frame(cbind("Statystyka"=c("Kraj","Minimum","Średnia","Odchylenie standardowe",
                                    "Mediana","Medianowe odchylenie bezwzgledne","Maksimum"),
                     "Wartość"=c(
                       input$Wyszukiwanie,
                       round(min(data$HDI), 2),
                       round(mean(data$HDI), 2),
                       round(sd(data$HDI), 2),
                       round(median(data$HDI), 2),
                       round(mad(data$HDI), 2),
                       round(max(data$HDI), 2)
                     )
    )
    )
  })
  output$op5 <-renderTable({ 
    data <- dane %>%
      filter(country==as.character(input$Wyszukiwanie))
    data.frame(cbind("Statystyka"=c("Kraj","Minimum","Średnia","Odchylenie standardowe",
                                    "Mediana","Medianowe odchylenie bezwzgledne","Maksimum"),
                     "Wartość"=c(
                       input$Wyszukiwanie,
                       round(min(data$PKB), 2),
                       round(mean(data$PKB), 2),
                       round(sd(data$PKB), 2),
                       round(median(data$PKB), 2),
                       round(mad(data$PKB), 2),
                       round(max(data$PKB), 2)
                     )
    )
    )
  })
}

shinyApp(ui, server)