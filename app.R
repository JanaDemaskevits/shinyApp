library(shiny)
library(ggcorrplot)
library(shinyjs)




# Load data ----
wine <- read.csv("wineQualityReds.csv",header=T,sep=",")
wine <- wine[,-1]  # ID väli eemaldamine



corr <- cor(wine)

variables <- names(wine)

# Define UI for app ----
ui <- fluidPage(
  
 
  # CSS binding ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  # App title ----
  titlePanel(("Veini kvaliteedi prognoos"), windowTitle = "Veini kvaliteedi prognoos"),
  
  # App help text ----
  helpText("ITB8812 Andmete visualiseerimise projekt. Jana Demaskevits"),
  br(),
  
   
  
  mainPanel(

    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(id="tabs1", type = "tabs",
                tabPanel("SISSEJUHATUS", 
                         sidebarLayout( 
                           sidebarPanel( 
                             img(src="wine.jpg", height='280px',width='210px')
                             ),
                         mainPanel(h3("Sissejuhatus"),
                                   br(),
                                   p("Selle projekti autor valis analüüsimiseks andmestiku Red Wine Dataset, mis sisaldab teavet punaste veinide keemiliste omaduste kohta, samuti professionaalide hinnanguid punaste veinide kvaliteedile. Andmestik on võetud https://www.kaggle.com/piyushgoyal443/red-wine-dataset veebilehe andmestikude andmebaasist. Andmestik on .csv formaadis. Valitud andmestik on uurimiseks avalik.[1]"),
                                   br(),
                                   h3("Projekti eesmärgid:"),
                                   br(),
                                   p("1. Tutvustada andmetega."), 
                                   p("2. Teostada andmete esialgse visuaalse analüüsi sõltuvuste defineerimiseks."), 
                                   p("3. Kursuses 'Andmete visualiseerimine' omandatud meetodite ja teadmiste rakendamine.")
                         ))),
                tabPanel("ANDMED",
                         sidebarLayout(
                         sidebarPanel( h3("Tunnused:"),
                                       
                                       br(),
                                       p(strong("fixed acidity"),"(tartaric acid - g / dm^3): enamus happeid, mis on seotud veiniga või fikseeritud või lendumatud (ei aurustu kergesti)"), 
                                       p(strong("volatile acidity")," (acetic acid - g / dm^3): äädikhappe kogus veinis, mis liiga kõrge sisalduse korral võib põhjustada ebameeldiva äädikamaitse"), 
                                       p(strong("citric acid")," (g / dm^3): väikestes kogustes leiduv sidrunhape võib lisada veinidele värskust ja maitset"),
                                       p(strong("residual sugar"),"(g / dm^3): pärast käärimise lõppu järelejäänud suhkrukogus. On haruldane leida veine, mille kogus on alla 1 grammi liitri kohta, ja veine, mille kogus on üle 45 grammi liitri kohta, peetakse magusaks)",
                                       p(strong("chlorides"),"(sodium chloride - g / dm^3: soola kogus veinis")),
                                       p(strong("free sulfur dioxide"), "(mg / dm^3): SO2 vaba vorm eksisteerib tasakaalus molekulaarse SO2 (lahustunud gaasina) ja vesiniksulfiidi iooni vahel; see takistab mikroobide kasvu ja veini oksüdeerumist"),
                                       p(strong("total sulfur dioxide"),"(mg / dm^3): S02 vabade ja seotud vormide kogus; madalates kontsentratsioonides ei ole SO2 veinis enamasti tuvastatav, kuid üle 50 ppm vaba SO2 kontsentratsiooni korral ilmneb SO2 veini ninas ja maitses"),
                                       p(strong("density"),"(g / cm^3): vee tihedus on lähedane vee tihedusele, sõltuvalt alkoholi- ja suhkrusisalduse protsendist"),
                                       p(strong("pH"), ": kirjeldab, kui happeline või aluseline on vein skaalal 0 (väga happeline) kuni 14 (väga aluseline); enamik veine jääb pH skaalal vahemikku 3-4"),
                                       p(strong("sulphates"),"(potassium sulphate - g / dm3): veinilisand, mis võib soodustada vääveldioksiidi (S02) taset, mis toimib antimikroobse ja antioksüdandina"),
                                       p(strong("alcohol")," (% by volume): veini alkoholisisalduse protsent"),
                                       p(strong("quality"),"(score between 0 and 10) - professionaalide hinnanguid punaste veinide kvaliteedile")
                         ),
                         mainPanel(
                           br(),
                           h3("Andmed:"), dataTableOutput("tabel")
                         ))),
                tabPanel("KORRELATSIOONIMAATRIKS",
                         sidebarLayout(
                           sidebarPanel(
                            
                             selectInput(
                               "select1",
                               "Visualisatsiooni meetod 1:",
                               choices = list("square" = 1, "circle" = 2),
                               selected = 1
                             ),
                             selectInput("select2", "Visualisatsiooni meetod 2:", choices = list("true" = 1, "false" = 2), 
                                         selected = 2),
                           ),
                           mainPanel(h3("Tunnuste korrelatsioonimaatriks:"),
                                     
                                    
                                     plotOutput("correlation"),
                                    br(),
                                    p("Tunnuse quality on määratletakse järgmised sõltuvused:
                                       
                                       tunnusega allcohol on keskmine kasvav sõltuvus (r=0.48), mis tähendab, et kõrgemat alkoholisisaldust peetakse üldiselt veini kvaliteetsemaks.
                                     
                                     Tunnusega volative.acidity on nõrg kahenev sõltuvus (r=-0.39), mis tähendab, mida suurem on lenduva happesuse sisaldus, seda madalam on kvaliteedi hindamine.
                                     
                                     Tunnuste citric.acid ja fixed.acidity vahel on tugev kasvav sõltuvus (r=0.67). Tunnuste fixed.acidit ja density vahel on tugev kasvav sõltuvus (r=0.67). Tunnuste total.sulfur.dioxide ja free.sulfur.dioxide vahel on tugev kasvav sõltuvus (r=0.67).
                                     
                                     Tunnuste pH ja fixed.acidity vahel on tugev kahenev sõltuvus (r=-0.68). Tunnuste citric.acid ja volative.acidity vahel on keskmine kahenev sõltuvus (r=-0.55). Tunnuste citric.acid ja ph vahel on keskmine kahenev sõltuvus (r=-0.54). Tunnuste density ja alcohol vahel on keskmine kahenev sõltuvus (r=-0.5).
                                     
                                     Teiste tunnuste vahel on nõrga tugevusega sõltuvused. Mõnedel juhtudel üldse sõltuvus puudub."
                           ),
                           br(),
                           )
                         )
                         ),
                tabPanel("KIRJELDAV ANALÜÜS",
                         sidebarLayout(
                           sidebarPanel(h3("Tunnused:"),
                           selectInput('selectVar', "", variables, selected = variables[[1]]),
                           br(),
                           sliderInput(inputId = "bins",
                                       label = "Number of bins:",
                                       min = 1,
                                       max = 50,
                                       value = 30),
                           
                                                     
                           
                           ),
                           
                           mainPanel(br(),
                                     
                                     fluidRow(
                                       column(width = 8,
                                              h3("Histogramm:"),
                                              plotOutput(outputId = "distPlot"),
                                       ),
                                       column(width = 8,
                                              h3("Boxplot:"),
                                              plotOutput("boxplot"),
                                       )
                                     ),
                             
                             
                             h3("Arvkarakteristikud:"),
                             br(),    
                             dataTableOutput('value')
                             
                           ) ))
                
    )))
   
  

server <- function(input, output) {

# Generate an HTML table view of the data ----
output$tabel <- renderDataTable( wine, options =
                                     list(searching = FALSE,ordering=F, lengthMenu = c(5, 10, 20),
                                          pageLength = 10, autoWidth = TRUE, scrollX = TRUE)) 

# Access the value of the widget with input$select
output$correlation <- renderPlot({
  if(input$select1 == 1 && input$select2 == 1) {ggcorrplot(corr, lab = TRUE)}
  else if(input$select1 == 1 && input$select2 == 2) {ggcorrplot(corr, lab = FALSE)}
  else if(input$select1 == 2 && input$select2 == 1) {ggcorrplot(corr, lab = TRUE, method = "circle")}
  else if(input$select1 == 2 && input$select2 == 2) {ggcorrplot(corr, lab = FALSE, method = "circle")}
})


output$value <- renderDataTable( summary(wine), options =
                                   list(searching = FALSE,ordering=F, 
                                        dom = 't')
  )
# 2. Its output type is a plot
output$distPlot <- renderPlot({

  req(input$selectVar)
  x <- wine[[input$selectVar]]

  bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
  hist(x, breaks = bins, col = "#FFB6C1", border = "grey",
       xlab = input$selectVar,
       ylab="Sagedus",
       main = "")
  
})
output$boxplot <- renderPlot({
  req(input$selectVar)
  x <- wine[[input$selectVar]]
  boxplot(x, col="#B0C4DE", horizontal=1, xlab=input$selectVar)
  points(mean(x), 1, col = "red", pch = 18)
  text(mean(x), 0.95, "mean", col="red", cex=0.5)
})
}





shinyApp(ui = ui, server = server, options = list(autoWidth = TRUE))
