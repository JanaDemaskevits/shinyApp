library(shiny)
library(ggcorrplot)
library(ggplot2)
library(FSA)


# Load data ----
wine <- read.csv("data/wineQualityReds.csv",header=T,sep=",")
corr <- cor(wine[-1])
variables <- names(wine[-1])


# Define UI for app ----
ui <- fluidPage(
  
  
  # CSS binding ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  # App title ----
  titlePanel(("Veini kvaliteedi analüüs"), windowTitle = "Veini kvaliteedi analüüs"),
  
  # App help text ----
  helpText("ITB8812 Andmete visualiseerimise projekt. Jana Demaskevits"),
  br(),
  
  
  
  mainPanel(width = '1200px',
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(id="tabs1",  type = "tabs",
                        tabPanel("SISSEJUHATUS", 
                                 sidebarLayout( 
                                   sidebarPanel( 
                                     img(src="wine2.jpg", height='280px',width='355px')
                                   ),
                                   mainPanel(h3("Sissejuhatus"),
                                             br(),
                                             p("Selle projekti autor valis analüüsimiseks andmestiku Red Wine Dataset, mis sisaldab teavet punaste veinide keemiliste omaduste kohta, samuti professionaalide hinnanguid punaste veinide kvaliteedile. Andmestik on võetud ",
                                               a("https://www.kaggle.com/piyushgoyal443/red-wine-dataset", href = "https://www.kaggle.com/piyushgoyal443/red-wine-dataset"), "veebilehe andmestikude andmebaasist. Andmestik on .csv formaadis. Valitud andmestik on uurimiseks avalik.[P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553. ISSN: 0167-9236.[WWW] http://dx.doi.org/10.1016/j.dss.2009.05.016 [Pre-press (pdf)] http://www3.dsi.uminho.pt/pcortez/winequality09.pdf [bib] http://www3.dsi.uminho.pt/pcortez/dss09.bib (05.12.2021)]"),
                                             
                                             h3("Projekti eesmärgid:"),
                                             br(),
                                             p("Projekti eesmärk on viia läbi kirjeldav analüüs, uurida ja leida punase veini kirjeldavate andmete sõltuvusi ning nende mõju veini kvaliteedile.
Ning ka vaadata kas teised muutujad on omavahel korrelatsioonis. Tulemuste visualiseerimiseks kasutada kursuses 'Andmete visualiseerimine' omandatud meetodid ja teadmised."), 
                                             
                                             h3("Rakenduse ülevaade:"),
                                             br(),
                                             p("Vaheleht 1:  Sissejuhatus ja projekti eesmärgid."),
                                             p("Vaheleht 2:  Tunnuste nimekirja koos kirjeldusega ning ka tabel andmetega."),
                                             p("Vaheleht 3:  Histogramm andmeväärtuste esinemissageduse vaatamiseks ja karp-vurrud diagramm väärtuste jaotuse ja erindite vaatamiseks sõltuvalt valitud muutujast ja tabel arvkarakteristikutega."),
                                             p("Vaheleht 4:  Korrelatsioonimaatriks, kus on võimalik näha muutujate vaheliste seoste olemasolu ja mõju kvaliteedimuutujale."),
                                             p("Vaheleht 5:  Tabel ning ka karp-vurrud diagramm koos valitud muutuja väärtuste jaotusega sõltuvalt kvaliteedimuutuja väärtusest."),
                                             br()
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
                                                 p(strong("pH"), "kirjeldab, kui happeline või aluseline on vein skaalal 0 (väga happeline) kuni 14 (väga aluseline); enamik veine jääb pH skaalal vahemikku 3-4"),
                                                 p(strong("sulphates"),"(potassium sulphate - g / dm3): veinilisand, mis võib soodustada vääveldioksiidi (S02) taset, mis toimib antimikroobse ja antioksüdandina"),
                                                 p(strong("alcohol")," (% by volume): veini alkoholisisalduse protsent"),
                                                 p(strong("quality"),"(score between 0 and 10) - professionaalide hinnanguid punaste veinide kvaliteedile")
                                   ),
                                   mainPanel(
                                     br(),
                                     p("Sellel vahelehel kuvatakse tunnuste nimekirja koos kirjeldusega ning ka tabel andmetega – objektide arv, tunnuste nimed ja neid väärtused."),
                                     h3("Andmed:"), 
                                     dataTableOutput("tabel")
                                   ))),
                        
                        tabPanel("KIRJELDAV ANALÜÜS",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput('selectVar', "Vali tunnust:", variables, selected = variables[[1
                                     ]]),
                                     br(),
                                     sliderInput(inputId = "bins",
                                                 label = "Vali intervallide arv:",
                                                 min = 1,
                                                 max = 50,
                                                 value = 30
                                     ),
                                   ),
                                   
                                   mainPanel(br(),
                                             p("Sellel vahelehel kuvatakse histogramm andmeväärtuste esinemissageduse vaatamiseks ja karp-vurrud diagramm väärtuste jaotuse ja erindite vaatamiseks sõltuvalt valitud muutujast. Allpool on võimalik näha ka tabel arvkarakteristikutega."),
                                             
                                             fluidRow(
                                               column(6,
                                                      h3("Histogramm:"),
                                                      plotOutput("distPlot"),
                                               ),
                                               column(6,
                                                      h3("Karp-vurrud diagramm:"),
                                                      plotOutput("boxplot"),
                                               ),
                                               column(3,
                                                      h3("Arvkarakteristikud:"),
                                                      br(),    
                                                      dataTableOutput("table2"),
                                                      br()
                                               )
                                               
                                             ),
                                             
                                             
                                            
                                             
                                   ) )),
                        tabPanel("KORRELATSIOONIMAATRIKS",
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     radioButtons("radio", label = p("Vali maatriksi tüüp"),
                                                  choices = list("ruut" = 1, "ring" = 2), 
                                                  selected = 1
                                                  ),
                                    
                                     checkboxInput("checkbox", label = p("koefitsientidega"), value = FALSE)
                                   ),
                                   mainPanel(br(),
                                     p("Sellel vahelehel kuvatakse korrelatsioonimaatriks, kus on võimalik näha muutujate vaheliste seoste olemasolu ja mõju kvaliteedimuutujale."),
                                     h3("Korrelatsioonimaatriks:"),
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
                        tabPanel("SÕLTUVUS TUNNUSEST",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput('selectVar2', "Vali tunnust:", variables[-12], selected = variables[[1
                                     ]]),
                                     br(),
                                   ),
                                   
                                   mainPanel(
                                     br(),
                                     p("Sellel vahelehel kuvatakse tabel ning ka karp-vurrud diagramm koos valitud muutuja väärtuste jaotusega sõltuvalt kvaliteedimuutuja väärtusest."),
                                     br(),
                                     h3("Kokkuvõte kvaliteedi tunnuse sõltuvust valitud tunnusest:"),
                                     dataTableOutput("table4"),
                                     h3("Karp-vurrud diagramm:"),
                                     plotOutput("boxplot3")
                                     
                                     
                                   ) ))
            )))



server <- function(input, output) {
  data_input <- reactive({
    req(input$selectVar2)
    x <- wine[[input$selectVar2]]
  })
  
  data_input2 <- reactive({
    req(input$selectVar)
    x <- wine[[input$selectVar]]
  })
  
  checkVar <- function(input) {
 
    
    if(input == 'fixed.acidity' || input == 'volatile.acidity' || input == 'citric.acid' || input == 'residual.sugar' || input == 'chlorides' || input == 'sulphates')
    {unit = 'g / dm^3'}
    if(input == 'free.sulfur.dioxide' || input == 'total.sulfur.dioxide') 
    {unit = 'mg / dm^3'}
    if(input == 'pH') 
    {unit = 'score'}
    if(input == 'alcohol') 
    {unit = '%'}
    if(input == 'quality') 
    {unit = 'score'}
    
    xlab_name = sprintf("%s (%s)", input, unit)
  }
    
    
 
  
  # Generate an HTML table view of the data ----
  output$tabel <- renderDataTable(wine[1:13], options =
                                     list(searching = FALSE,ordering=T, lengthMenu = c(5, 10, 20),
                                          pageLength = 10, autoWidth = TRUE, scrollX = TRUE)) 
  
  # Access the value of the widget with input$radio and input$checkbox
  output$correlation <- renderPlot({
    if(input$radio == 1 && input$checkbox  == TRUE) {ggcorrplot(corr, lab = TRUE)}
    else if(input$radio == 1 && input$checkbox  == FALSE) {ggcorrplot(corr, lab = FALSE)}
    else if(input$radio == 2 && input$checkbox  == TRUE) {ggcorrplot(corr, lab = TRUE, method = "circle")}
    else if(input$radio == 2 && input$checkbox  == FALSE) {ggcorrplot(corr, lab = FALSE, method = "circle")}
   
    })
  
  # Generate an HTML table view of the numerical characteristics ----
  output$table2 <- renderDataTable(summary(wine[c(sprintf("%s", input$selectVar))]), options =
                                      list(searching = FALSE,ordering=F, 
                                           dom = 't')
  )
  
  # Generate a histogramm
  output$distPlot <- renderPlot({
    
    bins <- seq(min(data_input2()), max(data_input2()), length.out = input$bins + 1)
    
    hist(data_input2(), breaks = bins, col = "#FF336A", border = "black",
         xlab =checkVar(input$selectVar),
         ylab="Sagedus",
         main = "")
  })
  
  # Generate a boxplot 
  output$boxplot <- renderPlot({
    boxplot(data_input2(), col=rgb(0,0,1,0.5), horizontal=1, xlab=checkVar(input$selectVar))
    points(mean(data_input2()), 1, col = "red", pch = 18)
    text(mean(data_input2()), 0.95, "mean", col="red", cex=0.5)
  })
  
  # Generate a boxplot 
  output$table4 <- renderDataTable(
    Summarize(data_input()~quality, data=wine),options =
                                                 list(searching = FALSE, ordering=F, 
                                                      dom = 't')
 )
  
  # Generate a boxplot 
  output$boxplot3 <- renderPlot({
    boxplot(data_input()~wine$quality, data=wine, col = "#00FF7F", ylab=checkVar(input$selectVar2), xlab=sprintf("%s (%s)", variables[[12]], "score"))
    kesk <- tapply(data_input(), wine$quality, mean)
    points(1:6, kesk, col = "red", pch = 18)

  })
   
}
shinyApp(ui = ui, server = server)
