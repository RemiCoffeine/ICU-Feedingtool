library(shinythemes)
library(bslib)
source("Ernaerung_alltogether.R",  encoding="utf-8")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"), #(theme = shinytheme("cerulean"),
  
  
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: aqua;  color:black}
    .tabbable > .nav > li > a[data-value='Ergebnis'] {background-color: red;   color:white}
    .tabbable > .nav > li > a[data-value='Eingabe'] {background-color: blue;  color:white}
    
  ")),
  
  
  
  
  fluidRow(width = 12,
           align = "center",
    column(width = 12,
  titlePanel("ICU-Feedingtool v.0.5"),
  tags$cite("Work in progress! Kalkulationen auf Grundlage von: DGEM-Leitlinie: Klinische Ernaehrung in der Intensivmedizin."),
  br(),
  tags$cite("Bugreports an: johannes.krell@mri.tum.de, 2022"),
  )),
  br(),
  br(),
  tabsetPanel(
    
    
    
 
 
    
    tabPanel("Eingabe",
  wellPanel(fluidRow(
    column(
           width = 12,
           align = "center",
           
 
           tags$cite("Wassereinlagerungen von aktuellem Gewicht abziehen."),
      sliderInput("weight", label = h3("Gewicht (kg)"), 
                  min = 40,
                  max = 200,
                  value = 60),
      
      sliderInput("hight", label = h3("Groesse (cm)"), 
                  min = 140,
                  max = 220,
                  value = 170),
      
      tags$cite("Vergangenge Zeit seit letzter Homoeostasestoerung."),
      sliderInput("day", label = h3("Tag nach Stoerung"), 
                   min = 0,
                   max = 20,
                   value = 1),
      
                
      conditionalPanel(
        condition = "input.day >= 3",
        tags$cite("schrittweise Steigerung bis volle Kalorienzahl"),
        sliderInput("current_kals", label = h3("aktuelle Ernaehrung in kal/Tag"), 
                    min = 0,
                    max = 3000,
                    value = 1000)
      ),
      conditionalPanel(
        condition = "input.day > 1",
        sliderInput(inputId = "insulin",
                    label = "Insulin in IE/h:",
                    min = 0,
                    max = 10,
                    value = 0)
      ),
     
      
    
  
  
      
      
      sliderInput("feedingtime", label = h3("Ernaehrungsstunden am Tag"),
                  min = 10,
                  max = 24,
                  value = 23),
      
      conditionalPanel(
        condition = "input.dialysis_choice == 2 && input.day >= 2 ",
        sliderInput("phosphate", label = h3("Phosphat im Labor"), step = 0.1,
                    min = 0,
                    max = 4,
                    value = 1)
      ),
      
      
      sliderInput("propofol", label = h3("propofol_mg_kg_h"), step = 0.5,
                  min = 0,
                  max = 10,
                  value = 0),
      
      
      
      selectInput("feed", label = h3("Welche Ernaehrung?"), 
                  choices = list("Fresubin" = 1, "Fresubin_Energy" = 2, "SK_hepatisch" = 4, "SK_jejunal" = 5, "SK_renal_low_protein" = 6,
                                 "SK_renal_high_protein" = 7, "Olimel_57" = 9 , "olimel_76" = 10, "Smovkabiven" = 11), 
                  selected = 1),
      
      selectInput("protein", label = h3("Welche Proteinquelle?"), 
                  choices = list("Proteinpulver" = 8, "Aminoplasmal" = 12), 
                  selected = 8),
      
      
      radioButtons("dialysis_choice", label = h3("Dialyseverfahren?"),
                   choices = list("Ja" = 1, "Nein" = 2), 
                   selected = 2),
      
      
      
      conditionalPanel(
        condition = "input.dialysis_choice == 1",
        
        radioButtons("protein_loss", label = h3("Dialyseverfahren"),
                     choices = list("CVVHD" = 1, "Genius_dialyse" = 2, "keine Dialyse" =3), 
                     selected = 3),
        sliderInput(inputId = "dialysis_hours",
                    label =  h3("Dialysestunden pro Tag"),
                    min = 0,
                    max = 24,
                    value = 0),
      )),
    
    tags$h3("Weitere Daten"),
    tableOutput(outputId = "stats"),
      
    
  )
  )),


tabPanel("Ergebnis", 
         wellPanel(fluidRow(
           column(
             align = "center",
             tableOutput(outputId = "calories"),width = 12,
             br(),
             tableOutput(outputId = "dosage"),
             br(),
             tableOutput(outputId = "vitamins"),
           ))),
)),
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  table<-reactive(  makros_needed(feeds= feeds, feeds_selection = as.numeric(input$feed), Propofol = input$propofol,day = input$day,hight=input$hight,  weight = input$weight, insulin = input$insulin, current_cal_total= input$current_kals, phosphate = input$phosphate,
                                  protein_source =as.numeric(input$protein), dialysis =as.numeric(input$protein_loss), hours_per_day_dialysis=input$dialysis_hours, possible_lost_protein=possible_lost_protein, feedingtime = input$feedingtime)) 
  
 
  

  output$calories <- renderTable(table()[[1]])
  output$dosage <- renderTable(table()[[2]])
  output$vitamins <- renderTable(table()[[3]])
  output$stats <- renderTable(table()[[4]])
  
}



shinyApp(ui = ui, server = server)
