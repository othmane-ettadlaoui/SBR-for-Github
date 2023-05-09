#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(shiny)
library(shinydashboard)
source("SBR_functions.R")
# Define UI for application that draws a histogram
ui <- dashboardPage(
  #Head
  dashboardHeader(
    title = "SBR Application",
    titleWidth = 300,
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://github.com/othmane-ettadlaoui",
        icon("github"),
        "github",
        target = "_blank"
      )
    )
  ),

  #Sidebar
  dashboardSidebar(id= "sidebar",
                   sidebarMenu(
                     menuItem(text = "CSR_marche",tabName = "CSR_marche"),
                     menuSubItem(text = "CSR_action",tabName = "CSR_action"),
                     menuSubItem(text = "CSR_taux",tabName = "CSR_taux"),
                     menuSubItem(text = "CSR_immobilier",tabName = "CSR_immobilier"),
                     menuItem(text = "option 2",tabName = "option2"),
                     menuItem(text = "option 3",tabName = "option3")
                   )
  ),

  #Body
  dashboardBody(
    tabItems(
      tabItem(tabName = "CSR_action",
              tabBox(id ="tb1",width = 12,
                     tabPanel("CSR_Action", h4("Calcule de CSR pour le risque Action"))
                     ),
              fluidRow(
                       box(status = "danger", solidHeader = T, width = 12, maximizable = T,
                           title = "CSR_Action",
                           numericInput("BE_action_Baisse", "BE_action_Baisse", value = 0),
                           numericInput("BE_action", "BE_action", value = 0),
                           actionButton("calculer", "Calculer"),
                           verbatimTextOutput("resultat")
                          ),
                       
                       
              )
      ),
      tabItem(tabName = "CSR_taux",
              tabBox(id ="tb2",width = 12,
                     tabPanel("CSR_taux", h4("Calcule de CSR pour le risque Taux")),
              ),
      fluidRow(
                box(status = "warning", solidHeader = T, width = 12, maximizable = T,
                    title = "CSR_taux",
                    numericInput("BE_taux_baisse", "BE_taux_baisse", value = 0),
                    numericInput("BE_taux_hausse", "BE_taux_hausse", value = 0),
                    numericInput("BE", "BE", value = 0),
                    actionButton("Tester", "Calculer"),
                    verbatimTextOutput("resultat1")      
              )
           )
      ),
      tabItem(tabName = "CSR_immobilier",
              tabBox(id ="tb3",width = 12,
                     tabPanel("CSR_immobilier", h4("Calcule de CSR pour le risque Immobilier")),
              ),
              fluidRow(
                box(status = "warning", solidHeader = T, width = 12, maximizable = T,
                    title = "CSR_immobilier",
                    numericInput("BE_Baisse", "BE_Baisse", value = 0),
                    numericInput("ME", "BE", value = 0),
                    actionButton("Tester1", "Calculer"),
                    verbatimTextOutput("resultat2")      
                )
              )
      ),
      tabItem(tabName = "option2",
              tabBox(id ="tb4",width = 12,
                     tabPanel("test12", h4("Test valide")),
                    )
            )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # CSR ACTION
  BE_action_Baisse1 = reactive({as.numeric(input$BE_action_Baisse)}) %>% bindEvent(input$calculer) 
  BE_action1 = reactive({as.numeric(input$BE_action)}) %>% bindEvent(input$calculer) 
  calcule = reactive(CSR_action(BE_action_Baisse1(), BE_action1())) 
  
  observeEvent(input$calculer, {
  output$resultat =renderPrint(paste("LE CSR ACTION EST ",calcule()))
  })
  
  
  # CSR ACTION
  BE_taux_baisse1 = reactive({as.numeric(input$BE_taux_baisse)}) %>% bindEvent(input$Tester) 
  BE_taux_hausse1 = reactive({as.numeric(input$BE_taux_hausse)}) %>% bindEvent(input$Tester) 
  BE1 = reactive({as.numeric(input$BE)}) %>% bindEvent(input$Tester) 
  calcule1 = reactive(CSR_taux(BE_taux_baisse1(), BE_taux_hausse1(), BE1())) 
  observeEvent(input$Tester, {
    output$resultat1 =renderPrint(paste("LE CSR Taux EST ",calcule1()))
  })
  
  #CSR_immobilier
  BE_Baisse1 = reactive({as.numeric(input$BE_Baisse)}) %>% bindEvent(input$Tester1) 
  BE2 = reactive({as.numeric(input$ME)}) %>% bindEvent(input$Tester1) 
  calcule2 = reactive(CSR_immobilier(BE_Baisse1(), BE2())) 
  observeEvent(input$Tester1, {
    output$resultat2 =renderPrint(paste("LE CSR Immobilier EST ",calcule2()))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
