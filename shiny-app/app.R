#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## app.R ##
library(shinydashboard)
library(shinycssloaders)
library(httr)
library(ggplot2)
library(dplyr)
source("FetchData.R")
source("Utils.R")

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(
              plotOutput("plot1", height = 250) %>% withSpinner()),
            box(
                title = "Controls",
                htmlOutput("selectClincNames")
            )
        ),
        fluidRow(
            box(plotOutput("plot2", height = 300) %>% withSpinner()),
            box(
                title = "Controls",
                htmlOutput("selectFactor"),
                htmlOutput("selectImplants")
            )
        ),
    )
)

server <- function(input, output, session) {
    last_updated_insertions <- reactiveVal(Sys.time())
    last_updated_implants <- reactiveVal(Sys.time())
    
    insertions <- reactivePoll(60*1000*10, session, 
                             checkFunc = function() checkFunction(last_updated_insertions),
                             valueFunc = getInsertions
                             )
    implants <- reactivePoll(60*1000*10, session, 
                               checkFunc = function() checkFunction(last_updated_implants),
                               valueFunc = getImplants
    )
    
    output$selectClincNames <- renderUI({
        selectInput("select1",
                    "Select Clinic",
                    choices  = unique(insertions()$ClinicId),
                    multiple = TRUE)
        })
    
    output$selectImplants <- renderUI({
        selectInput("select2",
                    paste("Select", input$select3, ""),
                    choices  = unique(implants()[[input$select3]]),
                    multiple = TRUE)
    })
    
    output$selectFactor <- renderUI({
        selectInput("select3",
                    "Select Factor",
                    choices  = implants() %>% select_if(is.factor) %>% names(),
                    selected = "Position"
                    )
    })
    
    output$plot1 <- renderPlot({
        insertions() %>%
        #Filter data based on input values
        filter(grepl(paste(input$select1, collapse = "|"), ClinicId)) %>%
          ggplot(aes(x = as.factor(ClinicId), y= AntibioticsDoseMg, col = Name)) +
            geom_boxplot()+
            labs(title = "AntibioticsDoseMg", x="Clinic")
    })
    
    output$plot2 <- renderPlot({
      
        
        filteredData <- implants() %>%
            #Filter data based on input values
            filter(grepl(paste(input$select2, collapse = "|"),  !!sym(input$select3)))
        
        Complications <- filteredData %>% 
            group_by_at(input$select3) %>%
            summarise(n = sum(Complications, na.rm = TRUE)) %>%
            mutate(Status = factor("Complications"))
        
        Success <- filteredData %>% 
            group_by_at(input$select3) %>%
            summarise(n = sum(Complications, na.rm = FALSE)) %>%
            mutate(Status = factor("Success"))
        
        rbind(Complications, Success) %>%
            ggplot(aes_string(x=input$select3, y="n", fill="Status")) +
            geom_col()
    })
    
}

shinyApp(ui, server)
