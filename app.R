#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
            box(plotOutput("plot1", height = 250)),
            
            box(
                title = "Controls",
                htmlOutput("selectClincNames")
            )
        ),
        fluidRow(
            box(plotOutput("plot2", height = 300)),
            box(
                title = "Controls",
                htmlOutput("selectImplants")
            )
        )
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
                    "Select Implants",
                    choices  = unique(implants()$ImplantName),
                    selected = implants()$ImplantName[1],
                    multiple = TRUE)
    })
    
    output$plot1 <- renderPlot({
        ggplot(insertions() %>%
                   #Filter data based on input values
                   filter(grepl(paste(input$select1, collapse = "|"), ClinicId)),
               aes(x = as.factor(ClinicId), y= AntibioticsDoseMg, col = Name)) +
            geom_boxplot()
    })
    
    output$plot2 <- renderPlot({
        filteredData <- implants() %>%
            #Filter data based on input values
            filter(grepl(paste(input$select2, collapse = "|"), ImplantName))
        
        Complications <- filteredData %>% 
            group_by(ImplantName) %>%
            summarise(n = sum(Complications, na.rm = TRUE)) %>%
            mutate(Status = factor("Complications"))
        
        Success <- filteredData %>% 
            group_by(ImplantName) %>%
            summarise(n = sum(Complications, na.rm = FALSE)) %>%
            mutate(Status = factor("Success"))
        
        rbind(Complications, Success) %>%
            ggplot(aes(x = ImplantName, y=n, col=Status)) +
            geom_col()
    })
}

shinyApp(ui, server)
