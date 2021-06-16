#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Example from here: https://shiny.rstudio.com/gallery/datatables-options.html
# datatables formatting options: https://datatables.net/reference/option/
# Also see here: ?DT::datatable

library(shiny)

# Load dictionary and survey summary data
# dictionary <- subset(readRDS("dictionary/dictionary.rds"), select = -Type)
# surveys <- readRDS("dictionary/surveys.rds")

# dictionary <- subset(readRDS("data/dictionary.rds"), select = -Type)
# surveys <- readRDS("data/surveys.rds")

data(dictionary, package = "fusionData")
data(surveys, package = "fusionData")

dictionary <- subset(dictionary, select = -Type)

#-----------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel(div(img(src = "fusionACS_badge.jpg", height = "225px", width = "200px", align = "right"),
                   'fusionACS Universal Survey Dictionary')),
    #hr(),
    tabPanel('Survey summary', tableOutput('surveys')),
    #hr(),
    tabPanel('Main table', DT::dataTableOutput('dictionary'))
)

#-----------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$surveys <- renderTable(surveys)

    # Remove row names; for some reason 'rownames = FALSE' below does not work
    row.names(dictionary) <- NULL

    output$dictionary <- DT::renderDataTable(
        DT::datatable(dictionary,
                      rownames = FALSE,
                      filter = "top",
                      options = list(scrollX = TRUE,
                                     dom = 'tipr',  # See here: https://datatables.net/reference/option/dom
                                     pageLength = 25))
    )
}

#-----------------------------

# Run the application
shinyApp(ui = ui, server = server)
