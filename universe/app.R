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
# Detect if in a local environment or deployed remotely
# if (basename(getwd()) == "fusionData") {
#   data(dictionary, package = "fusionData")
#   data(surveys, package = "fusionData")
# } else {
load("./www/dictionary.rda")
load("./www/surveys.rda")
#}

# Remove 'Type' variable from dictionary
dictionary <- subset(dictionary, select = -Type)

# Update the 'Survey' variable with full names
surveys <- merge(surveys, data.frame(Survey = c("ACS", "AHS", "CEI", "NHTS", "RECS", "ASEC"),
                                     `Survey name` = c("American Community Survey",
                                                       "American Housing Survey",
                                                       "Consumer Expenditure Survey (Interview)",
                                                       "National Household Travel Survey",
                                                       "Residential Energy Consumption Survey",
                                                       "Annual Social and Economic Supplement of the CPS"),
                                     check.names = FALSE),
                 all.x = TRUE) |>
  subset(select = c(Survey, `Survey name`, Vintage, Respondent, `Sample size`, `No. of variables`))

#-----------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(
    div(img(src = "fusionACS_badge.jpg", height = "115px", width = "105px", align = "left", style = "padding: 10px;margin-top: -15px"),
        'fusionACS Universal Survey Dictionary')),
  tabsetPanel(type = "tabs",
              tabPanel('Surveys', tableOutput('surveys')),
              tabPanel('Variables', DT::dataTableOutput('dictionary'))
  ),
  title = 'fusionACS Universal Survey Dictionary'
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
