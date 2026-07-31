# ==============================================================================
# Shiny Application: fusionACS Universal Survey Dictionary ('universe')
# ==============================================================================
# Purpose:
#   Provides an interactive web interface for exploring the master survey
#   metadata and variable dictionary compiled across microdata surveys
#   supported by the fusionACS project.
#
# Tabs:
#   - Surveys: Summary overview of supported surveys, vintages, respondent
#     levels, sample sizes, variable counts, and storage footprints.
#   - Variables: Searchable, filterable DataTables interface for exploring
#     harmonized variable identifiers, descriptions, values, and survey metadata.
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
})

# Load master dictionary and survey summary datasets
load("./www/dictionary.rda")
load("./www/surveys.rda")

# Merge full survey titles into the summary metadata for display clarity
surveys <- merge(surveys, data.frame(Survey = c("ACS", "AHS", "CEI", "NHTS", "RECS", "ASEC", "CPS", "FAPS", "GALLUP"),
                                     `Survey name` = c("American Community Survey",
                                                       "American Housing Survey",
                                                       "Consumer Expenditure Survey (Interview)",
                                                       "National Household Travel Survey",
                                                       "Residential Energy Consumption Survey",
                                                       "Annual Social and Economic Supplement of the CPS",
                                                       "Current Population Survey",
                                                       "National Household Food Acquisition and Purchase Survey",
                                                       "Gallup U.S."
                                     ),
                                     check.names = FALSE),
                 all.x = TRUE) %>%
  subset(select = c(Survey, `Survey name`, Vintage, Respondent, `Sample size`, `No. of variables`, `Size on disk (MB)`))

#-------------------------------------------------------------------------------
# User Interface (UI)
#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------
# Server Logic
#-------------------------------------------------------------------------------

server <- function(input, output) {

  output$surveys <- renderTable(surveys)

  # Explicitly reset row names to prevent automatic index column rendering in DT
  row.names(dictionary) <- NULL

  output$dictionary <- DT::renderDataTable(
    DT::datatable(dictionary,
                  rownames = FALSE,
                  filter = "top",
                  options = list(scrollX = TRUE,
                                 dom = 'tipr',  # NOTE: Has been deprecated in DataTables v2. See: https://datatables.net/reference/option/dom
                                 searchHighlight = TRUE,
                                 pageLength = 25))
  )
}

#-------------------------------------------------------------------------------
# Application Initialization
#-------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
