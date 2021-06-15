library(tidyverse)
library(gt)
library(rhandsontable)
library(shinyjs)
library(shinyWidgets)
library(shiny)

#-----

# Load dictionary and survey summary data
dictionary <- readRDS("data/dictionary.rds")
surveys <- readRDS("data/surveys.rds")

# Functions specific to the 'harmony' app
source("R/harmony2dotR.R")
source("R/hfileList.R")

#-----

# Function to convert a numeric vector to integer, if possible
convertInteger <- function(x) if (all(x[!is.na(x)] %% 1 == 0)) as.integer(round(x)) else x

# Function to extract clean levels from dictionary 'Values'
clean <- function(x) gsub("[", "", gsub("]", "", str_squish(unlist(strsplit(x, split = "], ", fixed = T))), fixed = T), fixed = T)

# Pad a data frame with additional NA rows
pad <- function(x, n) {if (n > nrow(x)) x[(nrow(x) + 1):n, ] <- NA; return(x)}

# Function returns TRUE if 'x' has only one non-NA value
novary <- function(x) length(unique(na.omit(x))) == 1

# Parse a string containing numbers separated by commas
# NOTE: This correctly handles, for example, "10e3" character input (i.e. 10,000)
parseNumbers <- function(x) {
  x <- trimws(x)
  if (grepl(",\\s$", x) | grepl(",$", x)) str_sub(x, -1L, -1L) <- ""  # Remove comma at end of input string
  result <- eval(parse(text =  paste0("c(", x, ")")))  # Enclose the string in c() before parsing
  if (is.null(result)) "" else result
}

# Parse character string of numeric breaks points separate by commas and return bin labels
binLabels <- function(x, summary.values) {

  # TEST values
  # x <- "50e3, 70e3, 90e3, 120e3"  # Bin vector (character)
  # summary.values <- "Min: -15350, Median: 46600,  Mean: 75000, Max: 2940000"  # Known range in the data (used to validate)

  out <- if (length(x) == 0) {

    summary.values

  } else {

    if (x == "") {

      summary.values

    } else {

      # Min and max range parsed from the summary values string
      rng <- as.numeric(substring(grep("^M..:",  strsplit(summary.values, split = ", ", fixed = TRUE)[[1]], value = TRUE), first = 5))

      if (x == "*") {

        validate(need(diff(rng) <= 100, 'More than 100 integer values; too many to display'))
        rng[1]:rng[2]

      } else {

        # Coerce input to numeric
        y <- parseNumbers(x)

        # Validate input values
        validate(
          need(!anyNA(y), 'Input should be numbers separated by commas (ex: 3, 5, 7)'),
          need(!is.unsorted(y, strictly = TRUE), 'Values should be in ascending order'),
          need(y[1] > rng[1], paste0('First value must be greater than ', rng[1])),
          need(y[length(y)] <= rng[2], paste0('Last value must be less than or equal to ', rng[2]))
        )

        # Format 'y' for thousands separator
        #y <- format(y, big.mark = ",", scientific = FALSE, trim = TRUE)

        # Return labels
        c(paste0("Less than ", y[1]), if (length(y) > 1) paste0("[", y[-length(y)], " to ", y[-1], ")") else NULL, paste0(y[length(y)], " or more"))

      }
    }
  }
  return(out)
}

# readHarmony <- function(file) {
#   if (file.exists(file)) {
#     hfileList(dget(file))
#   } else {
#     NULL
#   }
# }

# Reactive trigger function
# https://github.com/daattali/advanced-shiny/tree/master/reactive-trigger
makeReactiveTrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}

#--------------------------------------------
#--------------------------------------------

ui <- fluidPage(

  # Set up shinyjs
  useShinyjs(),

  # Allow sweet alerts
  useSweetAlert(),

  # Custom CSS styling for disabled buttons
  inlineCSS(
    ".bttn[disabled] {
                cursor: not-allowed;
                opacity: 0.5;
              }"
  ),

  #-----------------

  titlePanel(title = 'fusionACS Survey Harmonization Tool'),

  hr(),

  # ROW WITH SURVEY SELECTION
  fluidRow(
    column(2,
           pickerInput(inputId = "dsvy", label = "Donor survey",
                       choices = setdiff(surveys$Survey, "ACS"))
    ),
    column(2,
           pickerInput(inputId = "dvint", label = "Donor vintage",
                       choices = unique(filter(surveys, Survey == setdiff(surveys$Survey, "ACS")[1])$Vintage))
    ),
    column(2,
           pickerInput(inputId = "rvint", label = "ACS vintage",
                       choices = unique(filter(surveys, Survey == "ACS")$Vintage))
    )
  ),

  # Tabs panel...
  tabsetPanel(

    # Main panel for performing harmonization
    tabPanel("Make harmony",

             br(),

             # ROW WITH SWITCH(ES)
             fluidRow(
               column(2,
                      disabled(actionBttn(
                        inputId = "submit",
                        label = "Submit harmony",
                        style = "gradient",
                        color = "success",
                        icon = icon("thumbs-up"),
                        size = "sm"
                      ))
               ),
               column(2,
                      disabled(actionBttn(
                        inputId = "delete",
                        label = "Delete harmony",
                        style = "gradient",
                        color = "warning",
                        icon = icon("trash-alt"),
                        size = "sm"
                      ))
               ),
               column(2,
                      materialSwitch(
                        inputId = "ordered",
                        label = "Ordered groups?",
                        status = "primary",
                        right = TRUE)
               ),
               column(2,
                      materialSwitch(
                        inputId = "restrict",
                        label = "Show only unharmonized variables",
                        status = "primary",
                        right = TRUE)
               )

             ),

             br(),

             # ROW WITH VARIABLE SELECTION
             fluidRow(
               column(4,
                      uiOutput("dvar_picker")
               ),
               column(4,
                      uiOutput("rvar_picker")
               )
             ),

             # ROW WITH NUMERIC BINNING (disabled if non-numeric variable selected)
             fluidRow(
               column(2,
                      disabled(textInput("dbin", label = "Bin breakpoints", width = '100%')),
               ),
               column(2,
                      textInput("dadj", label = "Adjustment", width = '100%'),
               ),
               column(2,
                      disabled(textInput("rbin", label = "Bin breakpoints", width = '100%')),
               ),
               column(2,
                      textInput("radj", label = "Adjustment", width = '100%'),
               )
             ),

             # ROW WITH COMMENT TEXT BOX
             fluidRow(
               column(6,
                      textAreaInput("comment", label = "Comments", width = '100%', resize = "vertical", height = '50px')
               ),
               column(2,
                      #uiOutput("ragg_picker")
                      pickerInput("ragg", label = "Household aggregator", choices = c("none", "reference", "sum", "min", "max", "mean", "median"))
               )
             ),

             hr(),

             # ROW WITH EDITABLE TABLE DATA
             fluidRow(
               column(4,
                      rHandsontableOutput("donor.df")
               ),
               column(4,
                      rHandsontableOutput("acs.df")
               )
             ),

             hr(),

             # ROW WITH SUMMARY HARMONIZATION TABLE
             fluidRow(
               column(8,
                      gt_output("htable")
               )
             )

    ),

    # Main panel for performing harmonization
    tabPanel("View harmonies",
             br(),
             fluidRow(
               column(8,
                      gt_output("harmonies")
               )
             )
    )

  ),

  textOutput("test1"),
  # textOutput("test2"),
  # textOutput("test3"),
  # tableOutput("test4")

)

#--------------------------------------------
#--------------------------------------------

server <- function(input, output, session) {

  # Update donor vintage choices based on selected donor survey
  observe({
    updatePickerInput(session, "dvint", choices = unique(filter(surveys, Survey == input$dsvy)$Vintage))
  })

  #-----

  # Trigger to force reading of harmonization file on disk
  # See here: https://github.com/daattali/advanced-shiny/tree/master/reactive-trigger
  H.trigger <- makeReactiveTrigger()

  # Read harmonization data on disk (NULL if no file exists)
  H <- reactive({
    H.trigger$depend()  # Trigger reload of data on disk
    hfile <- paste0("harmonies/", input$dsvy, "_", input$dvint, "__ACS_", input$rvint, ".R")
    if (file.exists(hfile)) {
      hfileList(dget(hfile))
    } else {
      NULL
    }
  })

  #------

  # Summary table displayed in the "View harmonies" tab
  output$harmonies <- render_gt({
    df <- H()$summary
    if (is.null(df)) df <- tibble(`Nothing yet...` = "Make some harmonies.")
    gt(df, rowname_col = "n") %>%
      cols_align("center") %>%
      opt_row_striping(row_striping = TRUE) %>%
      tab_options(table.align = "left")
  })

  #-----

  # Choice set for the donor and ACS variables lists
  choices <- reactive({

    d1 <- dictionary %>%
      filter(Survey == input$dsvy, Vintage == input$dvint) %>%
      mutate(long_desc = paste0(Variable, ": (", substring(Respondent, 1, 1), ") ", Description))
    if (input$restrict) d1 <- filter(d1, !Variable %in% H()$summary$Donor)

    d2 <- dictionary %>%
      filter(Survey == "ACS", Vintage == input$rvint) %>%
      mutate(long_desc = paste0(Variable, ": (", substring(Respondent, 1, 1), ") ", Description))
    if (input$restrict) d2 <- filter(d2, !Variable %in% H()$summary$ACS)

    list(
      donor.choices = list(
        Household = filter(d1, Respondent == "Household")$long_desc,
        Person = filter(d1, Respondent == "Person")$long_desc
      ),
      donor.vars = d1$Variable,

      acs.choices = list(
        Household = filter(d2, Respondent == "Household")$long_desc,
        Person = filter(d2, Respondent == "Person")$long_desc
      ),
      acs.vars = d2$Variable
    )

  })

  #-----

  # Create donor variable picker with choices set for the selected survey and vintage
  output$dvar_picker <- renderUI({
    pickerInput(inputId = "dvar", label = "Donor variable",
                choices = choices()$donor.choices,
                width = '100%',
                options = list(`live-search` = TRUE, size = 20)
    )
  })

  # Create ACS variable picker with choices set for the selected ACS vintage
  output$rvar_picker <- renderUI({
    pickerInput(inputId = "rvar", label = "ACS variable",
                choices = choices()$acs.choices,
                width = '100%',
                options = list(`live-search` = TRUE, size = 20)
    )
  })

  #-----

  # When the selected donor variable changes...

  # Row index in H() of the donor variable (NULL is the donor variable is unharmonized)
  Hind <- reactive({
    dv <- map_chr(str_split(input$dvar, ":"), 1)
    if (is.null(dv)) NULL else match(dv, H()$summary$Donor)
  })

  observeEvent(input$dvar, {

    i <- Hind()
    if (isTruthy(i)) {

      pchoices <- unlist(choices()$acs.choices)
      pselect <- pchoices[choices()$acs.vars == H()$summary$ACS[i]]
      updatePickerInput(session, "rvar", selected = pselect)

      # Update 'dbin' breakpoints
      updateTextInput(session, "dbin", value = H()$breaks1[[i]])

      # Update 'dadj' custom adjustment string
      updateTextInput(session, "dadj", value = H()$adj1[[i]])

      # Update 'rbin' breakpoints
      updateTextInput(session, "rbin", value = H()$breaks2[[i]])

      # Update 'radj' custom adjustment string
      updateTextInput(session, "radj", value = H()$adj2[[i]])

      # Update 'ragg' aggregator function selection
      updatePickerInput(session, "ragg", selected = H()$agg2[[i]])

    } else {

      reset("dbin")
      reset("dadj")
      reset("rbin")
      reset("radj")
      reset("ordered")
      reset("comment")
      updatePickerInput(session, "ragg", selected = "none")  # Using simple reset() caused problems

    }

  })

  # observeEvent(input$rvar, {
  #
  #   rv <- map_chr(str_split(input$rvar, ":"), 1)
  #   ind <- if (is.null(rv)) NA else match(rv, H()$summary$ACS)
  #   if (!is.na(ind)) {
  #
  #     # pchoices <- unlist(choices()$donor.choices)
  #     # pselect <- pchoices[choices()$donor.vars == H()$summary$Donor[ind]]
  #     # #updatePickerInput(session, "dvar", choices = pchoices, selected = pselect)
  #     # updatePickerInput(session, "dvar", selected = pselect)
  #
  #     # Update 'rbin' breakpoints
  #     updateTextInput(session, "rbin", value = H()$breaks2[[rv]])
  #
  #     # Update 'radj' custom adjustment string
  #     updateTextInput(session, "radj", value = H()$adj2[[rv]])
  #
  #     # Update 'ragg' aggregator function selection
  #     updatePickerInput(session, "ragg", selected = H()$agg2[[rv]])
  #
  #   } else {
  #
  #     reset("rbin")
  #     reset("radj")
  #     # reset("ordered")
  #     # reset("comment")
  #     reset("ragg")
  #
  #   }
  #
  # })

  #-----

  # Selected variables
  dvar <- reactive({map_chr(str_split(input$dvar, ":"), 1)})
  rvar <- reactive({map_chr(str_split(input$rvar, ":"), 1)})

  # Standard given name of the harmonized variable
  hvar <- reactive({paste(dvar(), rvar(), sep = "__")})

  # Is the current variable selection already harmonized?
  hexists <- reactive({
    if (length(hvar()) == 0) {
      FALSE
    } else {
      hvar() %in% H()$ids
    }
  })

  #-----

  # Update 'ordered' switch and comment text box
  observe({
    if (hexists()) {
      updateMaterialSwitch(session, "ordered", value = H()$ordered[[hvar()]])
      updateTextAreaInput(session, "comment", value = H()$comment[[hvar()]])
    }
  })

  #-----

  # Info about the selected donor variables
  dinfo <- reactive({filter(dictionary, Survey == input$dsvy, Vintage == input$dvint, Variable %in% dvar())})

  # Info about the selected ACS variables
  rinfo <- reactive({filter(dictionary, Survey == "ACS", Vintage == input$rvint, Variable %in% rvar())})

  #----

  # Are the selected variables numeric?
  dnum <- reactive({dinfo()$Type %in% c("int", "dbl")})
  rnum <- reactive({rinfo()$Type %in% c("int", "dbl")})

  # Enable/disable the "Bin breakpoint" input fields as applicable
  observe({toggleState(id = "dbin", condition = dnum())})
  observe({toggleState(id = "rbin", condition = rnum())})

  #----

  # Are the selected variables numeric or ordered?
  dord <- reactive({dinfo()$Type %in% c("int", "dbl", "ord")})
  rord <- reactive({rinfo()$Type %in% c("int", "dbl", "ord")})

  #----

  # Identify if this is a "special" case of household donor paired with person-level ACS
  special <- reactive({dinfo()$Respondent == "Household" & rinfo()$Respondent == "Person"})

  # CANNOT GET THIS TO WORK AS INTENDED
  # # Enable/disable the "Household aggregator" input field as applicable
  #observe({toggleState(id = "ragg", condition = special())})

  #----

  # Data frames with possibly binned variable labels; otherwise, original factor labels
  ddata <- reactive({
    d <- dinfo()
    if (nrow(d) > 0) {
      if (hexists()) {
        data.frame(levels = H()$levels1[[Hind()]],
                   Group = convertInteger(H()$groups1[[Hind()]])) %>%
          setNames(c(dvar(), "Group"))
      } else {
        dinfo() %>%
          transmute(Values = if (dnum()) list(binLabels(input$dbin, summary.values = Values)) else list(clean(Values))) %>%
          unnest(Values) %>%
          setNames(dvar()) %>%
          mutate(Group = 1L:n())
      }
    }
  })

  rdata <- reactive({
    d <- rinfo()
    if (nrow(d) > 0) {
      if (hexists()) {
        data.frame(levels = H()$levels2[[Hind()]],
                   Group = convertInteger(H()$groups2[[Hind()]])) %>%
          setNames(c(rvar(), "Group"))
      } else {
        rinfo() %>%
          transmute(Values = if (rnum()) list(binLabels(input$rbin, summary.values = Values)) else list(clean(Values))) %>%
          unnest(Values) %>%
          setNames(rvar()) %>%
          mutate(Group = 1L)
      }
    }
  })

  maxgroups <- reactive({
    max(nrow(ddata()), nrow(rdata()))
  })

  output$test4 <- renderTable(rinfo())

  #------

  # Create the rhandsontable objects (editable tables) where the user's work is done
  output$donor.df <- renderRHandsontable({
    d <- ddata()
    if (!is.null(d)) {
      d %>%
        select(everything(), Group) %>%  # Puts "Group" on right side of data frame
        rhandsontable(rowHeaders = NULL) %>%
        hot_col(col = dvar(), readOnly = TRUE) %>%
        hot_validate_numeric("Group", min = 0, max = maxgroups())
    } else {
      NULL
    }
  })

  output$acs.df <- renderRHandsontable({
    d <- rdata()
    if (!is.null(d)) {
      d %>%
        select(Group, everything()) %>%  # Puts "Group" on left side of data frame
        rhandsontable(rowHeaders = NULL) %>%
        hot_col(col = rvar(), readOnly = TRUE) %>%
        hot_validate_numeric("Group", min = 0, max = maxgroups())
    } else {
      NULL
    }
  })

  #----

  hot <- reactive({
    list(x = hot_to_r(input$donor.df),
         y = hot_to_r(input$acs.df))
  })

  #----

  observe({
    x <- hot()$x
    y <- hot()$y
    if (!is.null(x) & !is.null(y)) {
      if (!novary(x$Group) & !novary(y$Group)) {

        # Convert the input 'Group' values to a dense rank
        grps <- sort(unique(c(x$Group, y$Group)))
        x$Group <- match(x$Group, grps)
        y$Group <- match(y$Group, grps)

        # Detect if the grouping strategy is ordered
        dordered <- dord() & (!is.unsorted(x$Group, strictly = TRUE) | !is.unsorted(rev(x$Group), strictly = TRUE))
        rordered <- rord() & (!is.unsorted(y$Group, strictly = TRUE) | !is.unsorted(rev(y$Group), strictly = TRUE))
        gord <- dordered | rordered

        if (isTruthy(gord)) {
          if (gord) updateMaterialSwitch(session, "ordered", value = TRUE)
        }
      }
    }
  })

  #----

  # observe({
  #   if (!input$ordered) {
  #     updatePickerInput(session, "ragg", selected = "reference", choices = "reference")
  #   }
  # })

  #----

  # Data frame giving the current crosswalk grouping
  xwalk <- reactive({
    x <- hot()$x
    y <- hot()$y
    if (!is.null(x) & !is.null(y)) {
      grps <- sort(unique(c(x$Group, y$Group)))
      lapply(grps, function(g) {
        a <- filter(x, Group == g)
        b <- filter(y, Group == g)
        nrows <- max(nrow(a), nrow(b))
        a <- pad(a, nrows)
        b <- pad(b, nrows)
        out <- cbind(a, b[-1L])  # Removes "Group" column from 'b' (ACS data)
        out$Group <- g
        return(out)
      }) %>%
        bind_rows() %>%
        select(Group, everything()) %>%
        arrange()
    }
  })

  #----

  # Create gt() table of current grouping strategy to show the user
  output$htable <- render_gt({
    df <- xwalk()
    if (!is.null(df)) {
      df %>%
        group_by(Group) %>%
        mutate_at(vars(-group_cols()), ~ ifelse(is.na(.x) & n() == 1, "[No match]", .x)) %>%
        ungroup() %>%
        gt() %>%
        cols_align("center") %>%
        fmt_missing(columns = everything(), missing_text = "") %>%
        tab_options(table.align = "left") %>%
        tab_style(
          style = list(
            cell_fill(color = "lightgrey")
          ),
          locations = cells_body(
            rows = dense_rank(Group) %% 2 == 0)
        )
    }
  })

  #------

  # Determine when to enable the SUBMIT button
  observe({toggleState(id = "submit",
                       condition = all(c(
                         isTruthy(xwalk()),
                         ifelse(special(), input$ragg != "none", TRUE),
                         !dvar() %in% H()$summary$Donor | hexists(),
                         dnum() | !novary(hot()$x$Group),
                         rnum() | !novary(hot()$y$Group)
                       )))
  })

  # Determine when to enable the DELETE button
  observe({toggleState(id = "delete", condition = hexists())})

  #------

  # What to do when "Delete" is clicked
  observeEvent(input$delete, {

    # Load harmony file on disk
    hfile <- paste0(input$dsvy, "_", input$dvint, "__ACS_", input$rvint, ".R")
    h <- if (file.exists(hfile)) dget(hfile) else list()

    # Remove the selected harmony
    h[[hvar()]] <- NULL

    # Save harmonizaton .R file to disk
    harmony2dotR(h, hfile)

    # Reset input fields
    reset("dvar")
    reset("rvar")
    reset("ordered")
    reset("comment")
    reset("ragg")

    # Trigger reload of data on disk
    H.trigger$trigger()

  })

  #------

  # What to do when "Submit" is clicked
  observeEvent(input$submit, {

    # x <- hot_to_r(input$donor.df)
    # y <- hot_to_r(input$acs.df)
    x <- hot()$x
    y <- hot()$y

    # Convert the input 'Group' values to a dense rank
    grps <- sort(unique(c(x$Group, y$Group)))
    delta <- ifelse(0 %in% grps, -1L, 0L)
    x$Group <- match(x$Group, grps) + delta
    y$Group <- match(y$Group, grps) + delta

    # Prepare list object to insert into harmony file on disk
    out <- vector(mode = "list", length = 2)

    out[[1]]$groups = x$Group
    out[[1]]$levels = x[[dvar()]]
    out[[1]]$breaks <- parseNumbers(input$dbin)
    out[[1]]$adj <- input$dadj

    out[[2]]$groups = y$Group
    out[[2]]$levels = y[[rvar()]]
    out[[2]]$breaks <- parseNumbers(input$rbin)
    out[[2]]$adj <- input$radj
    out[[2]]$agg <- ifelse(input$ragg == "none", "", input$ragg)  # Set "none" to blank

    names(out) <- c(input$dsvy, "ACS")

    out$ordered <- ifelse(length(x$Group) == 1 | length(y$Group) == 1, "", input$ordered)  # Set to blank if numeric variable present
    out$comment <- gsub('"', "", gsub("'", "", input$comment, fixed = TRUE), fixed = TRUE)  # Removes any ' or " from the string (causes parsing errors)
    out$modified <- as.character(Sys.time())

    # Add result to existing harmonization list OR create a new one
    hfile <- paste0("harmonies/", input$dsvy, "_", input$dvint, "__ACS_", input$rvint, ".R")
    h <- if (file.exists(hfile)) dget(hfile) else list()

    # Assign results
    h[[hvar()]] <- out

    # Alphabetize harmonies
    h <- h[sort(names(h))]

    # Save harmonizaton .R file to disk
    submit <- harmony2dotR(h, hfile)

    # If styler::style_file() inside harmony2dotR() reports successful change, send success message
    if (submit$changed) {
      sendSweetAlert(
        session = session,
        title = "Success!",
        text = "All is right in the world...",
        type = "success"
      )
    }

    # Trigger reload of data on disk
    H.trigger$trigger()

  })

}

#---------

shinyApp(ui, server)
