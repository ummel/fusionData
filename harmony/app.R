# Setup and package dependencies
suppressPackageStartupMessages({
  library(gt)
  library(rhandsontable)
  library(shinyjs)
  library(shinyWidgets)
  library(shiny)
  library(stringr)
  library(purrr)
  library(dplyr)
  library(tidyr)
  library(cli)
})

# Load dictionary and survey summary datasets
load("./www/dictionary.rda")
load("./www/surveys.rda")

# Helper functions for crosswalk manipulation and file parsing
source("R/harmony2dotR.R")
source("R/hfileList.R")

# Convert numeric vectors to integers when all non-NA values are whole numbers
convertInteger <- function(x) {
  if (all(x[!is.na(x)] %% 1 == 0)) as.integer(round(x)) else x
}

# Clean bracketed formatting from dictionary metadata strings
clean <- function(x) {
  gsub("[", "", gsub("]", "", str_squish(unlist(strsplit(x, split = "], ", fixed = TRUE)))), fixed = TRUE)
}

# Pad data frames with NA rows to maintain equal row counts during side-by-side display
pad <- function(x, n) {
  if (n > nrow(x)) x[(nrow(x) + 1):n, ] <- NA
  return(x)
}

# Check if a vector contains only a single distinct non-NA value
novary <- function(x) {
  length(unique(na.omit(x))) == 1
}

# Safely parse comma-separated numeric inputs (supports notation like "10e3")
parseNumbers <- function(x) {
  x <- trimws(x)
  if (grepl(",\\s$", x) | grepl(",$", x)) str_sub(x, -1L, -1L) <- ""
  result <- eval(parse(text = paste0("c(", x, ")")))
  if (is.null(result)) "" else result
}

# Construct interval bin labels from user breakpoint inputs and empirical range bounds
binLabels <- function(x, summary.values) {
  out <- if (length(x) == 0) {
    summary.values
  } else {
    if (x == "") {
      summary.values
    } else {
      # Extract minimum and maximum bounds from the summary metadata string
      rng <- as.numeric(substring(grep("^M..:", strsplit(summary.values, split = ", ", fixed = TRUE)[[1]], value = TRUE), first = 5))

      if (x == "*") {
        shiny::validate(need(diff(rng) <= 100, 'More than 100 integer values; too many to display'))
        rng[1]:rng[2]
      } else {
        y <- parseNumbers(x)

        shiny::validate(
          need(!anyNA(y), 'Input should be numbers separated by commas (ex: 3, 5, 7)'),
          need(!is.unsorted(y, strictly = TRUE), 'Values should be in ascending order'),
          need(y[1] > rng[1], paste0('First value must be greater than ', rng[1])),
          need(y[length(y)] <= rng[2], paste0('Last value must be less than or equal to ', rng[2]))
        )

        c(paste0("Less than ", y[1]),
          if (length(y) > 1) paste0("[", y[-length(y)], " to ", y[-1], ")") else NULL,
          paste0(y[length(y)], " or more"))
      }
    }
  }
  return(out)
}

# Reactive trigger pattern for forcing re-evaluation when disk state changes
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

# ==============================================================================
# User Interface
# ==============================================================================

ui <- fluidPage(

  useShinyjs(),
  useSweetAlert(),

  inlineCSS(
    ".bttn[disabled] {
      cursor: not-allowed;
      opacity: 0.5;
    }"
  ),

  titlePanel(title = 'fusionACS Survey Harmonization Tool'),

  hr(),

  # Survey selection controls
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

  tabsetPanel(

    # Crosswalk editing panel
    tabPanel("Make harmony",

             br(),

             # Action buttons and view toggles
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

             # Variable dropdown selectors
             fluidRow(
               column(4,
                      uiOutput("dvar_picker")
               ),
               column(4,
                      uiOutput("rvar_picker")
               )
             ),

             # Binning and value adjustment inputs
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

             # Additional metadata controls
             fluidRow(
               column(6,
                      textAreaInput("comment", label = "Comments", width = '100%', resize = "vertical", height = '50px')
               ),
               column(2,
                      pickerInput("ragg", label = "Household aggregator",
                                  choices = c("none", "reference", "sum", "min", "max", "mean", "median"))
               )
             ),

             hr(),

             # Interactive mapping tables
             fluidRow(
               column(4,
                      rHandsontableOutput("donor.df")
               ),
               column(4,
                      rHandsontableOutput("acs.df")
               )
             ),

             hr(),

             # Preview output
             fluidRow(
               column(8,
                      gt_output("htable")
               )
             )
    ),

    # Summary overview panel
    tabPanel("View harmonies",
             br(),
             fluidRow(
               column(8,
                      gt_output("harmonies")
               )
             )
    )
  ),

  textOutput("test1")
)

# ==============================================================================
# Server Logic
# ==============================================================================

server <- function(input, output, session) {

  cli::cli_alert_info("Initializing harmony interactive session.")

  # Update available donor vintages when donor survey selection changes
  observe({
    updatePickerInput(session, "dvint", choices = unique(filter(surveys, Survey == input$dsvy)$Vintage))
  })

  # Reactive trigger to track changes made to disk crosswalk files
  H.trigger <- makeReactiveTrigger()

  # Load the active harmony definition file from disk
  H <- reactive({
    H.trigger$depend()
    hfile <- paste0("harmonies/", input$dsvy, "_", input$dvint, "__ACS_", input$rvint, ".R")
    if (file.exists(hfile)) {
      hfileList(dget(hfile))
    } else {
      NULL
    }
  })

  # Render summary table of existing harmonized crosswalks
  output$harmonies <- render_gt({
    df <- H()$summary
    if (is.null(df)) df <- tibble(`Nothing yet...` = "Make some harmonies.")
    gt(df, rowname_col = "n") %>%
      cols_align("center") %>%
      opt_row_striping(row_striping = TRUE) %>%
      tab_options(table.align = "left")
  })

  # Filter available variables from dictionary metadata
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

  # Render searchable variable pickers
  output$dvar_picker <- renderUI({
    pickerInput(inputId = "dvar", label = "Donor variable",
                choices = choices()$donor.choices,
                width = '100%',
                options = list(`live-search` = TRUE, size = 20))
  })

  output$rvar_picker <- renderUI({
    pickerInput(inputId = "rvar", label = "ACS variable",
                choices = choices()$acs.choices,
                width = '100%',
                options = list(`live-search` = TRUE, size = 20))
  })

  # Find row index matching currently selected donor variable in existing summaries
  Hind <- reactive({
    dv <- map_chr(str_split(input$dvar, ":"), 1)
    if (is.null(dv)) NULL else match(dv, H()$summary$Donor)
  })

  # Populate or clear fields when donor variable selection updates
  observeEvent(input$dvar, {
    i <- Hind()
    if (isTruthy(i)) {
      pchoices <- unlist(choices()$acs.choices)
      pselect <- pchoices[choices()$acs.vars == H()$summary$ACS[i]]
      updatePickerInput(session, "rvar", selected = pselect)

      updateTextInput(session, "dbin", value = H()$breaks1[[i]])
      updateTextInput(session, "dadj", value = H()$adj1[[i]])
      updateTextInput(session, "rbin", value = H()$breaks2[[i]])
      updateTextInput(session, "radj", value = H()$adj2[[i]])
      updatePickerInput(session, "ragg", selected = H()$agg2[[i]])
    } else {
      reset("dbin")
      reset("dadj")
      reset("rbin")
      reset("radj")
      reset("ordered")
      reset("comment")
      updatePickerInput(session, "ragg", selected = "none")
    }
  })

  # Extract active variable keys and respondent levels
  dvar <- reactive({ map_chr(str_split(input$dvar, ":"), 1) })
  rvar <- reactive({ map_chr(str_split(input$rvar, ":"), 1) })

  dtype <- reactive({ ifelse(substring(map_chr(str_split(input$dvar, ":"), 2), 3, 3) == "H", "Household", "Person") })
  rtype <- reactive({ ifelse(substring(map_chr(str_split(input$rvar, ":"), 2), 3, 3) == "H", "Household", "Person") })

  hvar <- reactive({ paste(dvar(), rvar(), sep = "__") })

  # Check if active selection pair already has a saved crosswalk
  hexists <- reactive({
    if (length(hvar()) == 0) FALSE else hvar() %in% H()$ids
  })

  # Load saved comments and ordered switch state if crosswalk exists
  observe({
    if (hexists()) {
      updateMaterialSwitch(session, "ordered", value = H()$ordered[[hvar()]])
      updateTextAreaInput(session, "comment", value = H()$comment[[hvar()]])
    }
  })

  # Subset dictionary records for active variables
  dinfo <- reactive({ filter(dictionary, Survey == input$dsvy, Vintage == input$dvint, Respondent %in% dtype(), Variable %in% dvar()) })
  rinfo <- reactive({ filter(dictionary, Survey == "ACS", Vintage == input$rvint, Respondent %in% rtype(), Variable %in% rvar()) })

  # Check if selected variables are numeric
  dnum <- reactive({ dinfo()$Type %in% c("int", "dbl") })
  rnum <- reactive({ rinfo()$Type %in% c("int", "dbl") })

  # Enable breakpoint text inputs only for numeric variables
  observe({ toggleState(id = "dbin", condition = dnum()) })
  observe({ toggleState(id = "rbin", condition = rnum()) })

  # Check if variables are ordered or continuous
  dord <- reactive({ dinfo()$Type %in% c("int", "dbl", "ord") })
  rord <- reactive({ rinfo()$Type %in% c("int", "dbl", "ord") })

  # Detect mismatch between donor household and ACS person level variables
  special <- reactive({ dinfo()$Respondent == "Household" & rinfo()$Respondent == "Person" })

  # Generate initial category assignment data frames
  ddata <- reactive({
    d <- dinfo()
    if (nrow(d) > 0) {
      if (hexists()) {
        data.frame(levels = H()$levels1[[Hind()]], Group = convertInteger(H()$groups1[[Hind()]])) %>%
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
        data.frame(levels = H()$levels2[[Hind()]], Group = convertInteger(H()$groups2[[Hind()]])) %>%
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

  maxgroups <- reactive({ max(nrow(ddata()), nrow(rdata())) })

  output$test4 <- renderTable(rinfo())

  # Render interactive group assignment tables
  output$donor.df <- renderRHandsontable({
    d <- ddata()
    if (!is.null(d)) {
      d %>%
        select(everything(), Group) %>%
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
        select(Group, everything()) %>%
        rhandsontable(rowHeaders = NULL) %>%
        hot_col(col = rvar(), readOnly = TRUE) %>%
        hot_validate_numeric("Group", min = 0, max = maxgroups())
    } else {
      NULL
    }
  })

  # Reactively extract current handsontable inputs
  hot <- reactive({
    list(x = hot_to_r(input$donor.df), y = hot_to_r(input$acs.df))
  })

  # Automatically toggle "ordered" switch if category index mappings are monotonic
  observe({
    x <- hot()$x
    y <- hot()$y
    if (!is.null(x) & !is.null(y)) {
      if (!novary(x$Group) & !novary(y$Group)) {
        grps <- sort(unique(c(x$Group, y$Group)))
        x$Group <- match(x$Group, grps)
        y$Group <- match(y$Group, grps)

        dordered <- dord() & (!is.unsorted(x$Group, strictly = TRUE) | !is.unsorted(rev(x$Group), strictly = TRUE))
        rordered <- rord() & (!is.unsorted(y$Group, strictly = TRUE) | !is.unsorted(rev(y$Group), strictly = TRUE))
        gord <- dordered | rordered

        if (isTruthy(gord)) {
          if (gord) updateMaterialSwitch(session, "ordered", value = TRUE)
        }
      }
    }
  })

  # Merge category tables by group index into a single preview structure
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
        out <- cbind(a, b[-1L])
        out$Group <- g
        return(out)
      }) %>%
        bind_rows() %>%
        select(Group, everything()) %>%
        arrange()
    }
  })

  # Render active crosswalk preview table
  output$htable <- render_gt({
    df <- xwalk()
    if (!is.null(df)) {
      df %>%
        group_by(Group) %>%
        mutate_at(vars(-group_cols()), ~ ifelse(is.na(.x) & n() == 1, "[No match]", .x)) %>%
        ungroup() %>%
        gt() %>%
        cols_align("center") %>%
        sub_missing(columns = everything(), missing_text = "") %>%
        tab_options(table.align = "left") %>%
        tab_style(
          style = list(cell_fill(color = "lightgrey")),
          locations = cells_body(rows = dense_rank(Group) %% 2 == 0)
        )
    }
  })

  # Enable submission button when inputs pass validation requirements
  observe({
    toggleState(id = "submit",
                condition = all(c(
                  isTruthy(xwalk()),
                  ifelse(special(), input$ragg != "none", TRUE),
                  !dvar() %in% H()$summary$Donor | hexists(),
                  dnum() | !novary(hot()$x$Group),
                  rnum() | !novary(hot()$y$Group)
                )))
  })

  # Enable deletion button if selected crosswalk exists on disk
  observe({ toggleState(id = "delete", condition = hexists()) })

  # Delete crosswalk definition from disk
  observeEvent(input$delete, {
    hfile <- paste0("harmonies/", input$dsvy, "_", input$dvint, "__ACS_", input$rvint, ".R")
    h <- if (file.exists(hfile)) dget(hfile) else list()

    h[[hvar()]] <- NULL

    harmony2dotR(h, hfile)
    cli::cli_alert_warning(paste0("Removed crosswalk entry: ", hvar()))

    reset("dvar")
    reset("rvar")
    reset("ordered")
    reset("comment")
    reset("ragg")

    H.trigger$trigger()
  })

  # Save or update crosswalk definition on disk
  observeEvent(input$submit, {
    x <- hot()$x
    y <- hot()$y

    grps <- sort(unique(c(x$Group, y$Group)))
    delta <- ifelse(0 %in% grps, -1L, 0L)
    x$Group <- match(x$Group, grps) + delta
    y$Group <- match(y$Group, grps) + delta

    out <- vector(mode = "list", length = 2)

    out[[1]]$groups = x$Group
    out[[1]]$levels = x[[dvar()]]
    out[[1]]$breaks <- parseNumbers(input$dbin)
    out[[1]]$adj <- input$dadj

    out[[2]]$groups = y$Group
    out[[2]]$levels = y[[rvar()]]
    out[[2]]$breaks <- parseNumbers(input$rbin)
    out[[2]]$adj <- input$radj
    out[[2]]$agg <- ifelse(input$ragg == "none" | length(input$ragg) == 0, "", input$ragg)

    names(out) <- c(input$dsvy, "ACS")

    out$ordered <- ifelse(length(x$Group) == 1 | length(y$Group) == 1, "", input$ordered)
    out$comment <- gsub('"', "'", input$comment, fixed = TRUE)
    out$modified <- as.character(Sys.time())

    hfile <- paste0("harmonies/", input$dsvy, "_", input$dvint, "__ACS_", input$rvint, ".R")
    h <- if (file.exists(hfile)) dget(hfile) else list()

    h[[hvar()]] <- out
    h <- h[sort(names(h))]

    submit <- harmony2dotR(h, hfile)
    cli::cli_alert_success(paste0("Saved crosswalk entry: ", hvar()))

    if (submit$changed) {
      sendSweetAlert(
        session = session,
        title = "Success!",
        text = "All is right in the world...",
        type = "success"
      )
    }

    H.trigger$trigger()
  })
}

# Launch standalone Shiny application
shinyApp(ui, server)
