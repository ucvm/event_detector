library(tidyr)
library(readr)
library(RColorBrewer)
library(magrittr)
library(tibble)
library(baseline)
library(pracma)
library(shiny)
library(shinythemes)
library(stringr)
library(dplyr)
library(ggplot2)
library(DT)
# library(multidplyr)
library(eventDetector)

# set env variable for zip to work
#Sys.setenv(R_ZIPCMD="/usr/bin/zip")

# setup multidplyr
# cluster = create_cluster(8)
# cluster %>%  cluster_eval({
#   library(eventDetector)
#   library(dplyr)
#   library(tidyr)
# })


ui = shinyUI(fluidPage(theme = shinytheme("spacelab"),

  titlePanel("Event detector"),

  sidebarLayout(
    sidebarPanel(
      h3("Data Load"),
      fileInput("datafile", "Choose the file to upload (csv only)",
                 accept = c('application/csv', '.csv', 'application/text'),
                 multiple = TRUE),
      uiOutput("channelui"),
      uiOutput("sectionui"),
      actionButton("loadbutton", "Load Data"),
      hr(),
      h3("Pre-filtering"),
      numericInput("track_size", "Select the minimum track size to analyze", value = 30, step = 1),
      h3("Peak detection parameters"),
      numericInput("minpeakheight", "Select the minimum peak intensity", value = 100, min = 0),
      numericInput("minpeakdistance", "Select the minimum distance between peaks (min)", value = 1, min = 1),
      h3("Plotting parameters"),
      checkboxInput("filterPeaks", "Show only tracks with peaks", FALSE),
      hr(),
      downloadButton("downloadData", "Download data")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
          h3("Table"),
          DT::dataTableOutput("summaryTable"),
          br(),
          h3("Track Plots"),
          plotOutput("singlePlot", height = "600px")),
        tabPanel("Peak Plots",
          h3("Number of peaks"),
          plotOutput("peak_num_plot"),
          h3("Width of peaks"),
          plotOutput("peak_width_plot")
        )
      )
    )
  )
)
)



server = shinyServer(function(input, output, session) {

  options(shiny.maxRequestSize=70*1024^2)

  # scan file and create ui for file load parameters

  scan = reactive({
    datafile = input$datafile
    if (is.null(datafile)) {
      return(NULL)
    }
    path = datafile$datapath
    return(scan_file(path))
  })

  output$channelui = renderUI({
    selectInput("channel", "Intensity Channel", choices = scan()$channels)
  })

  output$sectionui = renderUI({
    selectInput("section", "Section name", choices = scan()$sections)
  })

  raw = eventReactive(input$loadbutton, {
    datafile = input$datafile
    if (is.null(datafile)) {
      return(NULL)
    }
    path = datafile$datapath
    return(read_data(path, channel = input$channel, section = input$section) %>% filter_by_tracks(10))

  })

  # event finder --------------------------------------------------------------------------------

  # --- baseline correct (do this once when data loads)
  baseline_corrected = reactive({
    if (is.null(raw())) {
      return(NULL)
    }

    withProgress(message = "Baseline correcting...", value = 0, {

      incProgress(0.5)
      baseline_data = raw() %>%
        group_by(Well, Label) %>%
        do(baseline_correct(.))
      incProgress(1)
    })

    return(baseline_data)
  })


  # --- filter tracks
  filtered = reactive({
    if (is.null(baseline_corrected())) {
      return(NULL)
    }

    baseline_corrected() %>% filter_by_tracks(input$track_size)

  })

  # --- find peaks
  peaks = reactive({
    if (is.null(filtered())) {
      return(NULL)
    }

    # cluster %>%
    #   cluster_assign_expr("minpeakdistance", input$minpeakdistance) %>%
    #   cluster_assign_expr("minpeakheight", input$minpeakheight)

    filtered() %>%
      group_by(Well, Label) %>%
      do(detect_peaks(., minpeakheight = input$minpeakheight, minpeakdistance = input$minpeakdistance)) %>%
      collect()

  })


# Single plot ---------------------------------------------------------------------------------

  # --- track plot
  output$singlePlot = renderPlot({
    rows = input$summaryTable_rows_selected

    validate(
      need(length(rows) > 0, "Please select wells to plot")
    )

    rows = as.integer(rows)
    wells = well_stats()$Well[rows]
    plot_tracks(filtered(), peak_data = peaks(), filter_peaks = input$filterPeaks,
                  show_peaks = TRUE, well = wells )
  })


# Summary --------------------------------------------------------------------------

  # --- get well stats
  well_stats = reactive({
    get_stats(filtered(), peaks())
  })

  # --- render the summary table
  output$summaryTable = DT::renderDataTable({
    validate(
      need(peaks(), "Peaks not calculated")
    )
    well_stats() %>% datatable() %>% formatRound(c(2,5,9, 12))
  })

  # --- plot for the nubmer of peaks per well
  output$peak_num_plot = renderPlot({
    validate(
      need(peaks(), "Peaks not calculated")
    )
    plot_number_of_peaks(well_stats(), peaks())
  })

  # --- plot of the width of peaks per well
  output$peak_width_plot = renderPlot({
    validate(
      need(peaks(), "Peaks not calculated")
    )
    well_stats() %>% arrange(`Median peak width`) %>% plot_width_of_peaks(peaks())
  })


# Download handlers ---------------------------------------------------------------------------

  output$downloadData = downloadHandler(
    filename = function() {"results.zip"},
    content = function(fname) {
      setwd(tempdir())
      #cat(getwd(), file = stderr())
      files = c("summary_data.csv", "processed_data.csv", "peak_data.csv")
      write_csv(well_stats(), path = "summary_data.csv")
      write_csv(filtered(), path = "processed_data.csv")
      write_csv(peaks(), path = "peak_data.csv")

      zip(zipfile = fname, files = files)
    },
    contentType = "application/zip"
  )

})






# Run the application
shinyApp(ui = ui, server = server)

