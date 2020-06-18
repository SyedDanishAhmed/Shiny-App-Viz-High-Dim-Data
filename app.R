# Title : Multivariate data visualization tool using muHVT
# Created : Jan 17, 2019 2:00 PM
# Author : Syed Danish Ahmed
# This code creates a Shiny tool for visualizing multivariate data.
# Revisions :
#   Version : 19.01.02
# Date : Jan 17, 2019 6:00 PM
# Author : Syed Danish Ahmed
# Changes :
#   * Added viz. for sub-tessellations
#   Version : 19.01.03
# Date : Jan 18, 2019 3:00 PM
# Author : Syed Danish Ahmed
# Changes :
#   * Added heatmap
#   Version : 19.01.04
# Date : Jan 23, 2019 7:00 PM
# Author : Syed Danish Ahmed
# Changes :
#   * Fixed bugs in D3 plot and added toggle for delaunay triangulation
#   * Fixed aspect ratio for muHVT charts
#   * Added help text in sidebar
#   * Updated app header
#   * Added feature to ignore columns from loaded dataset 
#   Version : 19.01.05
# Date : Jan 24, 2019 6:00 PM
# Author : Syed Danish Ahmed
# Changes :
#   * Updated help text for mean absolute deviation instead of Euclidean
#   * Fixed aspect ratio for muHVT charts
#   * Updated the slider title to "Number of Clusters at each Level"
#   * Fixed bug - Filtered dataset was not getting used in muHVT plots
#   * Fixed bug - Removed train test
#   Version : 19.01.06
# Date : Jan 29, 2019 8:00 PM
# Author : Syed Danish Ahmed
# Changes :
#   * Updated location and improved visual appearance of Submit button for ignoring columns
#   * Suppressed ggplotly error messages
#   * Updated hover text for HVT plot with Level, Parent and Child
#   Version : 19.01.07
# Date : Feb 01, 2019 2:00 PM
# Author : Syed Danish Ahmed
# Changes :
#   * Updated range of slider for maximum number of clusters to 45
#   * Progress bar for plot rendering status
#   * Highlight cells on hover in D3 Heatmap
#   * Correct formula for Quantization Error calculation
#   * Limit #decimal points in Quantization Error Summary table
#   Version : 19.02.01
# Date : Feb 06, 2019 8:00 PM
# Author : Syed Danish Ahmed
# Changes :
#   * Updated the muHVT algorithm - Check QE at each step
#   * Updated headers for sliders on left pane
#   * Updated range of values for QE thresold
#   * Bug fix - Displaying actual depth instead on user input depth
#   * Formatted decimal points
#   * Removed search option from Dataset tab
#   * Reduced scroll bar height
#   * Updated maximum number of clusters from 45 to 40
#   Version : 19.02.02
# Date : Feb 12, 2019 10:52 PM
# Author : Sangeet Moy Das
# Changes :
#   * Made hvt.results eventReactive in turn making it work on clicking submit
#   * Segment table not updating issue fixed
#   * Fixed issue which arose when switching from 1D to 2D, 2D to 1D – rendered blank pages sometimes
#   * Added a SUBMIT button, which would trigger model building & will trigger the changes on tweaking the parameters
#   * Changed minimum number of the cluster to 3
#   Version : 19.02.03
# Date : Feb 18, 2019 05:05 PM
# Author : Syed Danish Ahmed
# Changes :
#   * Provided option to select error metric
#   * Replaced muHVT local with CRAN version
#   * Provided default depth as 1
#   * Made number of clusters dynamic (divide nrow by 3^d where d is depth) and updated helptext
#   * Removed row name from quantization error summary table
#   * Created a new dataset using torus code named torus_data.csv
#   * Removed ggplotly - This affected height and title
#   * Added height to plotOutput, also removed extra br() tags
#   * Added theme in ggplot for title size, alignment & margin
#   * Updated line width and centroid size for all plots
#   * Added function to round off all numeric columns to given decimal places
#   * Replaced progress bar with spinner loader
#   * Removed manipulations to Quantization Error from hvt.results
#   * Fixed bug - Using actualDepth in hvt.results


version <- "19.02.03"

# Install missing packages
list.of.packages <- c("dplyr", "muHVT", "shiny", "ggplot2", "plotly", "DT", "r2d3", "purrr", "tidyr", "rstudioapi","R.utils","magrittr")
# list.of.packages <- c("dplyr", "shiny", "ggplot2", "plotly", "DT", "r2d3", "purrr", "tidyr", "rstudioapi","R.utils","magrittr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

reactData <- shiny::reactiveValues()

# Import required libraries
library(dplyr)
library(muHVT)
library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(r2d3)
library(purrr)
library(tidyr)
library(rstudioapi)
library(R.utils)
library(magrittr)
library(shinycssloaders)

# Getting the path of your current open file
current_path <- getActiveDocumentContext()$path
# Set the working directory to the relevant one:
setwd(dirname(current_path ))

# Setting theme for removing axis titles
th = theme(axis.title=element_blank(),
           axis.text=element_blank(),
           axis.ticks=element_blank())

# UI logic
ui <- fluidPage(
  
  # Application title
  titlePanel(title = tags$div(style='background-color:lightblue; background-color: #428bca; border-radius: 4px; padding-bottom: 5px; padding-top: 10px; padding-left: 10px;'
                              #tags$div(img(src = 'Logos/logo.png', title = 'Mu Sigma Inc.',
                               #            height = "60px")
                                #       ,style='float:left; margin-left: 10px; margin-right: 18px;')
                              ,tags$div(span('Hierarchical Voronoi Tessellations App'
                                             , style='font-size:30px;color:white;font-family:"Courier",cursive;font-weight:bolder'))
                              , span(style='font-size:14px;color:white',paste0('v',version))), windowTitle = "Hierarchical Voronoi Tessellation"),
  
  br(),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(width = 3,
                 
                 # Input: Select a file
                 fileInput("file1", "Choose CSV File",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 
                 # Input: Checkbox if file has header
                 checkboxInput("header", "Header", TRUE),
                 helpText(tags$em("Note: Data is Z-score normalized")),
                 
                 uiOutput("nclustVal"),
                 
                 sliderInput("depth",
                             "Maximum Depth",
                             min = 1,
                             max = 3,
                             value = 1),
                 
                 uiOutput("err"),
                 
                 helpText(tags$em("Note: Mean absolute deviation is used for error calculation. Non-numeric fields are ignored")),
                 
                 radioButtons(inputId = "errorMetric", label = "Select Error Metric", choices = c("mean", "max"), selected = "mean"),
                 
                 uiOutput("submit_param"),
                 br()
                 
    ),
    
    mainPanel(width = 9,
              
              # Output: Tabset w/ plot, summary, and table
              tabsetPanel(type = "tabs",
                          tabPanel("Dataset",
                                   htmltools::tags$div(title="Click here to ignore columns", br(),
                                                       # selectInput("select", "Select Columns to be Ignored",uiOutput("mySlider"), multiple = TRUE),
                                                       fluidRow((column(3,
                                                                        uiOutput("dataUI"))),
                                                                column(6,
                                                                       shiny::actionButton("update", "Submit", class = "btn",style="width: 75px;
                                                                                           height: 33px;
                                                                                           margin-top: 24px;
                                                                                           color: #ffffff;
                                                                                           font-size: 14px;
                                                                                           background-color: #2d6396;
                                                                                           border: 1px solid #2d6396;")))),
                                   br(),
                                   htmltools::tags$div(title="Click here to ignore columns",DT::dataTableOutput("datasetHead"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;")),
                          tabPanel("Hierarchical Voronoi Tessellation", br(), withSpinner(plotOutput("distPlot", height = 500)), br(), withSpinner(tableOutput("summary"))),
                          tabPanel("Heatmap", br(), uiOutput("mySlider"), withSpinner(plotOutput("heatMap", height = 500)), br(), withSpinner(tableOutput("summary2"))),
                          tabPanel("D3",
                                   h2(textOutput("text")),
                                   h4(em("Heatmap based on Quantization Error")),
                                   img(src='Logos/cool.png', align = "right",width = "200" ),
                                   checkboxInput("delaunay", "Show Delaunay Triangulations", FALSE),
                                   d3Output("d3", height = "500px")),
                          
                          
                          tabPanel("Quantization Error Summary",
                                   br(),
                                   br(),
                                   withSpinner(DT::dataTableOutput("logs")),
                                   style = "height:600px; overflow-y: scroll;overflow-x: scroll;")), tags$style(type="text/css",
                                                                                                                ".shiny-output-error { visibility: hidden; }",
                                                                                                                ".shiny-output-error:before { visibility: hidden; }")
                                                       )
                                   )
                          )


roundOffDataFrame <- function(data, roundOff = FALSE, precision = 4)
{
  #
  # data: a data frame; roundOff: TRUE/FALSE/NULL; precision: numeric/integer/NULL
  #
  if(roundOff) {
    indices <- base::which(base::unname(base::unlist(base::sapply(data, CheckColumnTypeNew))) == 'numeric')
    invisible(
      lapply(indices, function(x){
        data[, x] <<- base::round(data[, x, drop=F], precision)
      })
    )
  }
  return(data)
}


CheckColumnTypeNew <- function(dataVector) {
  #Check if the column type is "numeric" or "character" & decide type accordDingly
  if (base::class(dataVector) == "integer" || class(dataVector) == "numeric") {
    columnType <- "numeric"
  } else { columnType <- "character" }
  #Return the result
  return(columnType)
}

# Server logic

server <- function(session, input, output) {
  
  
  
  output$text <- renderText("Voronoi Tessellations")
  
  dataset <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    inFile <- input$file1
    
    # Set the seed for reproducible results
    set.seed(240)
    
    round_df <- function(df, digits) {
      nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
      
      df[,nums] <- round(df[,nums], digits = digits)
      
      (df)
    }
    
    # Error handling to avoid error display if file is not read properly
    tryCatch(
      {
        # Load data from csv file
        computers <- read.csv(inFile$datapath, header=T)
        
        trainComputers <- computers
        
        # Round numeric columns to 2 places of decimal points
        trainComputers <- round_df(trainComputers, digits=2)
        
        trainComputers
      },
      error = function(e) {
      }
    )
    
  })
  
  
  filtereddata <- eventReactive({
    input$update
    dataset()
  }, {
    req(dataset())
    if(is.null(input$select) || input$select == "")
      dataset()
    else
      dataset()[, !colnames(dataset()) %in% input$select]
  })
  
  observeEvent(dataset(), {
    updateSelectInput(session, "select", choices=colnames(dataset()))
  })
  
  output$dataUI <- renderUI({
    selectInput("select", "Select Columns to be Ignored",names(dataset()), multiple = TRUE)
  })
  
  
  hvt.results <-  eventReactive(input$submit_param,{
    # hvt_func <- function(){
    tryCatch(
      {
        
        # Initializing a list for hvt results
        hvt.results <- list()
        
        nums <- unlist(lapply(filtereddata(), is.numeric))
        trainComputers <- filtereddata()[ , nums]
        
        ncol_data <- ncol(trainComputers)
        
        ncol_data <- ifelse(is.numeric(ncol_data),ncol_data,0)
        
        
        hvt.results <- muHVT::HVT(trainComputers,
                                  nclust = input$nclust,
                                  depth = input$depth,
                                  # quant.err = input$quant.err * ncol_data,
                                  quant.err = input$quant.err,
                                  projection.scale = 10,
                                  normalize = T,
                                  error_metric = input$errorMetric)
        
        # tryCatch({
        #   hvt.results[[3]]$summary$Quant.Error <- hvt.results[[3]]$summary$Quant.Error / ncol_data
        # },error = function(e) {
        #   hvt.results[[3]]$summary$Quant.Error <- 0
        # })
        
        
        hvt.results
        
      },
      error = function(e) {
        # print(e)
      }
    )
  })
  
  
  
  
  output$datasetHead <- renderDataTable({
    DT::datatable(filtereddata(), options = list(pageLength = 5, searching=FALSE, lengthMenu = list(c(5, 10, 20, 30, 40, -1), list('5', '10', '20', '30','40', 'All')), paging = T))
  }
  
  )
  
  
  get_coordinates <- function(level,parent,child,data){
    x = NA
    try(
      assign("x",paste0(data[[level]][[parent]][[child]][["pt"]], collapse = ",")),
      silent = T
    )
    # print(x)
    return(x)
  }
  
  map_coordinates <- function(data){
    data[[3]]$summary %>%
      mutate(coord = pmap_chr(list(Segment.Level,Segment.Parent,Segment.Child),get_coordinates,
                              data =  data[[2]])) %>%
      separate(coord,c("x","y"), sep = ",") %>%
      mutate(x = as.numeric(x)) %>%
      mutate(y = as.numeric(y)) %>%
      mutate(Level =paste0("",Segment.Level,
                           "\nParent: ",Segment.Parent,
                           "\nChild: ",Segment.Child))
    
  }
  
  
  
  output$distPlot <- renderPlot({
    
    # Parameters for plotHVT function
    #
    # hvt.results - A list containing the ouput of HVT function which has the details of the tessellations to be plotted
    #
    # line.width - A vector indicating the line widths of the tessellation boundaries for each level
    #
    # color.vec - A vector indicating the colors of the boundaries of the tessellations at each level
    #
    # pch1 - Symbol type of the centroids of the tessellations (parent levels). Refer points. (default = 21)
    #
    # centroid.size - Size of centroids of first level tessellations. (default = 3)
    #
    # title - Set a title for the plot. (default = NULL)
    
    # Error handling to avoid error display on UI when file is not uploaded
    # tryCatch({
    
    
    # Create a Progress object
    # withProgress(message = 'Rendering plot', value = 0.5, {
    #   n <- 10
    #   for (i in 1:n) {
    #     incProgress(n, detail = paste("Please wait!"))
    #   }
      
      
      
      m <- list(
        l = 50,
        r = 50,
        b = 50,
        t = 50,
        pad = 0
      )
      
      # d <- as.integer(reactData$depth)
      
      # hvt_result_obj = hvt_func()
      actualDepth = NROW(hvt.results()[[3]][["compression_summary"]])
      
      # line.width = c(0.6,0.4,0.2)
      line.width = c(1.2,0.8,0.4)
      # line.width.subset = line.width[1:d]
      line.width.subset = line.width[1:actualDepth]
      color.vec = c("#141B41","#0582CA","#8BA0B4")
      # color.vec.subset = color.vec[1:d]
      color.vec.subset = color.vec[1:actualDepth]
      
      # suppressMessages(ggplotly(muHVT::plotHVT(hvt.results(),
      #                                   line.width = line.width.subset,
      #                                   color.vec = color.vec.subset,
      #                                   centroid.size = 1) + ggtitle(paste("Hierarchical Voronoi Tessellation With Depth = ", actualDepth)) +
      #                             geom_point(data = map_coordinates(hvt.results()),aes(x = x ,y = y ,name = Level), alpha = 0) +
      #                             scale_x_continuous(expand = c(0, 0)) +
      #                             scale_y_continuous(expand = c(0, 0)) + th, height = 500, tooltip = "name")) %>% layout(margin = m) %>% config(displayModeBar = FALSE)
      
      muHVT::plotHVT(hvt.results(),
                     line.width = line.width.subset,
                     color.vec = color.vec.subset,
                     centroid.size = 3)+ ggtitle(paste("Hierarchical Voronoi Tessellation With Depth = ", actualDepth)) +
        theme(plot.title = element_text(size = 20, hjust = 0.5,margin=margin(0,0,20,0))) +
        geom_point(data = map_coordinates(hvt.results()),aes(x = x ,y = y ,name = Level), alpha = 0) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) + th
      
      
      # }, warning = function(w) {
      #   # warning-handler-code
      #   # print(w)
      # }, error = function(e) {
      #   # error-handler-code
      #   # print(e)
      # }, finally = {
      #   # cleanup-code
      # })
      #
    # })
  })
  
  
  output$heatMap <- renderPlot({
    
    # hvt.results - A list of hvt.results obtained from the HVT function.
    #
    # dataset - A dataframe containing the variables that you want to overlay as heatmap. The user can pass an external dataset or the dataset that was used to perform hierarchical vector quantization. The dataset should have same number of points as the dataset used to perform hierarchical vector quantization in the HVT function
    #
    # child.level - A number indicating the level for which the heat map is to be plotted.
    #
    # hmap.cols - The column number of column name from the dataset indicating the variables for which the heat map is to be plotted. To plot the quantization error as heatmap, pass 'quant_error'. Similary to plot the no of points in each cell as heatmap, pass 'no_of_points' as a parameter
    #
    # color.vec - A color vector such that length(color.vec) = child.level (default = NULL)
    #
    # line.width - A line width vector such that length(line.width) = child.level. (default = NULL)
    #
    # palette.color - A number indicating the heat map color palette. 1 - rainbow, 2 - heat.colors, 3 - terrain.colors, 4 - topo.colors, 5 - cm.colors, 6 - BlCyGrYlRd (Blue,Cyan,Green,Yellow,Red) color. (default = 6)
    #
    # show.points - A boolean indicating if the centroids should be plotted on the tesselations. (default = FALSE)
    
    # Create a Progress object
    #progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    #on.exit(progress$close())
    #progress$set(message = "Rendering plot ...", value = 0)
    
    # withProgress(message = 'Rendering plot', value = 0.5, {
    #   n <- 10
    #   for (i in 1:n) {
    #     incProgress(1/n, detail = paste("Please be patient!"))
    #   }
      
      
      # tryCatch({
      m <- list(
        l = 50,
        r = 50,
        b = 50,
        t = 50,
        pad = 0
      )
      
      # d <- as.integer(input$depth)
      actualDepth = NROW(hvt.results()[[3]][["compression_summary"]])
      
      # line.width = c(0.6,0.4,0.2)
      line.width = c(1.2,0.8,0.4)
      # line.width.subset = line.width[1:d]
      line.width.subset = line.width[1:actualDepth]
      color.vec = c("#141B41","#0582CA","#8BA0B4")
      # color.vec.subset = color.vec[1:d]
      color.vec.subset = color.vec[1:actualDepth]
      
      centroid.size = c(3,2,1.1)
      centroid.size.subset = centroid.size[actualDepth]
      
      nums <- unlist(lapply(filtereddata(), is.numeric))
      trainComputers <- filtereddata()[ , nums]
      
      
      # g <- suppressMessages(ggplotly(muHVT::hvtHmap(hvt.results(),
      #                       trainComputers,
      #                       child.level = d,
      #                       hmap.cols = input$select.variable,
      #                       line.width = line.width.subset,
      #                       color.vec = color.vec.subset,
      #                       palette.color = 6,
      #                       centroid.size = 1,
      #                       show.points = T) + ggtitle(paste("Hierarchical Voronoi Tessellation with", input$select.variable, "heatmap overlay")) +
      #                 scale_x_continuous(expand = c(0, 0)) +
      #                 scale_y_continuous(expand = c(0, 0)) + th, height = 500)) %>% layout(margin = m) %>% config(displayModeBar = FALSE) 
      # 
      # 
      # p <- plotly_build(g)
      # p$x$layout$annotations[[1]]$text <- ""
      # 
      # p
      
      muHVT::hvtHmap(hvt.results(),
                     trainComputers,
                     child.level = actualDepth,
                     hmap.cols = input$select.variable,
                     line.width = line.width.subset,
                     color.vec = color.vec.subset,
                     palette.color = 6,
                     centroid.size = centroid.size.subset,
                     show.points = T) + ggtitle(paste("Hierarchical Voronoi Tessellation with", input$select.variable, "heatmap overlay")) +
        theme(plot.title = element_text(size = 20, hjust = 0.5,margin=margin(0,0,20,0))) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) + th 
      
      # }, warning = function(w) {
      #   # warning-handler-code
      # }, error = function(e) {
      #   # error-handler-code
      # }, finally = {
      #   # cleanup-code
      # })
      
    # })
  })
  
  
  output$mySlider <- renderUI({
    
    d <- filtereddata()
    nums <- unlist(lapply(d, is.numeric))
    d <- d[ ,nums]
    
    selectInput("select.variable", label = HTML(paste0("<b>","Select Feature for Overlaying Heatmap","</b>")),
                choices = cbind(c("quant_error", "no_of_points"),sort(names(d))), selected = 1)
  })
  
  
  output$summary <- renderTable({
    
    # Code to view Quantization Error for each cluster
    # hvt.results()[[3]][['summary']][,1:5]
    
    # Check truthiness
    req(hvt.results()[[3]]$compression_summary)
    
    # Code to remove summary section instead of displaying NULL when no file uploaded
    if(is.null(hvt.results()[[3]]$compression_summary)){
      
      removeUI(
        selector = '#summary'
      )
      
    }
    
    # Creating a summary table to viz. quantization error for each cluster
    result <- hvt.results()[[3]]$compression_summary
    
    result$segmentLevel <- as.integer(result$segmentLevel)
    
    result
    
  })
  
  output$logs <- DT::renderDataTable({
    
    # withProgress(message = 'Rendering plot', value = 0.5, {
    #   n <- 10
    #   for (i in 1:n) {
    #     incProgress(n, detail = paste("Please wait!"))
    #   }
      
      ncolSummary <-  ncol(hvt.results()[[3]]$summary)
      
      DT::datatable(hvt.results()[[3]]$summary,
                    rownames= FALSE,
                    options = list(pageLength = 10,
                                   lengthMenu = list(c(10, 20, 30, 40, -1), list('10', '20', '30','40', 'All')),
                                   paging = T, searching=FALSE)) %>%
        formatRound(c(5:ncolSummary), 2) %>%
        formatStyle(columns = c(5:ncolSummary), 'text-align' = 'center')
      
    # })
    
  })
  
  output$err <- renderUI({
    
    ncol_data <- ncol(filtereddata())
    
    max_value <- ifelse(is.numeric(ncol(filtereddata())),round(ncol(filtereddata())/reactData$depth),0)
    
    max = 1.5
    
    value = 0.2
    
    sliderInput("quant.err",
                "Quantization Error Threshold",
                min = 0,
                max = max,
                value = value)
  })
  
  
  output$nclustVal <- renderUI({
    
    nrow_data <- NROW(filtereddata())
    
    d <- input$depth
    
    if(d == 1)
    {
      max = round(nrow_data/3,0)
    }
    else if(d == 2)
    {
      max = round(nrow_data/9,0)
    }
    else if(d == 3)
    {
      max = round(nrow_data/27,0)
    }
    
    sliderInput("nclust",
                "Number of Clusters at each Level",
                min = 1,
                max = max,
                value = 15)
  })
  
  output$submit_param <- renderUI({
    
    ncol_data <- ncol(filtereddata())
    
    max_value <- ifelse(is.numeric(ncol(filtereddata())),round(ncol(filtereddata())/reactData$depth),0)
    
    actionButton("submit_param", "Submit",class = "btn",style="width: 75px;
                 height: 33px;
                 margin-top: 24px;
                 color: #ffffff;
                 font-size: 14px;
                 background-color: #2d6396;
                 border: 1px solid #2d6396;")
  })
  
  output$maxQuantError <- renderText({
    paste0("Note: Select Quantization Error below ",floor(max(hvt.results()[[3]]$summary$Quant.Error,na.rm = T)), " to view plot")
  })
  
  output$summary2 <- renderTable({
    
    # Check truthiness
    req(hvt.results()[[3]]$compression_summary)
    
    # Code to remove summary section instead of displaying NULL when no file uploaded
    if(is.null(hvt.results()[[3]]$compression_summary)){
      
      removeUI(
        selector = '#summary'
      )
      
    }
    
    # Creating a summary table to viz. quantization error for each cluster
    result <- hvt.results()[[3]]$compression_summary 
    
    result$segmentLevel <- as.integer(result$segmentLevel)
    
    result
    
  })
  
  # observeEvent(input$delaunay,{
  output$d3 <- renderD3({
    
    
    # grad_values = hvt.results.d3()[[3]]$summary[,5]
    grad_values = 
      hvt.results()[[3]]$summary %>% 
      dplyr::filter(`Segment.Level` == 1 & !is.na(Quant.Error)) %>% 
      .$Quant.Error
    df_points <-
      map_df(hvt.results()[[2]][[1]][[1]],~data.frame(x = .x$pt["x"], y = .x$pt["y"])) %>%
      cbind(grad_values = grad_values)
    
    # output$d3 <- renderD3({
    r2d3(data = df_points,d3_version = 4, script = "voronoi.js",
         options = list(x_min = min(df_points$x),y_min = min(df_points$y),
                        x_max = max(df_points$x),y_max = max(df_points$y),
                        grad_min = min(df_points$grad_values,na.rm = T),grad_max = max(df_points$grad_values, na.rm = T),
                        chart_type = input$delaunay))
    
    # }, warning = function(w) {
    #   # warning-handler-code
    # }, error = function(e) {
    #   # error-handler-code
    # }, finally = {
    #   # cleanup-code
    # }
    
    # )
  })
  # })
}

# Binding the app with UI and server components
shinyApp(ui, server)
