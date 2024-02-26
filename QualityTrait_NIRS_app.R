library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(icardaFIGSr)
library(dplyr)
library(shinyWidgets)
library(shinyjs)
library(mdatools)


base::source("/Volumes/Macintosh HD — Data/Desktop/FIGS/icardaFIGSr/nir_api.R")


ui <- dashboardPage(
  dashboardHeader(title = "Data Analysis App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Quality", tabName = "dataQuality", icon = icon("dashboard")),
      menuItem("Preprocessing", tabName = "preprocessing", icon = icon("cogs")),
      menuItem("Data Analysis", tabName = "dataAnalysis", icon = icon("chart-bar")),
      menuItem("Modeling", tabName = "modeling", icon = icon("th")),
      menuItem("Model Evaluation", tabName = "modelEval", icon = icon("tachometer-alt")),
      menuItem("Make Predictions", tabName = "makePrediction", icon = icon("rocket"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "dataQuality",
              box(title = "Data parameters", status = "primary", solidHeader = TRUE,
                  width = 12,collapsible = T,
                  fluidRow(
                    column(3, selectInput("qualityLab", "Quality lab",
                                          choices = c("ICARDA-MAR", "ICARDA-LBN", "CIMMYT"), 
                                          multiple = T)),
                    column(3, uiOutput("cropSelect")), 
                    column(3, uiOutput("countrySelect")),
                    column(3, uiOutput("NirModelSelect"))
                  ),
                  fluidRow(
                    column(3, uiOutput("yearSelect")),
                    column(3, uiOutput("siteSelect")),
                    column(3, actionButton("fetchData", "Fetch Data", class = "btn-primary"))
                  )
                ),
              
              box(title = "NIRS Data", status = "success", solidHeader = TRUE,
                  width = 12, collapsible = T,
                  tabBox(width = 12,
                    tabPanel("Data Quality Metrics", DTOutput("dataQualityTableNIR")), 
                    tabPanel("Column wise stats", DTOutput("nirDataColumnStatsTable"))
                  )
              ),
              box(title = "Quality traits Data", status = "success", solidHeader = TRUE,
                  width = 12, collapsible = T,
                  tabBox(width = 12,
                    tabPanel("Data Quality Metrics", DTOutput("dataQualityTableTrait")), 
                    tabPanel("Column wise stats", DTOutput("traitDataColumnStatsTable"))
                  )

              )
      ),

      # Preprocessing Tab
      tabItem(tabName = "preprocessing",
            box(title = "preprocessing parameters", status = "primary", solidHeader = TRUE, 
                width = 12, collapsible = T,
                fluidRow(
                  column(4,uiOutput("Selectedtrait")),
                  column(4,selectInput("preprocessingMethod", "Select Preprocessing Method",
                                       choices = c("SNV"="Standard Normal Variate",
                                                   "MSC"="Multiplicative Scatter Correction",
                                                   "SVG" = "Savitski-golay smoothing",
                                                   "SVG 1stD" = "Savitski-golay smoothing and 1st derivative",
                                                   "SVG 2nD" = "Savitski-golay smoothing and 2nd derivative",
                                                   "scaling",
                                                   "center",
                                                   "scale_mean_center",
                                                   "Length_Normalization",
                                                   "Area_Normalization"), selected = "SNV")
                         ), 
                  column(4,actionButton("runPreprocessing", "Run Preprocessing", class = "btn-primary")
                  )
                )
              ),
              fluidRow(
                box(title = "Original Data", plotOutput("originalDataPlot"), width = 6),
                box(title = "Preprocessed Data", plotOutput("preprocessedPlots"), width = 6)
              )
      ),
      
      # Data Analysis Tab
      tabItem(tabName = "dataAnalysis",
              selectInput("multivariateAnalysis", "Select Multivariate Analysis Method",
                          choices = c("PCA", "SOM", "SNE", "UMAP"), selected = NULL),
              selectInput("traitsForPlotting", "Select Traits for Plotting", choices = NULL, multiple = TRUE),
              actionButton("runAnalysis", "Run Analysis"),
              plotOutput("analysisPlot"),
              actionButton("createClass", "Create Class from Trait"),
              DTOutput("newClassTable")
      ),
      
      # Modeling Tab
      tabItem(tabName = "modeling",
              numericInput("trainSize", "Training Set Size", value = 0.7),
              numericInput("testSize", "Testing Set Size", value = 0.3),
              actionButton("runModel", "Run Model"),
              DTOutput("modelSummary")
      ),
      
      # Model Evaluation Tab
      tabItem(tabName = "modelEval",
              fluidRow(
                valueBoxOutput("precision"),
                valueBoxOutput("accuracy"),
                valueBoxOutput("recall")
              ),
              box(title = "Model Predictions", plotOutput("predictionPlot")),
              box(title = "Model Evaluation Metrics", DTOutput("evaluationMetrics"))
      ), 
      
      # Make Predictions Tab
      tabItem(tabName = "makePrediction",
              selectInput("predictionCountry", "Country", choices = NULL),
              selectInput("predictionLocation", "Location", choices = NULL),
              selectInput("predictionCrop", "Crop", choices = NULL),
              sliderInput("predictionYear", "Year", min = 2000, max = 2023, value = 2022),
              actionButton("runPrediction", "Make Prediction"),
              downloadButton("downloadPredictions", "Download Predictions")
    )
  )
 )
)



server <- function(input, output, session) {
  
  # Dynamic UI Outputs
  observe({
    
    req(input$qualityLab)
    
    # Crop selection based on NIR data
    output$cropSelect <- renderUI({
      selectInput("crop", "Crop",
                  choices = c("Barley","Bread Wheat", "Chickpea","Lentil","Faba Bean","Durum Wheat"),
                  selected = NULL, multiple = T)
    })
    
    # NIR Model selection based on NIR data
    output$NirModelSelect <- renderUI({
      selectInput("nirModel", "NIR Model", choices = c('Antharis II','FOSS DS2500'), multiple = T)
    })
    
    # Year selection based on NIR data
    output$yearSelect <- renderUI({
      sliderInput("year", "Year", min = 2010, max = 2023 , value =  c(2017,2019))
    })
    
    # Country selection based on NIR data
    output$countrySelect <- renderUI({
      selectInput("country", "Country", choices = c("Morocco", "Tunisia", "Lebanon", "Mexico"), 
                  multiple = T, selectize = T)
    })
    
    
    # Site selection based on NIR data
    output$siteSelect <- renderUI({
      #choices <- unique(nirData()$location)  # Assuming 'location' field corresponds to site
      selectInput("location", "Location", choices = c("-","Annoceur","Beja","Beni-Mellal","Chebika",
                                                      "Ciudad Obregón","Douyet","El Kef", "Jemâa-Shaim","Melk Zher" ,
                                                      "Merchouch","Oued Mliz","Sidi el Aïdi","Tassaout","Terbol"),
                                                      multiple = TRUE, selected = NULL )#c("Merchouch", "Annoceur", "Beni-Mellal", "Sidi el Aïdi","Tassaout"))
    })
  
  })
  
  # Data Fetching Logic
  
  # Initialize NIR and Trait datasets with reactiveVal
  nirData <- reactiveVal()
  traitData <- reactiveVal()
  
  # Fetching NIR Data
  observeEvent(input$fetchData, {
    req(input$qualityLab)  # Ensure that a quality lab is selected
    withProgress(message = 'Fetching NIR Data...', value = 0, {
      for (i in 1:30) {
        incProgress(1/30)
        Sys.sleep(0.1)  # Simulated delay for fetching data
      }
      # Assume getNIRData returns a dataframe
      fetchedNirData <- getNIRData(qualityLab = input$qualityLab, crop = input$crop, nir_model = input$nirModel, 
                                   trial = input$trial, year = input$year, location = input$site)
      nirData <- nirData(fetchedNirData)  # Update the reactive value
    })
  })
  
  # Fetching Trait Data
  observeEvent(input$fetchData, {
    req(input$qualityLab)
    withProgress(message = 'Fetching Trait Data...', value = 0, {
      for (i in 1:30) {
        incProgress(1/30)
        Sys.sleep(0.1)
      }
      # Assume getTraitsData returns a dataframe
      fetchedTraitData <- getTraitsData(qualityLab = input$qualityLab, crop = input$crop, nir_model = input$nirModel, 
                                        trial = input$trial, year = input$year, location = input$site)
      traitData <- traitData(fetchedTraitData)  # Update the reactive value
    })
  })
  
  # Compute quality metrics NirData 
  qualityMetricsNir <- reactive({
    data <- nirData()  # Access the current value of nirData
    if(is.null(data) || nrow(data) == 0) {
      list(TotalRows = NA, TotalColumns = NA, MissingValues = NA, CompleteRows = NA)
    } else {
      # Compute quality metrics
      computeDataQuality(data)
    }
  })

  
  # Compute quality metrics TraitsData 
  qualityMetricsTrait <- reactive({
    data <- traitData()  # Access the current value of traitData
    if(is.null(data) || nrow(data) == 0) {
      list(TotalRows = NA, TotalColumns = NA, MissingValues = NA, CompleteRows = NA)
    } else {
      # Compute quality metrics
      computeDataQuality(data)
    }
  })
  
  ## Function to compute data quality
  computeDataQuality <- function(data) {
    if(is.null(data) || nrow(data) == 0) {
      return(data.frame(Metric = character(), Value = numeric()))  # Return an empty data frame if data is NULL or empty
    }
    
    totalRows <- nrow(data)
    totalColumns <- ncol(data)
    completeRows <- nrow(data) - sum(!complete.cases(data))
    completeColumns <- sum(colSums(is.na(data)) == 0)  # Count columns without any missing values
    missingValues <- sum(is.na(data))
    percentMissingData <- (missingValues / (totalRows * totalColumns)) * 100  # Calculate percentage of missing data
    
    # Prepare a data frame directly for output
    metricsDF <- data.frame(
      Metric = c("Total Rows", "Total Columns",  "Complete Rows", "Complete Columns", "Missing Values", "Percent Missing Data"),
      Value = c(totalRows, totalColumns, completeRows, completeColumns, missingValues, round(percentMissingData, 2)),
      stringsAsFactors = FALSE  # Avoid factor conversion
    )
    
    ## Transpose table
    metricsDF <- t(metricsDF)
    
    ## Assign columns
    colnames(metricsDF) <- metricsDF[1, ]  
    
    ## convert to dataframe
    metricsDF <- as.data.frame(metricsDF) 
    
    # Remove duplicate row (same as column)
    metricsDF <- metricsDF[-1,]
    
    return(metricsDF)
  }
  
  
  # Example for NIR Data Quality Metrics
  output$dataQualityTableNIR <- renderDataTable({
    req(nirData())  # Ensure nirData is available before proceeding
    computeDataQuality(nirData())
  })
  
  # Example for Trait Data Quality Metrics
  output$dataQualityTableTrait <- renderDataTable({
    req(traitData())  # Ensure traitData is available before proceeding
    computeDataQuality(traitData())
  })
  
  # function to compute column stats
  computeColumnStats <- function(data, digits = 2) {
    # Function to compute stats for a single column
    computeStats <- function(column) {
      if(is.numeric(column)) {
        return(c(
          Mean = round(mean(column, na.rm = TRUE), digits),
          Median = round(median(column, na.rm = TRUE), digits),
          Min = round(min(column, na.rm = TRUE), digits),
          Max = round(max(column, na.rm = TRUE), digits),
          NA_Count = sum(is.na(column)),
          Unique_Values = length(unique(column))
        ))
      } else if(is.character(column)) {
        return(c(
          Unique_Values = length(unique(column)),
          NA_Count = sum(is.na(column))
        ))
      } else {
        # For other types, only count missing and unique values
        return(c(
          Unique_Values = length(unique(column)),
          NA_Count = sum(is.na(column))
        ))
      }
    }
    
    # Apply the computeStats function to each column and combine the results
    statsList <- lapply(data, computeStats)
    statsDF <- do.call(rbind, statsList)
    rownames(statsDF) <- names(data)
    
    statsDF <- statsDF[-c(1:17),]
    return(statsDF)
    
  }
  
  # Reactive expression for NIR data column stats
  nirDataColumnStats <- reactive({
    req(nirData())  # Ensure nirData is available
    computeColumnStats(nirData())
  })
  
  # Output for NIR Data Column Stats
  output$nirDataColumnStatsTable <- renderDataTable({
    nirDataColumnStats()
  })
  
  # Reactive expression for Trait Data
  traitDataColumnStats <- reactive({
    req(traitData())  # Ensure traitData is available
    computeColumnStats(traitData())
  })
  
  # Output for trait Data Column Stats
  output$traitDataColumnStatsTable <- renderDataTable({
    traitDataColumnStats()
  })
  
  
  ## Ouput traits stats
  output$traitStats <- renderTable({
    req(input$fetchData) # Ensure the data is available
    qualityMetrics <- computeDataQuality(traitData())
    
    # Check if the TraitStats data frame is not empty
    if(nrow(qualityMetrics$TraitStats) > 0) {
      qualityMetrics$TraitStats
    } else {
      "No trait statistics available"
    }
  })
  
  
  # Preprocessing
  
  preprocessedData <- reactiveVal()
  
  ## Selected Trait
  output$Selectedtrait <- renderUI({
    selectInput("trait", "Trait to model",choices =colnames(traitData())[18:46], 
                selected = "Protein")
    })

  ## Selected Methods
  
  # Update to handle a single preprocessing method selection and apply it
  observeEvent(input$runPreprocessing, {
    req(nirData(), traitData(), input$preprocessingMethod)
    
    # Generate Train_test_data by combining nirData and traitData
    Train_test_data <- nirData() %>%
      left_join(traitData(), by = "QualityLabPlotNumber") %>%
      # Keep the trait and all numeric columns before filtering
      select(all_of(input$trait), everything()) %>%
      filter(!is.na(.[[input$trait]]))
    
    # Use an indexing vector to specifically select columns that are numeric and have numeric names,
    # which are assumed to represent wavelengths
    wavelength_columns <- grep("^[0-9]+$", names(Train_test_data), value = TRUE)
    
    # Ensure the trait column is included along with wavelength columns
    selectedColumns <- c(input$trait, wavelength_columns)
    Train_test_data <- Train_test_data %>%
      select(all_of(selectedColumns))
    
    # Store original data in preprocessedData for reference
    dataList <- list("Original" = Train_test_data)
    
    
    # Apply the selected preprocessing method
    dataList[[input$preprocessingMethod]] <- switch(input$preprocessingMethod,
                            "SNV" = prep.snv(Train_test_data[,-1]),
                            "MSC" = prep.msc(as.matrix(Train_test_data[,-1])),
                            "SVG" = prep.savgol(Train_test_data[,-1], width = 15, porder = 3),
                            "SVG 1stD" = prep.savgol(Train_test_data[,-1], width = 15, porder = 3, dorder = 1),
                            "SVG 2ndD" = prep.savgol(Train_test_data[,-1], width = 15, porder = 3, dorder = 2),
                          #  "scale" = prep.autoscale(Train_test_data[,-1], center = FALSE, scale = TRUE),
                          #  "center" = prep.autoscale(Train_test_data[,-1], center = TRUE, scale = FALSE),
                          #  "scale_mean_center" = prep.autoscale(Train_test_data[,-1], center = TRUE, scale = TRUE),
                          #  "Area_Normalization" = prep.norm(Train_test_data[,-1], "area"),
                          #  "Length_Normalization" = prep.norm(Train_test_data[,-1], "length"),
                          Train_test_data[,-1]
    )
    
    # Store the processed data, including the method name as key for reference
    preprocessedData(dataList)
  })
  
  # Plot for Original Data
  output$originalDataPlot <- renderPlot({
    req(preprocessedData()["Original"])
    
    # Assuming mdaplot can plot matrices or data frames directly
    originalData <- preprocessedData()[["Original"]]
    
    mdaplot(originalData[,-1], type = "l", main = "Original NIR Data")
  })
  
  # Plot for Preprocessed Data
  output$preprocessedPlots <- renderPlot({
    req(preprocessedData(), input$preprocessingMethod)
    
    # Access the preprocessed data using the method selected by the user
    processed <- preprocessedData()[[input$preprocessingMethod]]
    
    # Check if processed data exists and is not just the original data
    if(!is.null(processed) && input$preprocessingMethod != "Original") {
      mdaplot(processed, main = paste("Preprocessed Data - Method:", input$preprocessingMethod))
    } else {
      # Fallback to original data plot if no preprocessing method is applied
      originalData <- preprocessedData()[["Original"]]
      
      mdaplot(originalData[,-1], type = "l", main = "Original NIR Data")
    }
  })
  
  # Data Analysis
  output$analysisPlot <- renderPlot({
    req(input$analysisColumn)
    # Create plots based on selected column...
  })
  
  # Modeling
  observeEvent(input$runModel, {
    # Split data, train model, and display summary...
  })
  
  # Model Evaluation
  calcPrecision <- function(predictions, actual) {
    # Calculate precision based on predictions and actual values
  }
  # Similarly, define functions for calculating accuracy and recall
  
  output$precision <- renderValueBox({
    # Use calcPrecision to calculate and display the precision
  })
  # Similarly, implement value boxes for accuracy and recall
  
}

# Run the application
shinyApp(ui, server)




