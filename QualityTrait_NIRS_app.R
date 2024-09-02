library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(icardaFIGSr)
library(DT)
library(ggplot2)
library(dplyr)
library(mdatools)
library(caret)

base::source("/Volumes/Macintosh HD — Data/Desktop/FIGS/nir_api.R")


ui <- dashboardPage(
  # # Custom CSS for notifications in the bottom-left corner
  # tags$head(
  #   tags$style(HTML("
  #     .shiny-notification {
  #       position: fixed;
  #       bottom: 20px;
  #       left: 20px;
  #       width: 300px;
  #     }
  #   "))
  # ),
  
  dashboardHeader(title = "Traits NIRSpectra Analytics", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Quality", tabName = "dataQuality", icon = icon("dashboard")),
      menuItem("Preprocessing", tabName = "preprocessing", icon = icon("cogs")),
      menuItem("Multivariate Analysis", tabName = "dataAnalysis", icon = icon("chart-bar")),
      menuItem("Modeling and Evaluation", tabName = "modeling", icon = icon("th")),
      menuItem("Make Predictions", tabName = "makePrediction", icon = icon("rocket"))
    )
  ),
  
  dashboardBody(
    
  # Data quality Tab
    tabItems(
      tabItem(tabName = "dataQuality",
              # Data extraction parameters
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
                    column(3, actionButton("fetchData", "Fetch Data", class = "btn-primary", 
                                           icon = icon("download")))
                  )
              ),
              # Data quality metrics and column wise statistics for NIRS
              box(title = "NIRS Data", status = "success", solidHeader = TRUE,
                  width = 12, collapsible = T,
                  fluidRow(
                         box(title = "Data Quality Metrics", DTOutput("dataQualityTableNIR"), 
                             width = 12, status = "success", collapsible = T), 
                         box(title="Column wise stats", DTOutput("nirDataColumnStatsTable"), 
                             width = 12, status = "success", collapsible = T)
                  )
              ),
              # Data quality metrics and column wise statistics for Trait
              box(title = "Quality traits Data", status = "success", solidHeader = TRUE,
                  width = 12, collapsible = T,
                  fluidRow(
                         box(title = "Data Quality Metrics", DTOutput("dataQualityTableTrait"), 
                             width = 12, status = "success", collapsible = T),
                         box(title = "Column wise stats", DTOutput("traitDataColumnStatsTable"), 
                             width = 12, status = "success", collapsible = T)
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
                                         choices = c("SNV",# = "Standard Normal Variate",
                                                     "MSC",# = "Multiplicative Scatter Correction",
                                                     "SVG",# = "Savitski-golay smoothing",
                                                     "SVG 1stD",# = "Savitski-golay smoothing and 1st derivative",
                                                     "SVG 2nD" ,#= "Savitski-golay smoothing and 2nd derivative",
                                                     "Length_Normalization",
                                                     "Area_Normalization"),
                                         selected = "SNV")
                    ),
                    column(4,actionButton("runPreprocessing", "Run Preprocessing",
                                          class = "btn-primary", 
                                          icon=icon("tools"))
                    )
                  )
              ),
              
              fluidRow(
                box(title = "Original Data", plotOutput("originalDataPlot"), width = 6, 
                    status = "success", solidHeader=T, collapsible=T),
                box(title = "Preprocessed Data", plotOutput("preprocessedPlots"), width = 6, 
                    status = "success", solidHeader=T, collapsible=T)
              )
      ),
      
      # Data Analysis Tab
      tabItem(tabName = "dataAnalysis",
              ## Data parameters inputs
              box(title = "Data analysis prameters", status = "primary", solidHeader = T, 
                  width= 12 , collapsible = T, 
                  fluidRow(
                    column(4,selectInput("multivariateAnalysis", "Select Multivariate Analysis Method",
                                         choices = c("PCA", "SOM", "SNE", "UMAP"), selected = "PCA")), 
                    column(4, uiOutput("traittoplot")),
                    column(4, actionButton("runAnalysis", "Run Analysis",
                                           class = "btn-primary", 
                                           icon=icon("play")))
                  )
              ),
              
              # Create trait classes from numeric values
              box(title = "Custom classification parameters", status = "primary", solidHeader = T, 
                  width = 12, collapsible = T,
                  # Class creation parameters
                  fluidRow(
                    column(3, sliderInput("numClasses", "Number of Classes",
                                          value = 2, min = 2, max = 5, step=1)),
                    column(6,uiOutput("classInputsUI")), 
                    column(3, actionButton("createClasses", "Create Classes",
                                           class = "btn-primary", 
                                           icon=icon("plus-square")))
                  )
              ), 
              
              # Show analysis results   
              box(title = "Analysis Results", status = "success", solidHeader = T,
                  width = 12, collapsible = T,
                  fluidRow(
                         # Show PCA results
                         box(title = "Scores" ,plotOutput("Scores"), 
                             width = 6, status = "success", collapsible = T), 
                         box(title = "Cumulative Variance", plotOutput("CumulVariance"), 
                                  width = 6, status = "success", collapsible = T),
                         box(title = "Extremes", plotOutput("Extremes"), 
                             width = 6, status = "success", collapsible = T),
                         box(title = "Residuals", plotOutput("Residuals"), 
                             width = 6, status = "success", collapsible = T),
                         
                         # Show Traits density and metadata
                         box(title = "Density Plot" ,
                             width = 12,status = "success", collapsible = T,
                                  fluidRow(
                                    column(6, plotOutput("traitDensityPlot")),
                                    column(6, plotOutput("DensityPlot")))),
                         
                         box(title="Trait and classes", DTOutput("TraitClasses"), 
                             width = 6, status = "success", collapsible = T),
                         box(title="Traits Meta data" ,DTOutput("TraitMetaData"), 
                                  width = 6, status = "success", collapsible = T)
                  )
              )
      ),
      
      # Modeling/evaluation Tab
      tabItem(tabName = "modeling",
              # Model parameters inputs
              box(title = "Modeling prameters", status = "primary", solidHeader = T, 
                  width= 12 , collapsible = T, 
                  fluidRow(
                    # Task Type Selection
                    column(3,radioButtons("taskType", "Select Task Type",
                                          choices = list("Other" = "other",
                                                         "Classification" = "classification",
                                                         "Regression" = "regression"))),
                    # Dynamic UI for Model Selection based on Task Type
                    column(3,uiOutput("modelSelection")),
                    # Dynamic UI for specifying intervals length for IPLS
                    column(3,uiOutput("intervalLength")),
                    # Button to Run Modeling Task
                    column(3,actionButton("runModel", "Run Modeling Task",
                                          icon = icon("rocket"),
                                          class = "btn-primary"))
                  )
              ),
              
              # Output Containers for Model Summary and Plots
              box(title = "Model summaries",status = "success",solidHeader = TRUE,
                  collapsible = TRUE,width = 12,
                  # Model summary outputs
                  fluidRow(
                          box(title = "Model Summary", verbatimTextOutput("modelSummaryUI"),
                              width = 12, status = "success", collapsible = T, solidHeader = T),
                         box(title = "Model diagnosis", plotOutput("modelPlots"), 
                             width = 12, status = "success", collapsible = T, solidHeader = T), 
                         box(title = "Coeffecients", plotOutput("modelCoefPlot"), 
                             width = 12, status = "success", collapsible = T, solidHeader = T), 
                         box(title = "Performance Metrics", plotOutput("modelPerfMetrPlot"), 
                             width = 12, status = "success", collapsible = T, solidHeader = T)
                          ),
                  fluidRow(
                    box(title = "SIMCA Outputs", width = 12, collapsible = T, solidHeader = T, 
                        status = "success",
                             box(title = "Specificity", width = 6,solidHeader = T, collapsible = T,
                                 plotOutput("simcaSpecificityPlot")), 
                             box(title = "Sensitivity", width = 6,solidHeader = T, collapsible = T,
                                 plotOutput("simcaSensitivityPlot")), 
                             
                             box(title = "Misclassification", width = 6,solidHeader = T, collapsible = T,
                                 plotOutput("simcaMisclassifiedPlot")), 
                             box(title = "Predictions", width = 6,solidHeader = T, collapsible = T,
                                 plotOutput("simcaPredictionsPlot"))
                         )
                    
                  )
              )
      ),
      
      ## Making predictions Tab
      tabItem(tabName = "makePrediction",
              # Prediction Parameters Inputs
              box(title = "Prediction Parameters", status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE, 
                  fluidRow(
                    column(3, selectInput("predictionCountry", "Country", choices = c("Morocco", "Tunisia", "Lebanon", "Mexico"), 
                                          multiple = T, selectize = T)),
                    column(3, selectInput("predictionLocation", "Location", choices = c("-","Annoceur","Beja","Beni-Mellal","Chebika",
                                                                                        "Merchouch","Oued Mliz","Sidi el Aïdi","Tassaout","Terbol"),
                                          multiple = TRUE, selected = NULL)),
                    column(3, selectInput("predictionCrop", "Crop", choices = c("Barley","Bread Wheat", "Chickpea","Lentil","Faba Bean","Durum Wheat"),
                                          selected = NULL, multiple = T)),
                    column(3, sliderInput("predictionYear", "Year", min = 2010, max = 2024, value =c(2016,2024))),
                    
                    actionButton("Checkdata_topredict", "Check data to predict")
                  )
              ),
              # Data to predict
              box(title = "MetaData of trait to predict", status = "primary", solidHeader = T, 
                  width = 12, collapsible = T, 
                  fluidRow(
                    box(title = "Trait Data", DTOutput("TraitMetaData_topredict"),
                        status = "success", solidHeader = F, 
                        width = 6, collapsible = T),
                    
                    box(title = "Spectral Data", DTOutput("NirsPredictor"), 
                        status = "success", solidHeader = F, 
                        width = 6, collapsible = T)
                  )
              ),
              
              # Button to Run Prediction Task
              box(width = 12, solidHeader = TRUE, status = "primary",
                  actionButton("runPrediction", "Run Prediction", icon = icon("rocket"), class = "btn-success")
              ),
              
              # Prediction Results
              box(title = "Prediction Results", status = "success",
                  solidHeader = TRUE, collapsible = TRUE, width = 12,
                  # Show prediction results
                  DTOutput("predictionTable")
              ),
              
              # Visualization
              box(title = "Prediction Visualizations", status = "success",
                  solidHeader = TRUE, collapsible = TRUE, width = 12,
                  fluidRow(
                    column(6, plotOutput("predictionDensityPlot")),
                    column(6, plotOutput("SpectralPlot"))
                  )
              ),
              
              # Download Predictions
              box(width = 12, solidHeader = TRUE, status = "success", collapsible = TRUE,
                  # Download Predictions Button
                  downloadButton("downloadPredictions", "Download Predictions", class = "btn-success")
              )
      )
    )
  )
)




server <- function(input, output, session) {
  
  
  ## Data quality parameters 
  
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
      sliderInput("year", "Year", min = 2010, max = 2023 , value =c(2017,2019))
    })
    
    # Country selection based on NIR data
    output$countrySelect <- renderUI({
      selectInput("country", "Country", choices = c("Morocco", "Tunisia", "Lebanon", "Mexico"), 
                  multiple = T, selectize = T)
    })
    
    # Site selection based on NIR data
    output$siteSelect <- renderUI({
      selectInput("location", "Location", choices = c("-","Annoceur","Beja","Beni-Mellal","Chebika",
                                                      "Ciudad Obregón","Douyet","El Kef", "Jemâa-Shaim","Melk Zher" ,
                                                      "Merchouch","Oued Mliz","Sidi el Aïdi","Tassaout","Terbol"),
                  multiple = TRUE, selected = NULL)
    })
  })
  
  
  ## Data Fetching Logic
  
  # Initialize NIR and Trait datasets with reactiveVal
  nirData <- reactiveVal()
  traitData <- reactiveVal()
  
  # Fetching NIR Data
  observeEvent(input$fetchData, {
    req(input$qualityLab)  # Ensure that a quality lab is selected
    withProgress(message = 'Fetching NIR Data...', value = 0, {
      for (i in 1:15) {
        incProgress(1/15)
        Sys.sleep(0.1)  # Simulated delay for fetching data
      }
      fetchedNirData <- getNIRData(qualityLab = input$qualityLab, crop = input$crop, nir_model = input$nirModel, 
                                   trial = input$trial, year = input$year, location = input$site)
      nirData <- nirData(fetchedNirData)  # Update the reactive value
    })
  })
  
  # Fetching Trait Data
  observeEvent(input$fetchData, {
    req(input$qualityLab)
    withProgress(message = 'Fetching Trait Data...', value = 0, {
      for (i in 1:15) {
        incProgress(1/15)
        Sys.sleep(0.1)
      }
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
      computeDataQuality(data)
    }
  })
  
  # Compute quality metrics TraitsData 
  qualityMetricsTrait <- reactive({
    data <- traitData()  # Access the current value of traitData
    if(is.null(data) || nrow(data) == 0) {
      list(TotalRows = NA, TotalColumns = NA, MissingValues = NA, CompleteRows = NA)
    } else {
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
  
  
  # NIR Data Quality Metrics
  output$dataQualityTableNIR <- renderDataTable({
    req(nirData())  
    datatable(computeDataQuality(nirData()[,-c(1:17)]), 
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
              ))
  })
  
  # Trait Data Quality Metrics
  output$dataQualityTableTrait <- renderDataTable({
    req(traitData())  # Ensure traitData is available before proceeding
    datatable(computeDataQuality(traitData()[,-c(1:17)]), 
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
              ))
  })
  
  # Function to compute column stats
  computeColumnStats <- function(data, digits = 2) {
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
    req(nirData())  
    computeColumnStats(nirData())
  })
  
  # Output for NIR Data Column Stats
  output$nirDataColumnStatsTable <- renderDataTable({
    datatable(
      nirDataColumnStats(), 
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ))
  })
  
  # Reactive expression for Trait Data
  traitDataColumnStats <- reactive({
    req(traitData())  # Ensure traitData is available
    computeColumnStats(traitData())
  })
  
  # Output for trait Data Column Stats
  output$traitDataColumnStatsTable <- renderDataTable({
    datatable(
      traitDataColumnStats(), 
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ))
  })
  
  
  ## Ouput traits stats
  output$traitStats <- renderTable({
    req(input$fetchData) 
    qualityMetrics <- computeDataQuality(traitData())
    
    # Check if the TraitStats data frame is not empty
    if(nrow(qualityMetrics$TraitStats) > 0) {
      datatable(qualityMetrics$TraitStats, 
                extensions = 'Buttons',
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ))
    } else {
      "No trait statistics available"
    }
  })
  
  
  ### Preprocessing
  
  
  # Reactive to store preprocessed data
  preprocessedData <- reactiveVal()
  
  # Selected Trait UI
  output$Selectedtrait <- renderUI({
    selectInput("trait", "Trait NIRS to preprocess",choices =colnames(traitData())[18:46],
                selected = "Protein")
  })
  
  # Update to handle a single preprocessing method selection and apply it
  observeEvent(input$runPreprocessing, {
    req(nirData(), traitData(), input$preprocessingMethod)
    
    withProgress(message = 'Preparing Plots...', value = 0, {
      for (k in 1:10) {
        incProgress(1/10)
        Sys.sleep(0.1)  # Simulated delay for fetching data
      }
      
    # Combine nirData and traitData
    Train_test_data <- nirData() %>%
      left_join(traitData(), by = "QualityLabPlotNumber")
    
    Train_test_data <- Train_test_data %>%
      filter(!is.na(.[[input$trait]])) %>%
      select(all_of(input$trait), grep("^[0-9]+$", names(.), value = TRUE))  # Select trait and wavelength columns
    
    # Initialize dataList with original data for reference
    dataList <- list("Original" = Train_test_data[,-1] )
    
    # Apply the selected preprocessing method and update dataList
    dataList[[input$preprocessingMethod]] <- switch(input$preprocessingMethod,
                                                    "SNV" = prep.snv(as.matrix(Train_test_data[,-1])),
                                                    "MSC" = prep.msc(as.matrix(Train_test_data[,-1])),
                                                    "SVG" = prep.savgol(Train_test_data[,-1], width = 15, porder =3, dorder = 0),
                                                    "SVG 1stD" = prep.savgol(Train_test_data[,-1], width = 15, porder = 3, dorder = 1),
                                                    "SVG 2nD" = prep.savgol(Train_test_data[,-1], width = 15, porder = 3, dorder = 2),
                                                    "Area_Normalization" = prep.norm(Train_test_data[,-1], "area"),
                                                    "Length_Normalization" = prep.norm(Train_test_data[,-1], "length"),
                                                    default = Train_test_data[,-1]
    )
    # Store the processed data
    preprocessedData(dataList)
    })
  })
  
  # Plot for Original Data
  output$originalDataPlot <- renderPlot({
    req(preprocessedData()["Original"])
    
    originalData <- preprocessedData()[["Original"]]
    
    # Set data attributes for plotting
    attr(originalData, "name") = "Wavelengths (nm)"
    
    attr(originalData, "xaxis.name") = "Wavelengths (nm)"
    attr(originalData, "xaxis.values") = colnames(originalData)%>%as.numeric()
    
    mdaplot(originalData, type = "l",
            main = "Original NIR Data")
  })
  
  # Plot Preprocessed NIR Data
  output$preprocessedPlots <- renderPlot({
    req(preprocessedData())
    
    # Ensure preprocessing method is selected and preprocessing has been run
    if (!is.null(input$preprocessingMethod) && input$runPreprocessing > 0) {
      processed <- preprocessedData()[[input$preprocessingMethod]]
      
      # Check if the processed data exists and is not null
      if (!is.null(processed) && ncol(processed) > 0) {
        # Set data attributes for plotting
        attr(processed, "xaxis.name") = "Wavelengths (nm)"
        attr(processed, "xaxis.values") = colnames(processed)%>%as.numeric()
        
        mdaplot(processed, type = "l", 
                main = paste("Preprocessed Data - Method:", input$preprocessingMethod))
      } else {
        # If processed data is not available, attempt to plot the original data
        originalData <- preprocessedData()[["Original"]]
        if (!is.null(originalData) && ncol(originalData) > 0) {
          mdaplot(originalData, type = "l", 
                  main = "Original NIR Data")
        }
      }
    } else {
      # Plot the original data if preprocessing method is not selected or preprocessing hasn't been run
      originalData <- preprocessedData()[["Original"]]
      if (!is.null(originalData) && ncol(originalData) > 0) {
        mdaplot(originalData, type = "l", main = "Original NIR Data")
      } else {
        # Return an empty plot or a message indicating no data is available
        plot.new()
        text(0.5, 0.5, "No data available for plotting", cex = 1.2)
      }
    }
  })
  
  
  
  ### Data Analysis
  
  # Trait selection UI 
  output$traittoplot <- renderUI({
    req(traitData())
    selectInput("traittoplot", "Trait to analyze", choices = colnames(traitData())[18:46], selected = "Protein")
  })
  
  # Define analysisResults as a reactive value to store PCA model results
  analysisResults <- reactiveVal(NULL)
  
  # analysisResults stores the PCA model and trainIndex for reference
  observeEvent(input$runAnalysis, {
    req(input$multivariateAnalysis, input$traittoplot,traitData(), nirData())
    
    withProgress(message = paste0('Fitting',input$multivariateAnalysis,'...'),
                 value = 0, {
      for (k in 1:5) {
        incProgress(1/5)
        Sys.sleep(0.1)  # Simulated delay for fetching data
      }
      
      # Combine preprocessed NIRS and Trait data
      combinedData <-  nirData() %>%
        left_join(traitData(), by = "QualityLabPlotNumber")%>%
        filter(!is.na(.[[input$traittoplot]])) %>%
        select(all_of(input$traittoplot), grep("^[0-9]+$", names(.), value = TRUE))  # Select trait and wavelength columns
      
      # Data partitioning
      set.seed(123) # For reproducibility
      trainIndex <- sample(nrow(combinedData), nrow(combinedData) * 0.75)
      
      Xc <- combinedData[trainIndex, -1] # Calibration predictors
      yc <- combinedData[trainIndex, 1]  # Calibration response
      
      Xt <- combinedData[-trainIndex, -1] # Test predictors
      yt <- combinedData[-trainIndex, 1]  # Test response
      
      # Fit PCA model @ x.test = Xt,
      modelPCA <- mdatools::pca(Xc, x.test = Xt, scale = T, center=T, ncomp = 10) # Can add method c("SVD","nipals")
      
      # Store PCA model, trainIndex, and traitData for plotting
      analysisResults(list(model = modelPCA, trainIndex = trainIndex,
                           Xcalib = Xc, Ycalib=yc, CombinedData=combinedData, 
                           traitData = combinedData[ ,1], NIRData = combinedData[,-1]))
      
    })
    
  })
  
  # PCA Scores with Selected Trait
  output$Scores <- renderPlot({
    req(analysisResults(), input$traittoplot)
    
    results <- analysisResults() 
    
    traitValuesForCalibration <- results$traitData[results$trainIndex,input$traittoplot]
  
    plotScores(results$model, show.labels = FALSE,
               main = paste("PCA Scores Colored by", input$traittoplot))
  })
  
  # Plot for PCA Extremes
  output$Extremes <- renderPlot({
    req(analysisResults())
    
    results <- analysisResults()
    if (!is.null(results$model) && input$multivariateAnalysis == "PCA") {

      plotExtreme(results$model, comp = 2:3, res =results$model$res$test, main = "Extreme plot (test)")
    }
  })
  
  output$Residuals <- renderPlot({
    req(analysisResults())
    
    results <- analysisResults()
    if (!is.null(results$model) && input$multivariateAnalysis == "PCA") {
      plotResiduals(results$model, ncomp=5,  main = paste("PCA Residuals of", input$traittoplot))
    }
  })
  # Plot for Explained Cumulative Variance 
  output$CumulVariance <- renderPlot({
    req(analysisResults())
    
    results <- analysisResults()
    if (!is.null(results$model) && input$multivariateAnalysis == "PCA") {
      plotVariance(results$model, type="h", show.labels = TRUE,  main = "Cumulative Variance Explained")
    }
  })
  
  # Density plot of selected trait
  output$traitDensityPlot <- renderPlot({
    
    req(traitData(), input$traittoplot, input$runAnalysis)
    dataToPlot <- data.frame(Value = traitData()[[input$traittoplot]])
    p1 <- ggplot(dataToPlot, aes(x = Value)) +
      geom_density()+
      theme_classic()+
      labs(title = paste("Density Plot by Class for", input$traittoplot),
           x = input$traittoplot, y = "Density")
    
    p1
  })
  
  ## Meta data table
  output$TraitMetaData <- renderDataTable({
    req(traitData(), input$traittoplot)
    DT::datatable(traitData(), 
                  extensions = 'Buttons',
                  options = list(
                    scrollX = TRUE, pageLength = 5,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                  ))
  })
  
  # Create classes from the selected trait
  output$classInputsUI <- renderUI({
    req(traitData(),input$traittoplot)
    
    # Make sure traitData is available
    numClasses <- input$numClasses
    selectedTrait <- input$traittoplot 
    
    # Calculate min and max based on the selected trait
    traitMin <- min(traitData()[[input$traittoplot]], na.rm = TRUE)
    traitMax <- max(traitData()[[input$traittoplot]], na.rm = TRUE)
    
    fluidRow(
      lapply(1:numClasses, function(i) {
        column(6,
               sliderInput(inputId = paste0("classLimit", i),
                           label = paste("Limits for Class", i),
                           min = floor(traitMin)-1, max = round(traitMax, 4),
                           value = round(quantile(traitData()[[input$traittoplot]],
                                                  probs = c((i-1)/numClasses, i/numClasses),
                                                  na.rm = TRUE, names = FALSE), 2))
        )
      }),
      lapply(1:numClasses, function(i) {
        column(6,
               textInput(inputId = paste0("className", i),
                         label = paste("Name for Class", i),
                         value = paste("Class", i))
        )
      })
    )
  })
  
  classData <- reactiveVal(NULL)
  
  ## Create classes from numeric values
  observeEvent(input$createClasses, {
    req(traitData(), input$traittoplot)

    # Access the selected trait data from traitData
    selectedTraitData <- traitData()[[input$traittoplot]]
    # Create a data frame for plotting, dynamically naming the trait column
    dataForPlotting <- data.frame(TraitValue = selectedTraitData, Class = NA_character_)
    
    # Apply class limits and names
    for(i in 1:input$numClasses) {
      classLimits <- input[[paste0("classLimit", i)]]
      className <- input[[paste0("className", i)]]
      
      withinLimits <- dataForPlotting$TraitValue >= classLimits[1] & dataForPlotting$TraitValue <= classLimits[2]
      dataForPlotting$Class[withinLimits] <- className
      dataForPlotting <- dataForPlotting%>% filter(!is.na(TraitValue))
    }
    
    # Store the modified data for further processing
    classData(dataForPlotting)
    
    # Plotting data density with classes
    output$DensityPlot <- renderPlot({
      req(classData()) 
      
      cleanDataForPlotting <- classData() 
      
      # Generate the plot
      p <- ggplot(cleanDataForPlotting, aes(x = TraitValue, group=Class, fill = Class)) +
        geom_density(alpha = 0.5) +
        scale_fill_manual(values = rainbow(length(unique(cleanDataForPlotting$Class)))) +
        labs(title = paste("Density Plot by Class for", input$traittoplot), x = input$traittoplot, y = "Density") +
        theme_classic()
      
      p 
    })
    
    showNotification(type = "message",
                     "Classes were succesfully created. Check Density outputs.")
  })
  
  ## Show trait values and classes
  output$TraitClasses <- renderDataTable({
    req(classData())
    datatable(classData(),
              extensions = 'Buttons',
              options = list(
                scrollX = TRUE, pageLength = 5,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
              ))
  })
  
  
  ### Modeling and evaluation
  
  
  # Render UI for model selection
  output$modelSelection <- renderUI({
    req(input$runPreprocessing, classData(), input$runAnalysis)
    if(input$taskType == "regression") {
      selectInput("modelType", "Select Model", choices = c("PLS", "IPLS"))
    } else if(input$taskType == "classification") {
      selectInput("modelType", "Select Model", choices = c("PLS-DA", "SIMCA"))
    } else {
      selectInput("modelType", "Model selection", choices = c("to be implemented"))
    }
  })
  
  
  # UI for specifying interval length in IPLS (only relevant for IPLS)
  output$intervalLength <- renderUI({
    if(input$modelType == "IPLS") {
      sliderInput("intervalLength", "Interval Length", value = 15, min = 1, max = 100, step = 2)
    } else {
      NULL
    }
  })
  
  
  ## Function to split and preprocess data for classification tasks
  prepare_sets <- function(data, class_column, calib_ratio = 0.5) {
    
    # Get the unique classes
    classes <- unique(data[[class_column]])
    
    # Initialize indices for calibration and validation sets
    cal_indices <- c()
    val_indices <- c()
    
    # Loop through each class
    for (cls in classes) {
      # Get the indices for the current class
      class_indices <- which(data[[class_column]] == cls)
      
      # Shuffle the indices to ensure random sampling
      class_indices <- sample(class_indices)
      
      # Split the indices into calibration and validation sets
      n_cal <- floor(calib_ratio * length(class_indices))
      cal_indices <- c(cal_indices, class_indices[1:n_cal])
      val_indices <- c(val_indices, class_indices[(n_cal + 1):length(class_indices)])
    }
    
    # Create the calibration and validation sets
    Xc <- data[cal_indices, -which(names(data) == class_column)]
    Xv <- data[val_indices, -which(names(data) == class_column)]
    
    cc_all <- data[cal_indices, class_column]
    cv_all <- data[val_indices, class_column]
    
    list(
      Xc = Xc,
      Xv = Xv,
      cc_all = cc_all,
      cv_all = cv_all,
      cal_indices = cal_indices,
      val_indices = val_indices
    )
  }
  
  
  # Reactive storage for model results and click flag
  ModelResult <- reactiveVal(NULL)
  runModel_clicked <- reactiveVal(FALSE)
  
  # Observe runModel button actions
  observeEvent(input$runModel, {
    req(input$modelType, preprocessedData(), input$taskType, classData())
    
    # Reset ModelResult and runModel_clicked each time the model is run
    ModelResult(NULL)
    runModel_clicked(FALSE)  # Reset the flag before running the model
    
    withProgress(message = paste0('Training Model ', input$modelType, '...'), value = 0, {
      for (k in 1:5) {
        incProgress(1/5)
      }
      
      responseColumn <- if (input$taskType == "regression") "TraitValue" else "Class"
      combinedData <- cbind(classData()[, responseColumn, drop = FALSE], preprocessedData())
      combinedData <- combinedData[complete.cases(combinedData), ]
      
      if (input$taskType == "regression") {
        set.seed(123)
        trainIndex <- createDataPartition(combinedData[[responseColumn]], p = 0.75, list = FALSE)
        trainData <- combinedData[trainIndex, ]
        testData <- combinedData[-trainIndex, ]
        
        fittedModel <- switch(input$modelType,
                              "PLS" = pls(trainData[, -1], trainData[[responseColumn]], 
                                          x.test = testData[, -1], y.test = testData[[responseColumn]]),
                              "IPLS" = ipls(trainData[, -1], trainData[[responseColumn]], 
                                            x.test = testData[, -1], y.test = testData[[responseColumn]], 
                                            int.num = if_else(is.null(input$intervalLength),15,input$intervalLength)))
                              
        ModelResult(list(Model = fittedModel, TestData = testData, TrainData = trainData, ResponseColumn = responseColumn))
        
      } else if (input$taskType == "classification") {
        
        # Ensure the Class column is a factor
        combinedData[[responseColumn]] <- as.factor(combinedData[[responseColumn]])
        
        # Split the data for PLS-DA
        split_data <- prepare_sets(combinedData, class_column = responseColumn, calib_ratio = 0.7)
        
        Xc <- split_data$Xc
        Xv <- split_data$Xv
        cc_all <- split_data$cc_all
        cv_all <- split_data$cv_all
        
        # Create PLS-DA model
        fittedModel <- switch(input$modelType,
                              "PLS-DA" = mdatools::plsda(Xc, cc_all, ncomp = 10, cv = 3)
        )
        
        ModelResult(list(Model = fittedModel, TestData = Xv, TrainData = Xc, ResponseColumn = responseColumn))
      } else {
        
        ModelResult(NULL)
        
      }
    })
    
    # Set the flag to TRUE after the model is trained
    runModel_clicked(TRUE)
  })
  
  # UI for Model Summary, updated to include iPLS
  output$modelSummaryUI <- renderPrint({
    req(input$taskType, input$modelType, runModel_clicked())  
    model <- isolate(ModelResult()$Model)
    
    if (is.null(model)) {
      print("No model has been trained yet.")
      return()
    }
    
    if (input$modelType == "PLS") {
      print(summary(model))
    } else if (input$modelType == "PLS-DA") {
      ncomp <- ifelse(!is.null(model$ncomp.selected) && model$ncomp.selected > 0, model$ncomp.selected, 3)
      if (is.null(ncomp) || ncomp > model$ncomp || ncomp <= 0) {
        ncomp <- model$ncomp  # Fallback to the maximum available components
      }
      print(summary(model, ncomp = ncomp))
    } else if (input$modelType == "IPLS") {
      print(summary(model))

    } else {
      print("Model summary is not available for the selected model type.")
    }
  })
  
  # Model Plots
  output$modelPlots <- renderPlot({
    req(input$taskType, input$modelType, runModel_clicked())  
    model <- isolate(ModelResult()$Model)
    
    if (is.null(model)) {
      plot.new()
      text(0.5, 0.5, "No model has been trained yet.", cex = 1.5)
      return()
    }
    
    if (input$modelType == "PLS") {
      plot(model, main = "PLS Model")
    } else if (input$modelType == "IPLS") {
      par(mfrow = c(2, 1))
      
      plot(model, main = "iPLS Model")
      plotRMSE(model, main = "RMSE Development for iPLS")
      
    } else if (input$modelType == "PLS-DA") {
      par(mfrow = c(2, 1))
      plot(model, what = "scores", main = "PLS-DA Scores")
      plot(model, what = "predictions", main = "PLS-DA Predictions")
    } else {
      plot.new()
      text(0.5, 0.5, "No plot available for the selected model type", cex = 1.5)
    }
  })
  
  
  # Model Coefficient Plot
  output$modelCoefPlot <- renderPlot({
    req(input$taskType, input$modelType, runModel_clicked())  
    model <- isolate(ModelResult()$Model)
    
    if (is.null(model)) {
      plot.new()
      text(0.5, 0.5, "No model has been trained yet.", cex = 1.5)
      return()
    }
    
    if (input$modelType == "PLS") {
      plotRegcoeffs(model, main = "PLS Coefficients")
    } else if (input$modelType == "PLS-DA") {
      plotRegcoeffs(model, main = "PLS-DA Coefficients")
    } else if (input$modelType == "IPLS") {
      plot(model, main = "iPLS Model")
      plotRMSE(model, main = "RMSE Development for iPLS")
    } else {
      plot.new()
      text(0.5, 0.5, "No coefficient plot available for the selected model type", cex = 1.5)
    }
  })
  
  # Performance Metrics Plot
  output$modelPerfMetrPlot <- renderPlot({
    req(input$taskType, input$modelType, runModel_clicked())  
    model <- isolate(ModelResult()$Model)
    
    if (is.null(model)) {
      plot.new()
      text(0.5, 0.5, "No model has been trained yet.", cex = 1.5)
      return()
    }
    
    if (input$modelType == "PLS") {
      par(mfrow = c(2, 2))
      plotPredictions(model$res$cal, ncomp = model$ncomp.selected, show.labels = FALSE, main = "PLS Predictions", show.stat = TRUE)
      plotRMSE(model, ncomp = model$ncomp.selected, main = "PLS RMSE")
      plotXYScores(model, ncomp = model$ncomp.selected, main = "PLS Scores")
      plotXYResiduals(model, ncomp = model$ncomp.selected, main = "PLS Residuals")
    } else if (input$modelType == "PLS-DA") {
      par(mfrow = c(2, 2))
      plotPredictions(model, main = "PLS-DA Predictions")
      plotXYScores(model, main = "PLS-DA Scores")
      plotSpecificity(model, main = "PLS-DA Cooman's Plot")
      plotMisclassified(model, main = "PLS-DA Misclassified")
    } else if (input$modelType == "IPLS") {
      par(mfrow = c(2, 2))
      plotRMSE(model$gm, main = "Global Model RMSE")
      plotRMSE(model$om, main = "Optimized Model RMSE")
      
      plot(model, intervals = model$int.selected, main = "Selected Intervals in iPLS")
      
    } else {
      plot.new()
      text(0.5, 0.5, "No performance plots available for the selected model type", cex = 1.5)
    }
  })
  
  
  
  
  
  
  # Separate reactive plot outputs for each SIMCA metric
  output$simcaSpecificityPlot <- renderPlot({
   
    req(input$taskType, input$modelType, input$runModel) 
    model <- isolate(ModelResult()$Model)
    
    classname <- ModelResult()$Classname
    
    if (input$modelType == "SIMCA") {
      plotSpecificity(model, classname = classname, main = "SIMCA Specificity")
    } else {
      plot.new()
      text(0.5, 0.5, "Specificity plot is only available for SIMCA models", cex = 1.5)
    }
  })
  
  output$simcaSensitivityPlot <- renderPlot({
    req(input$taskType, input$modelType, input$runModel)  
    model <- isolate(ModelResult()$Model)
    
    classname <- ModelResult()$Classname
    
    if (input$modelType == "SIMCA") {
      plotSensitivity(model, classname = classname, main = "SIMCA Sensitivity")
    } else {
      plot.new()
      text(0.5, 0.5, "Sensitivity plot is only available for SIMCA models", cex = 1.5)
    }
  })
  
  output$simcaMisclassifiedPlot <- renderPlot({
    req(input$taskType, input$modelType, input$runModel)  
    model <- isolate(ModelResult()$Model)
    
    model <- ModelResult()$Model
    classname <- ModelResult()$Classname
    
    if (input$modelType == "SIMCA") {
      plotMisclassified(model, classname = classname, main = "SIMCA Misclassified")
    } else {
      plot.new()
      text(0.5, 0.5, "Misclassified plot is only available for SIMCA models", cex = 1.5)
    }
  })
  
  output$simcaPredictionsPlot <- renderPlot({
    req(ModelResult(), input$runModel, !is.null(classData()))
    model <- ModelResult()$Model
    classname <- ModelResult()$Classname
    
    if (input$modelType == "SIMCA") {
      plotPredictions(model, classname = classname, main = "SIMCA Predictions")
    } else {
      plot.new()
      text(0.5, 0.5, "Predictions plot is only available for SIMCA models", cex = 1.5)
    }
  })
  
  
  # Reactive values to store data and results
  predictedData <- reactiveVal(NULL)
  predictorsData <- reactiveVal(NULL)
  
  # Initialize NIR and Trait datasets with reactiveVal
  NIRData <- reactiveVal()
  TraitData <- reactiveVal()
  
  # Fetching NIR Data
  observeEvent(input$Checkdata_topredict, {
    req(input$qualityLab)  # Ensure that a quality lab is selected
    withProgress(message = 'Fetching NIR Data...', value = 0, {
      for (i in 1:15) {
        incProgress(1/15)
        Sys.sleep(0.1)  # Simulated delay for fetching data
      }
      fetchedNirData <- getNIRData(qualityLab = input$qualityLab, crop = input$predictionCrop, nir_model = input$nirModel, 
                                   trial = input$trial, year = input$PredictionYear, location = input$PredictionLocation)
      NIRData <- NIRData(fetchedNirData)  # Update the reactive value
    })
  })
  
  # Fetching Trait Data
  observeEvent(input$Checkdata_topredict, {
    req(input$qualityLab)
    withProgress(message = 'Fetching Trait Data...', value = 0, {
      for (i in 1:15) {
        incProgress(1/15)
        Sys.sleep(0.1)
      }
      fetchedTraitData <- getTraitsData(qualityLab = input$qualityLab, crop = input$predictionCrop, nir_model = input$nirModel, 
                                        trial = input$trial, year = input$PredictionYear, location = input$PredictionLocation)
      TraitData <- TraitData(fetchedTraitData)  # Update the reactive value
    })
  })
  
  
  # Data filtering and fetching based on user inputs
  observeEvent(input$Checkdata_topredict, {
    
    req(NIRData(), TraitData(), ModelResult())
    # Replace this with actual data fetching logic
    
    TraitNirdata <-  NIRData() %>%
      left_join(TraitData(), by = "QualityLabPlotNumber")%>%
      filter(is.na(.[[input$traittoplot]])) %>%
      select(all_of(input$traittoplot), grep("^[0-9]+$", names(.), value = TRUE))  # Select trait and wavelength columns
      
    # Store the filtered data
    predictorsData(TraitNirdata[,-1])
    
    TraitNirdata <-  TraitData() %>%
      left_join(NIRData(), by = "QualityLabPlotNumber")%>%
      filter(is.na(.[[input$traittoplot]])) %>%
      select(all_of(input$traittoplot), 1:15)  # Select trait and metadata
    
    predictedData(TraitNirdata)
    
    # Display the filtered trait data and NIR data
    output$TraitMetaData_topredict <- renderDT({
      datatable(predictedData(), options = list(pageLength = 5, scrollX=TRUE))
    })
    
    output$NirsPredictor <- renderDT({
      datatable(predictorsData(), options = list(pageLength = 5, scrollX=TRUE))
    })
  })
  
  # Run the prediction task 
  observeEvent(input$runPrediction, {
    req(predictorsData())
    
    # Apply preprocessing to the predictors based on input$PreprocessingMethod
    preprocessed_predictors <- prep.snv(predictorsData())
    
    # Use the trained model to make predictions
    predictions <- predict(ModelResult()$Model, preprocessed_predictors)
    
    # Combine the predictions with the metadata and predictors
    prediction_results <- cbind(predictorsData(), Prediction = predictions)
    
    # Store the predictions
    predictedData(prediction_results)
    
    # Display the predictions in a table
    output$predictionTable <- renderDT({
      datatable(prediction_results, options = list(pageLength = 5))
    })
  })
  
  
  # Visualization: Density plot comparing predicted trait vs training trait values
  # output$predictionDensityPlot <- renderPlot({
  #   req(predictedData())
  #   
  #  # training_data <- ModelResult()$TrainData
  #   
  #   # training_plot <- ggplot(training_data, aes(x = TraitValue, fill = "Training")) +
  #   #   geom_density(alpha = 0.5, color = "blue") +
  #   #   labs(title = "Density Plot: Training vs Predicted Trait Values",
  #   #        x = "Trait Value", y = "Density")
  #   # 
  #   prediction_plot <- ggplot(predictedData(), aes(x = Prediction, fill = "Prediction")) +
  #     geom_density(alpha = 0.5, color = "red")
  #   
  #   # combined_plot <- training_plot +
  #   #   prediction_plot +
  #   #   scale_fill_manual(values = c("Training" = "blue", "Prediction" = "red")) +
  #   #   theme_minimal()
  # #  par(mfrow=c(1,2))
  # #  training_plot
  #   prediction_plot
  # })
  # 
  # Visualization: Spectral data vs wavelength
  output$SpectralPlot <- renderPlot({
    req(predictedData())
    
    spectral_data <- predictorsData()
    wavelengths <- as.numeric(colnames(spectral_data))
    
    # Assuming the spectral data is in a matrix or data frame format with rows as samples
    mdaplot(spectral_data, type = "l",
            main = "Spectral Data vs Wavelengths",
            xaxis.name = "Wavelength (nm)",
            xaxis.values = wavelengths)
  })
  
  # Download predictions
  output$downloadPredictions <- downloadHandler(
    filename = function() {
      paste("Predictions_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(predictedData(), file)
    })
  
}


# Run the application
shinyApp(ui, server)

