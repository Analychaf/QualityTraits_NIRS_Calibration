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
  dashboardHeader(title = "TraitWave Analytics", titleWidth = 300),
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
                  tabBox(width = 12,
                         tabPanel("Data Quality Metrics", DTOutput("dataQualityTableNIR")), 
                         tabPanel("Column wise stats", DTOutput("nirDataColumnStatsTable"))
                  )
              ),
              # Data quality metrics and column wise statistics for Trait
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
              # 
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
                  tabBox(width = 12,
                         # Show PCA results
                         tabPanel("Scores" ,plotOutput("Scores")), 
                         tabPanel("Cumulative Variance", plotOutput("CumulVariance")),
                         tabPanel("Loadings", plotOutput("Loadings")),
                         # Show Traits density and metadata
                         tabPanel("Density Plot" ,
                                  fluidRow(
                                    column(6, plotOutput("traitDensityPlot")),
                                    column(6, plotOutput("DensityPlot")))),
                         tabPanel("Trait and classes", DTOutput("TraitClasses")),
                         tabPanel("Traits Meta data" ,DTOutput("TraitMetaData"))
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
                                          choices = list("Classification" = "classification",
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
                  tabBox(width = 12,
                         tabPanel("Model Summary", textOutput("modelSummary")),
                         tabPanel("Model diagnosis", plotOutput("modelPlots")), 
                         tabPanel("Coeffecients", plotOutput("modelCoefPlot")), 
                         tabPanel("Selectivity Ratio", plotOutput("modelSelRatioPlot")), 
                         tabPanel("Performance Metrics", plotOutput("modelPerfMetrPlot"))
                         
                  )
              )
      ),
      
      # Make Predictions Tab
      tabItem(tabName = "makePrediction",
              # Prediction Parameters Inputs
              box(title = "Prediction Parameters", status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE, 
                  fluidRow(
                    column(3, selectInput("predictionCountry", "Country", choices = c("Morocco", "Tunisia", "Lebanon", "Mexico"), 
                                          multiple = T, selectize = T)),
                    column(3, selectInput("predictionLocation", "Location", choices = c("-","Annoceur","Beja","Beni-Mellal","Chebika",
                                                                                        "Ciudad Obregón","Douyet","El Kef", "Jemâa-Shaim","Melk Zher" ,
                                                                                        "Merchouch","Oued Mliz","Sidi el Aïdi","Tassaout","Terbol"),
                                          multiple = TRUE, selected = NULL)),
                    column(3, selectInput("predictionCrop", "Crop", choices = c("Barley","Bread Wheat", "Chickpea","Lentil","Faba Bean","Durum Wheat"),
                                          selected = NULL, multiple = T)),
                    column(3, sliderInput("predictionYear", "Year", min = 2010, max = 2023, value = 2022))
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
                  DTOutput("predictionTable"), 
              ), 
              box(width = 12, solidHeader = TRUE, status = "success", collapsible = TRUE,
                  # Download Predictions Button
                  downloadButton("downloadPredictions", "Download Predictions", class = "btn-success")
              ),
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
    selectInput("trait", "Trait to model",choices =colnames(traitData())[18:46],
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
    
    mdaplot(originalData, type = "l", main = "Original NIR Data")
  })
  
  # Plot Preprocessed NIR Data
  output$preprocessedPlots <- renderPlot({
    req(preprocessedData())
    
    # Ensure preprocessing method is selected and preprocessing has been run
    if (!is.null(input$preprocessingMethod) && input$runPreprocessing > 0) {
      processed <- preprocessedData()[[input$preprocessingMethod]]
      
      # Check if the processed data exists and is not null
      if (!is.null(processed) && ncol(processed) > 0) {
        # Assuming mdaplot can directly take the processed data
        mdaplot(processed, type = "l", 
                main = paste("Preprocessed Data - Method:", input$preprocessingMethod))
      } else {
        # If processed data is not available, attempt to plot the original data
        originalData <- preprocessedData()[["Original"]]
        if (!is.null(originalData) && ncol(originalData) > 0) {
          mdaplot(originalData, type = "l", main = "Original NIR Data")
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
    selectInput("traittoplot", "Trait to class", choices = colnames(traitData())[18:46], selected = "Protein")
  })
  
  # Define analysisResults as a reactive value to store PCA model results
  analysisResults <- reactiveVal(NULL)
  
  # analysisResults stores the PCA model and trainIndex for reference
  observeEvent(input$runAnalysis, {
    req(input$multivariateAnalysis, input$traittoplot,traitData(), nirData())
    
    withProgress(message = 'Traning Model...', value = 0, {
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
      
      # Fit PCA model
      modelPCA <- mdatools::pca(Xc ,x.test = Xt, scale = FALSE, ncomp = 7)
      
      # Store PCA model, trainIndex, and traitData for plotting
      analysisResults(list(model = modelPCA, trainIndex = trainIndex,
                           traitData = combinedData[, 1], NIRData = combinedData[,-1]))
      
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
  
  # Plot for PCA Loadings
  output$Loadings <- renderPlot({
    req(analysisResults())
    
    results <- analysisResults()
    if (!is.null(results$model) && input$multivariateAnalysis == "PCA") {
      plotResiduals(results$model,  main = paste("PCA Loadings of", input$traittoplot))
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
                           min = floor(traitMin), max = round(traitMax, 2),
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
  
  classData <- reactiveVal()
  
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
      
      cleanDataForPlotting <- classData() #%>% filter(!is.na(TraitValue))
      
      # Generate the plot
      p <- ggplot(cleanDataForPlotting, aes(x = TraitValue, group=Class, fill = Class)) +
        geom_density(alpha = 0.5) +
        scale_fill_manual(values = rainbow(length(unique(cleanDataForPlotting$Class)))) +
        labs(title = paste("Density Plot by Class for", input$traittoplot), x = input$traittoplot, y = "Density") +
        theme_classic()
      
      p 
    })
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
  
  # UI for selecting model type based on the task
  output$modelSelection <- renderUI({
    if(input$taskType == "regression") {
      selectInput("modelType", "Select Model", choices = c("PCA","PLS", "IPLS"))
    } else if(input$taskType == "classification") {
      selectInput("modelType", "Select Model", choices = c("PLS-DA", "SIMCA"))
    }
  })
  
  # UI for specifying interval length in IPLS (only relevant for IPLS)
  output$intervalLength <- renderUI({
    if(input$modelType == "IPLS") {
      numericInput("intervalLength", "Interval Length", value = 10, min = 1, max = 100)
    } else {
      NULL
    }
  })
  
  # Reactive storage for model results
  ModelResult <- reactiveVal(NULL)
  
  # Run Modeling Task
  observeEvent(input$runModel, {
    req(input$modelType, preprocessedData(), input$taskType, classData())
    
    withProgress(message = 'Training Model...', value = 0, {
      for (k in 1:5) {
        incProgress(1/5)
        # Sys.sleep(0.1)  # Simulated delay for fetching data
      }
      
      # Filter class data based on the task type
      responseColumn <- if (input$taskType == "regression") {
        "TraitValue"  # Column for regression
      } else {
        "Class"  # Column for classification
      }
      
      # Combine preprocessed NIRS and filtered Class data
      combinedData <- cbind(classData()[, c(responseColumn, "Class")], preprocessedData())
      
      # Data partitioning
      set.seed(123)  # For reproducibility
      trainIndex <- sample(nrow(combinedData), size = floor(0.75 * nrow(combinedData)))
      
      Xc <- combinedData[trainIndex, -c(1,2)]  # Calibration predictors
      yc <- combinedData[trainIndex, 1]   # Calibration response
      Xt <- combinedData[-trainIndex, -c(1,2)] # Test predictors
      yt <- combinedData[-trainIndex, 1]  # Test response
      
      # Model fitting logic based on selected model type
      fittedModel <- switch(input$modelType,
                            "PLS" = pls(Xc, yc, x.test =Xt, y.test = yt,  ncomp = 5, scale = F),
                            "IPLS" = ipls(Xc, yc, x.test =Xt, y.test = yt, 
                                          glob.ncomp = 5, int.num = input$intervalLength),
                            "PLS-DA" = plsda(Xc, yc, x.test =Xt, ncomp = 5, scale = F),
                            "SIMCA" = simca(Xc, yc,x.test =Xt, y.test = yt, ncomp = 5, scale = F),
                            pca(Xc, x.test =Xt, ncomp = 5)  # Default fallback method
      )
      ModelResult(list(Model = fittedModel))  #Store the model result
    })
  })
  
  # output Model summary
  output$modelSummary <- renderPrint({
    req(ModelResult())
    print(ModelResult()$Model)
  })
  
  # output model diagnosis plots
  output$modelPlots <- renderPlot({
    req(analysisResults())
    
    if (input$modelType %in% c("PLS", "PLS-DA")) {
      plot(ModelResult()$Model, comps = 1:2)
    } else if (input$modeltype == "SIMCA") {
      plot(ModelResult()$Model)
    }
  })
  
  ## output model evaluation Metrics
  
  # Coefficients 
  output$modelCoefPlot <- renderPlot({
    req(analysisResults(), ModelResult())
    
    plotRegcoeffs(ModelResult()$Model, type="h", show.ci=T, show.labels=F)
    plotRegcoeffs(ModelResult()$Model, type="l", show.ci=T, show.labels=F)
  })  
  
  
  # Selectivity Ratio
  output$modelSelRatioPlot <- renderPlot({
    req(analysisResults(), ModelResult())
    
    plotVIPScores(ModelResult()$Model, ncomp = 5, type = "h", show.labels = F)
    plotSelectivityRatio(ModelResult()$Model, ncomp = 5, type = "h", show.labels = F)
    
  })  
  
  # Performance
  output$modelPerfMetrPlot <- renderPlot({
    req(ModelResult())
    
    plotPredictions(ModelResult()$Model, ncomp = 5, show.labels = F)
    plotRMSE(ModelResult()$Model, ncomp = 5)
    
  })  
  
}

# Run the application
shinyApp(ui, server)




