
####
#  Document Path: /LikertMaker_Shiny/app.R
#
#  Author: Hume Winzar
#
#  Date: 2024-03-06
#
#  Title: LikertMakeR online
#
#  Description: shiny app for generating synthetic rating-scale data from a notional Cronbach's Alpha
#
#  R Version: R version 4.3.2 (2024-02-29 ucrt)
#
####

###### Libraries ____________________________________________

library(shiny) # obvious
library(shinythemes) # make pretty
library(shinycssloaders) # interactive and pretty

library(matrixcalc) # to test positive definite status
library(ragg) # to save output chart
library(markdown) # text files
library(DT) # data table rendering
# library(gridlayout) # for shiny UI editor

library(LikertMakeR)

##### Define UI _______________________________________________

ui <- fluidPage(
  theme = shinytheme("paper"),
  ## cerulean, journal, paper
  titlePanel("LikertMakeR online"),
  tabsetPanel(
    tabPanel(
      "Background & Instructions",
      sidebarLayout(
        sidebarPanel(
          h5("How to use LikertMakeR online"),
          img(
            src = "LikertMakeR_2.png",
            height = "75%", width = "75%", align = "center"
          )
        ),
        mainPanel(
          includeMarkdown("www/how_to.md")
        )
      )
    ), ## END "Background & Instructions" tabPanel
    
    
    tabPanel(
      "Correlation Matrix from Cronbach's Alpha",
      sidebarLayout(
        sidebarPanel(
          h5("Input parameters to generate a correlation matrix"),
          numericInput("alpha", "Cronbach's Alpha",
                       value = 0.85, min = -0.5, max = 0.99, step = 0.01
          ),
          numericInput("items", "Number of Items (Columns/Rows)",
                       value = 5, min = 2, max = 32, step = 1
          ),
          numericInput("variance", "Variance (optional)",
                       value = 0.5, min = 0, max = 2, step = 0.01
          ),
          br(), # Space before the action button
          actionButton("calculate", "Calculate",
                       icon("calculator"),
                       style = "color: #ffffff;
                       background-color: #007bff; border-color: #007bff"
          )
        ),
        mainPanel(
          tableOutput("matrix") |> withSpinner(type = 5),
          verbatimTextOutput("matrixStatus"),
          verbatimTextOutput("cronbachAlpha"),
          verbatimTextOutput("eigenValues")
        )
      )
    ), ## END "Cronbach's Alpha to Correlation Matrix" tabPanel
 
       
    tabPanel(
      "Generate Synthetic Data", 
      sidebarLayout(
        sidebarPanel(
          h5("For each variable in your data set,
             please input the desired mean, standard deviation,
             and lower & upper boundaries"),
          numericInput("n", "Number of Observations",
                       value = 64, min = 4, max = 512, step = 1
          ),
          uiOutput("dynamicInputs"),
          actionButton("generate", "Generate Synthetic Data",
                       icon("paper-plane"),
                       style = "color: #ffffff;
                       background-color: #007bff; border-color: #007bff"
          ),
          br(), br(),
          downloadButton("downloadData", "Download as CSV"),
          helpText("Comma-separated Values file")
        ),
        
        
        
        mainPanel(
          dataTableOutput("syntheticData") |> withSpinner(type = 4)
        )
      )
    ), ## END "Generate Synthetic Data" tabPanel
    
    
    tabPanel(
      "Validate Synthetic Data",
      sidebarLayout(
        sidebarPanel(
          h5("Visualisation Controls"),
          #  Generate Plot button
          actionButton("plotData", "Update Plot",
                       icon("image"),
                       style = "color: #ffffff;
                       background-color: #007bff; border-color: #007bff"
          ),
          br(), br(),
          downloadButton("downloadPlot", "Download Plot")
        ), ## END sidebar
        mainPanel(
          # Output for the pairs plot
          h5("Visual Summary"),
          plotOutput("dataVis", width = 600, height = 600),
          # means and standard deviations
          h5("Summary Moments"),
          verbatimTextOutput("dataSummary"),
          # Cronbach's Alpha
          verbatimTextOutput("cronbachAlphaOutput")
        )
      )
    ), ## END "Validate Synthetic Data" tabPanel
    
    tabPanel(
      "About LikertMakeR",
      sidebarLayout(
        sidebarPanel(
          h5("About LikertMakeR online"),
          img(
            src = "LikertMakeR_2.png",
            width = "75%", align = "center"
          )
        ),
        mainPanel(
          includeMarkdown("www/about.md")
        )
      )
    ) ## END "About LikertMakeR" tabPanel
    
    
  ) ## END tabsetPanel
) ## END fluidPage

###
##### Server logic ___________________________________________
###
server <- function(input, output, session) {
  # Reactive expression for generated matrix
  resultMatrix <- eventReactive(input$calculate, {
    if (input$calculate > 0) {
      tryCatch(
        {
          matrix <- makeCorrAlpha(
            alpha = input$alpha,
            items = input$items,
            variance = input$variance
          )
          if (!is.positive.definite(matrix)) {
            stop(
              paste0("The generated matrix is not positive definite."),
              paste0("  Please try again."),
              paste0("  If no success after several attempts, then try reducing the value of ‘Variance’.")
            )
          }
          return(matrix) # Return the matrix if positive definite
        },
        error = function(e) {
          return(list(error = e$message)) # Return an error as a list
        }
      )
    }
  })

  output$matrix <- renderTable({
    # Render the matrix if no error is present
    if (is.matrix(resultMatrix())) {
      return(resultMatrix())
    } else if (is.list(resultMatrix()) && !is.null(resultMatrix()$error)) {
      # Handle the case where an error has occurred
      return(data.frame(Error = resultMatrix()$error))
    }
  })

  output$matrixStatus <- renderPrint({
    if (is.list(resultMatrix()) && !is.null(resultMatrix()$error)) {
      resultMatrix()$error
    } else {
      "Positive-Definite Matrix generated successfully."
    }
  })

  # Calculate and display Cronbach's Alpha
  output$cronbachAlpha <- renderPrint({
    if (is.matrix(resultMatrix())) {
      c_alpha <- alpha(resultMatrix())
      paste("Cronbach's Alpha: ", round(c_alpha, 4))
    }
  })

  # Calculate and display the eigenvalues
  output$eigenValues <- renderPrint({
    if (is.matrix(resultMatrix())) {
      eigen_vals <- eigen(resultMatrix())$values
      paste("Eigenvalues: ", toString(round(eigen_vals, 4)))
    }
  })

  ## Dynamic inputs for variable parameters
  output$dynamicInputs <- renderUI({
    numItems <- input$items
    inputFields <- lapply(1:numItems, function(i) {
      fluidRow(
        column(3, numericInput(paste0("mean", i),
          label = paste0("Mean v", i), value = 3
        )),
        column(3, numericInput(paste0("sd", i),
          label = paste0("SD v", i), value = 1
        )),
        column(3, numericInput(paste0("lower", i),
          label = paste0("Lower v", i), value = 1
        )),
        column(3, numericInput(paste0("upper", i),
          label = paste0("Upper v", i), value = 5
        ))
      )
    })
    do.call(tagList, inputFields)
  })

  # Synthetic data generation logic with dynamic inputs
  syntheticData <- eventReactive(input$generate, {
    req(resultMatrix()) # Check 'resultMatrix' has been calculated
    # Check if 'resultMatrix' contains an error
    if (is.list(resultMatrix()) && !is.null(resultMatrix()$error)) {
      return(data.frame(Error = resultMatrix()$error))
    }

    matrix <- resultMatrix()
    n <- input$n
    numItems <- input$items
    means <- sapply(1:numItems, function(i) input[[paste0("mean", i)]])
    sds <- sapply(1:numItems, function(i) input[[paste0("sd", i)]])
    lowerbounds <- sapply(1:numItems, function(i) input[[paste0("lower", i)]])
    upperbounds <- sapply(1:numItems, function(i) input[[paste0("upper", i)]])

    # Validation passed, proceed to generate data
    tryCatch(
      {
        data <- makeItems(n, means, sds, lowerbounds, upperbounds, matrix)
        return(data)
      },
      error = function(e) {
        return(data.frame(Error = e$message))
      }
    )
  })


  output$syntheticData <- renderDT({
    syntheticData()
  })

  ## Visualise the data and correlations
  # A reactive expression to store plot data only when 'Generate Plot' is clicked
  plotData <- eventReactive(input$plotData,
    {
      req(syntheticData()) # Ensure there are data to plot
      syntheticData() # Holds the data to be used for plotting
    },
    ignoreNULL = FALSE
  )

  ### Function to generate the pairs plot
  ##
  generatePairsPlot <- function(data) {
    ##
    ## Upper triangle for correlation coefficients
    panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
      usr <- par("usr")
      on.exit(par(usr = usr))
      par(usr = c(0, 1, 0, 1))
      r <- cor(x, y, use = "complete.obs")
      txt <- format(c(r, 0.123456789), digits = digits)[1]
      txt <- paste(prefix, txt, sep = "")
      if (missing(cex.cor)) cex.cor <- 0.8 / strwidth(txt)
      text(0.5, 0.5, txt, cex = 1.5 + abs(r))
    }
    ## Diagonal for histograms
    panel.hist <- function(x, ...) {
      usr <- par("usr"); on.exit(par(usr = usr))
      par(usr = c(usr[1:2], 0, 1.5))
      h <- hist(x, plot = FALSE)
      breaks <- h$breaks 
      nB <- length(breaks) 
      y <- h$counts
      y <- y / max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col = "#368849")
    }
    ## Lower triangle for scatterplots with regression lines
    panel.lm <- function(x, y, col = "#36758825", bg = NA,
                         pch = 19, cex = 1, col.smooth = "#884936", ...) {
      points(x, y, pch = pch, col = col, bg = bg, cex = cex)
      abline(stats::lm(y ~ x), col = col.smooth, ...)
    }
    ## plotting the visualisation
    pairs(data,
      upper.panel = panel.cor,
      diag.panel = panel.hist,
      lower.panel = panel.lm,
      gap = 0.25
    )
  }

  # Calculate and display means and standard deviations
  output$dataSummary <- renderPrint({
    req(plotData()) # Ensure the plot data is ready
    data <- plotData()
    myMoments <- data.frame(
      mean = apply(data, 2, mean, na.rm = TRUE) |> round(3),
      sd = apply(data, 2, sd, na.rm = TRUE) |> round(3)
    ) |> t()
    # print("Summary Moments")
    print(myMoments)
  })

  # Calculate and display Cronbach's Alpha
  output$cronbachAlphaOutput <- renderPrint({
    req(syntheticData()) # Make sure syntheticData is available
    data <- syntheticData()
    cr_alpha <- alpha(NULL, data)
    paste("Cronbach's Alpha: ", round(cr_alpha, 4))
  })

  # Render the plot in the UI when the 'Generate Plot' button is clicked
  output$dataVis <- renderPlot({
    req(plotData()) # Ensure the plot data is ready
    generatePairsPlot(plotData()) # Generate and display the plot
  })
  # Download handler for the plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("LikertMakeR_corr_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(plotData()) # Ensure the plot data is ready
      ragg::agg_png(file,
        width = input$items * 100,
        height = input$items * 100,
        res = 144
      )
      generatePairsPlot(plotData()) # Re-generate the plot for saving
      dev.off()
    }
  )

  # Add download handler for downloading the synthetic data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("synthetic-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- syntheticData()
      write.csv(data, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
