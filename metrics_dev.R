library(shiny)
library(plotly)
library(MASS)
library(dplyr)
library(caret)
library(pROC)
library(shinydashboard)
library(pracma) # Additional library for trapz function

# UI for Shiny Dashboard
ui <- dashboardPage(
    
    dashboardHeader(title = "Metrics"),
    dashboardSidebar(
                    sidebarMenu(menuItem("Analysis", tabName = "analysisTab", icon = icon("chart-line"))),
                    sliderInput("n1", "Number of points in Cluster 1", min = 1, max = 200, value = 100),
                    sliderInput("n2", "Number of points in Cluster 2", min = 1, max = 200, value = 100),
                    sliderInput("mean1", "Mean of Cluster 1", min = 0, max = 10, value = 1),
                    sliderInput("mean2", "Mean of Cluster 2", min = 0, max = 10, value = 3),
                    sliderInput("sd1", "Standard Deviation of Cluster 1", min = 0.1, max = 10, value = 1),
                    sliderInput("sd2", "Standard Deviation of Cluster 2", min = 0.1, max = 10, value = 1)
                    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "analysisTab",
              fluidRow(
                box(plotlyOutput("dataPlot"), width = 6),
                box(plotlyOutput("precisionRecallPlot"), width = 6),
                box(textOutput("metricsText"), width = 6), # Display AP and AUPRC
                box(plotlyOutput("histogramPlot"), width = 6) # New box for histogram
              )
      )
    )
  )
)



# Server for Shiny Dashboard
server <- function(input, output)
  
  {
  
  # Reactive expression to generate data based on input
  generateData <- reactive({
    n1 <- input$n1
    n2 <- input$n2
    mean1 <- rep(input$mean1, 2)
    mean2 <- rep(input$mean2, 2)
    sd1 <- input$sd1
    sd2 <- input$sd2
    cov1 <- matrix(c(sd1, 0, 0, sd1), nrow = 2)
    cov2 <- matrix(c(sd2, 0, 0, sd2), nrow = 2)
    
    cluster1 <- mvrnorm(n1, mean1, cov1)
    cluster2 <- mvrnorm(n2, mean2, cov2)
    data <- rbind(data.frame(X = cluster1[,1], Y = cluster1[,2], Cluster = 'Cluster 1'),
                  data.frame(X = cluster2[,1], Y = cluster2[,2], Cluster = 'Cluster 2'))
    
    data$Cluster <- as.numeric(data$Cluster == 'Cluster 1')
    data
  })
  
  
  # Reactive variable to store the data and predictions
  dataStorage <- reactiveVal()
  
  # Observe any change in inputs and update the data and predictions
  observeEvent(c(input$n1, input$n2, input$mean1, input$mean2, input$sd1, input$sd2), {
    data <- generateData()
    model <- glm(Cluster ~ X + Y, data = data, family = 'binomial')
    predictions <- predict(model, type = "response")
    dataStorage(list(data = data, predictions = predictions))  # Update dataStorage
  }, ignoreNULL = FALSE)

  # Plot for the data
  output$dataPlot <- renderPlotly({
    storage <- dataStorage() 
    data <- storage$data
    plot_ly(data, x = ~X, y = ~Y, color = ~factor(Cluster), type = 'scatter', mode = 'markers') %>%
      layout(title = 'Cluster Distribution', xaxis = list(title = 'X'), yaxis = list(title = 'Y'))
  })
  
  # Reactive value for storing hovered threshold
  hoveredThreshold <- reactiveVal(0)
  
  # Plot for precision-recall and metrics calculation
  output$precisionRecallPlot <- renderPlotly({
    storage <- dataStorage() 
    data <- storage$data
    predictions <- storage$predictions

    
    thresholds <- seq(0, 1, length.out = 200)
    precision_values <- numeric(length(thresholds))
    recall_values <- numeric(length(thresholds))
    
    for (i in seq_along(thresholds)) {
      threshold <- thresholds[i]
      predicted_labels <- ifelse(predictions > threshold, 1, 0)
      cm <- confusionMatrix(factor(predicted_labels, levels = c(0, 1)), factor(data$Cluster, levels = c(0, 1)))
      precision_values[i] <- cm$byClass['Pos Pred Value']
      recall_values[i] <- cm$byClass['Sensitivity']
    }
    
    
    
    
    # calculate ap and auprc
    ap <- mean(precision_values, na.rm = TRUE)
    recall_values_auprc<-recall_values
    recall_values_auprc[is.na(recall_values_auprc)] <- 0
    precision_values_auprc <- precision_values
    precision_values_auprc[is.na(precision_values_auprc)] <- 1
    auprc <- trapz(recall_values, precision_values_auprc)
    
    
    
    output$metricsText <- renderText({
      paste("AP:", round(ap, 2), "AUPRC:", round(auprc, 2))
    })
    
    plot_ly(x = recall_values, y = precision_values, type = 'scatter', mode = 'lines+markers', 
            text = ~paste('Threshold:', round(thresholds, 2)), source = "precisionRecallPlot") %>%
      layout(title = 'Precision-Recall Curve', xaxis = list(title = 'Recall'), yaxis = list(title = 'Precision'))
  })
  

  
  
  observeEvent(event_data("plotly_hover", source = "precisionRecallPlot"), {
    hover_info <- event_data("plotly_hover", source = "precisionRecallPlot")
    if (!is.null(hover_info)) {
      # Safeguard to ensure valid threshold values
      print(hover_info)
      
      valid_threshold <- min(max(hover_info$x, 0), 1)
      print(valid_threshold)
      hoveredThreshold(valid_threshold)
    }
  })
  
  output$histogramPlot <- renderPlotly({
    # Accessing the reactive data
    storage <- dataStorage() 
    data <- storage$data
    predictions <- storage$predictions
    
    threshold <- hoveredThreshold()
    
    # Creating the histogram plot for classifier scores
    plot <- plot_ly() %>%
      add_histogram(x = predictions[data$Cluster == 0], name = 'Cluster 0', marker = list(color = 'blue'), opacity = 0.6) %>%
      add_histogram(x = predictions[data$Cluster == 1], name = 'Cluster 1', marker = list(color = 'green'), opacity = 0.6) %>%
      layout(barmode = 'overlay', xaxis = list(title = 'Classifier Score',range=c(0,1)), yaxis = list(title = 'Count')) %>%
      add_lines(x = c(threshold, threshold), y = c(0, max(hist(predictions, plot = FALSE)$counts)), line = list(color = 'red', width = 2))
    
    plot
  })
  
}  
  
# Run the app
shinyApp(ui, server) 
  
