library(shiny)
library(e1071)
library(randomForest)
library(caret)
data <- read.csv("./train.csv")

# pre-Processing data cleansing
data$Survived[data$Survived == 1] <- 'Survived'
data$Survived[data$Survived == 0] <- 'Died'
data$Survived <- as.factor(data$Survived)
data$Child <- 0
data$Child[data$Age < 18] <- 1
data$Fare2 <- '30+'
data$Fare2[data$Fare < 30 & data$Fare >= 20] <- '20-30'
data$Fare2[data$Fare < 20 & data$Fare >= 10] <- '10-20'
data$Fare2[data$Fare < 10] <- '<10'

medianAge <- median(data$Age, na.rm=TRUE)
data$Age[is.na(data$Age) ] <- medianAge

relevantData <- data[,-c(1, 4, 9, 11)]

runTrain <- function(method, resampMethod, resampNum, trainingData) {
  set.seed(1)
  modfit <- train(Survived ~ ., data=trainingData, 
                  method=method, 
                  trControl = trainControl(method = resampMethod,
                                           number = resampNum,
                                           verboseIter = TRUE)
  )
  return(modfit)
}

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  inTrain <- reactive({
    if (input$goButton == 0)
      return()
    
    isolate({
      createDataPartition(y=relevantData$Survived, p=input$split, list=FALSE)
      })
  })
    
  training <- reactive({
    relevantData[inTrain(), ]
  })
  
  testing <- reactive({
    relevantData[-inTrain(), ]
  })
  
  # Assigns the prediction method type
  method <- reactive({
    switch(input$method,
           "Trees" = "rpart",
           "Random forests" = "rf")
  })
  
  resampMethod <- reactive({
    switch(input$resampMethod,
           "Bootstrapping" = "boot",
           "Cross-validation" = "cv")    
  })
  
  number <- reactive({
    if (resampMethod() == "boot")
      input$resampNum
    else if (resampMethod() == "cv")
      input$numFolds
    else 1
  })
  
  modFit <- reactive({
    if (input$goButton == 0)
      return()
    
    isolate({
      runTrain(method(), resampMethod(), number(), training())
    })  
  })
  
  pred <- reactive({
    if (input$goButton == 0)
      return()
    
    predict(modFit(), newdata = testing())
  })
  
  output$model <- 
    renderTable({
      if (input$goButton == 0)
        return()

      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.25)
                     }
                   })
      
      table(pred(), testing()$Survived, dnn = c("Predicted", "Actual"))
    })
  
  output$oob <- renderPrint({
    if (input$goButton == 0)
      return()
    
    1 - sum(pred() == testing()$Survived) / length(testing()$Survived)
  })
  
})