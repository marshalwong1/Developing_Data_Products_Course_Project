library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Predicting the results of the Titanic with Machine Learning"),
  
  # Sidebar with controls to choose and set parameters of various prediction 
  # methods
  sidebarPanel(
    sliderInput("split", "Choose the proportion of the training set:",
                0.25, 0.75, 0.60, step=0.05),
    
    selectInput("method", "Choose a model generation method:", 
                choices = c("Trees", "Random forests")),    
    
    # Resampling methods
    selectInput("resampMethod", "Choose a resampling method:", 
                choices = c("Bootstrapping", "Cross-validation")),
    
    conditionalPanel(
      condition = "input.resampMethod == 'Bootstrapping'",
      sliderInput("resampNum", "Number Of Resamples:", 1, 25, 1, step=1, 
                  round=TRUE, format="0")
      ),
    
    conditionalPanel(
      condition = "input.resampMethod == 'Cross-validation'",
      sliderInput("numFolds", "Number Of Folds:", 2, 10, 2, step=1, 
                  round=TRUE, format="0")
    ),
        
    actionButton("goButton", "Go")
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    h3("Instructions"),
    p("This application allows you to explore the effects of certain 
      parameters used in certain prediction methods.  Data with respect
      to the Titanic passengers was obtained from Kaggle's Titanic: 
      Machine Learning from Disaster competition.  Basic pre-processing
      such as feature extraction and imputation was preformed on the data."),
    p("The first choice you can make is how much of the 891 observations
      to use as the training set."),
    p("Then you can select which model generation method to use."),
    p("Lastly, you can select what method of resampling to use and its
      parameters."),
    h3("Results"),
    p("The confusion matrix when evaluating the model against the testing data.
      Rows are predicted values.  Columns are actual results:"),
    tableOutput("model"),
    p("Out-of-Sample Error:"),
    verbatimTextOutput("oob")
  )
))