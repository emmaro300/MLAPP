
#Load Shiny module   

library(shiny)
library(bslib)
library(rsconnect)
#Global variables
prep <- c("Scale", "Normalise","Standardize", "Encode","Reduce Dimension")
samp <- c("Random Sample", "Stratified Sample")
resamp <- c("K-Fold CV", "Bootstrap")
mod <- c("Ordinary Least Squares","Partial Least Squares","Principal Component Regression","Regularised Regression","Logistic Regression","Multi Adaptive Regression Spline","K Nearest Neighbour","Decision Tree","Bootstrap Aggregation","Random Forest","Gradient Boosting Machines","Extreme Gradient Boosting Machines", "Deep Learning Neural Network", "Support Vector Machines")
regress <- c("MSE","RMSE","MAE","RSQ")
classify <- c("Accuracy","Specificity","Sensitivity","Cross Entropy","Gini Index","ROC")
plt <- c("ROC","VIP","PDP","CART","ANN","BEST_FIT")
# Define UI for Machine Learning application 
ui <- fluidPage(
  #Page title
  theme = bs_theme(bootswatch="cerulean"),
  title = "Machine Learning Models",
 
    # Application title
    titlePanel("ML App"),
    # Define navbarpage panel which contains tabpanel
   navbarPage(
     title = "ML",
    tabPanel(
      title = "Input Data",
      fileInput(
        "file",
        "Upload data file",
        buttonLabel = "Upload",
        accept = ".csv",
        placeholder = "csv only"
      )
      
    ),
        
    navbarMenu(
      "Preprocess",
    tabPanel(
      "Normalize"
    ),
    tabPanel(
      "Standardize"
    ),
    tabPanel(
      "Scale"
    ),
    tabPanel(
      "Encode"
    ),
    tabPanel(
      "Reduce Dimension"
    )
    ),
    
    navbarMenu(
      "Partition",
      tabPanel(
        "Random Sample"
      ),
      tabPanel(
        "Stratified Sample"
      )
    ),
    
    navbarMenu(
      "Resample",
      tabPanel(
        "K-fold CV"
      ),
      tabPanel(
        "Bootstrap"
      )
    ),
    
    navbarMenu(
      "Train",
      tabPanel(
        "Ordinary Least squares"
      ),
      tabPanel(
        "Partial Least Squares"
      ),
      tabPanel(
        "Principal Component Regression"
      ),
      tabPanel(
        "Logistic Regression"
      ),
      tabPanel(
        "Regularized Regression"
      ),
      
      tabPanel(
        "Multi Adaptive Regression Spline"
      ),
      tabPanel(
        "K Nearest Neighbour"
      ),
      tabPanel(
        "Decision Tree"
      ),
      tabPanel(
        "Bootstrap Aggregation"
      ),
      tabPanel(
        "Random Forest"
      ),
      tabPanel(
        "Gradient Boosting Machine"
      ),
      tabPanel(
        "Extreme Gradient Boosting Machine"
      ),
      tabPanel(
        "Deep Learning Neural Network"
      ),
      tabPanel(
        "Support vector Machine"
      )
    ),
    navbarMenu(
      "Evaluate",
      tabPanel(
        "Regression"
      ),
      tabPanel(
        "Classification"
      )
    ),
      
    navbarMenu(
      "Test and Predict",
      tabPanel(
        "Test"
      ),
      tabPanel(
        "Predict"
      )
    ),
    navbarMenu(
      "Results",
      tabPanel(
        "Table"
      ),
      tabPanel(
        "Plot"
      )
    )
      
   )
 )
    
# Define server logic required to run ML model
server <- function(input, output) {
  
} 

# Run the application 
shinyApp(ui = ui, server = server)

