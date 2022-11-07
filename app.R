
#Load Shiny module   

library(shiny)
library(bslib)
library(rsconnect)
library(shinydashboard)

# Define UI for Machine Learning application 

  ui <- dashboardPage(
    dashboardHeader(title = "MLAPP"),
    
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem(
        "File", 
        tabName = "file",
          menuSubItem(text = "New", tabName = "new"),
          menuSubItem(text = "Open", tabName = "open"),
          menuSubItem(text = "Save As", tabName = "save_as"),
          menuSubItem(text = "Save", tabName = "save"),
          menuSubItem(text = "Exit", tabName = "save"),
        icon = icon("th")
        ),
        menuItem(
          "Preprocess", 
          tabName = "preprocess",
          menuSubItem(text = "Normalize", tabName = "normalize"),
          menuSubItem(text = "Standardize", tabName = "standardize"),
          menuSubItem(text = "Scale", tabName = "scale"),
          menuSubItem(text = "Encode", tabName = "encode"),
          menuSubItem(text = "Reduce Dimension",tabName = "rdim"),
          icon = icon("list-alt")
    ),
       menuItem(
        "Partition", 
        tabName = "partition",
         menuSubItem(text = "Random Sample", tabName = "rsamp"),
         menuSubItem(text = "Stratifies Sample", tabName = "ssamp"),
         icon = icon("list-alt")
    ),
    menuItem(
      "Resample", 
      tabName = "resample",
      menuSubItem(text = "K-Fold CV", tabName = "kcv"),
      menuSubItem(text = "Bootstrap", tabName = "bootstrap"),
      icon = icon("list-alt")
    ),
    menuItem(
      "Train", 
      tabName = "train",
      menuSubItem(text = "Ordinary Least Squares", tabName = "ols"),
      menuSubItem(text = "Partial Least Squares", tabName = "pls"),
      menuSubItem(text = "Principal Component Regression", tabName = "pcr"),
      menuSubItem(text = "Logistic Regression", tabName = "lr"),
      menuSubItem(text = "Regularised Regression",tabName = "rr"),
      menuSubItem(text = "Multi Adaptive Regression Spline",tabName = "mars"),
      menuSubItem(text = "K Nearest Neighbour",tabName = "knn"),
      menuSubItem(text = "Decision Tree",tabName = "dt"),
      menuSubItem(text = "Bootstrap Aggregation",tabName = "bagging"),
      menuSubItem(text = "Randome Forest",tabName = "rf"),
      menuSubItem(text = "Gradient Boosting Machine",tabName = "gbm"),
      menuSubItem(text = "Extreme Gradient Boosting Machine",tabName = "xgboost"),
      menuSubItem(text = "Support Vector Machine",tabName = "svm"),
      menuSubItem(text = "Deep Learning Neural Network",tabName = "dlnn"),
      icon = icon("list-alt")
    ),
    menuItem(
      "Evaluate", 
      tabName = "evaluate",
      menuSubItem(text = "MSE", tabName = "mse"),
      menuSubItem(text = "RMSE", tabName = "rmse"),
      menuSubItem(text = "MAE", tabName = "mae"),
      menuSubItem(text = "RSQ", tabName = "rsq"),
      menuSubItem(text = "Accuracy", tabName = "accuracy"),
      menuSubItem(text = "Sensitivity", tabName = "sensitivity"),
      menuSubItem(text = "Specificity", tabName = "specificity"),
      menuSubItem(text = "ROC", tabName = "roc"),
      menuSubItem(text = "Gini Index", tabName = "gi"),
      menuSubItem(text = "Cross Entropy", tabName = "ce"),
      icon = icon("list-alt")
    ),
    menuItem(
      "Test", 
      tabName = "test",
      menuSubItem(text = "Test", tabName = "test"),
      menuSubItem(text = "Predict", tabName = "predict"),
      icon = icon("list-alt")
    ),
    menuItem(
      "Result", 
      tabName = "result",
      menuSubItem(text = "Table", tabName = "table"),
      menuSubItem(text = "VIP", tabName = "vip"),
      menuSubItem(text = "PDP", tabName = "pdp"),
      menuSubItem(text = "ROC", tabName = "rocp"),
      menuSubItem(text = "ANN", tabName = "ann"),
      menuSubItem(text = "Tree", tabName = "tree"),
      icon = icon("list-alt")
    ),
    menuItem(
      "Help", 
      tabName = "help",
      menuSubItem(text = "MLAPP Documentation", tabName = "mlapp"),
      menuSubItem(text = "Tutorial", tabName = "tutorial"),
      menuSubItem(text = "Shortcut Summary", tabName = "ssummary"),
      icon = icon("list-alt")
    )
  )
),
  
    body = dashboardBody(
      tabItems(
        tabItem("hi")
      )
    )
  )

# Define server logic required to run ML model
server <- function(input, output) {
  
} 

# Run the application 
shinyApp(ui = ui, server = server)

