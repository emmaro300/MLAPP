##Load required packages
library(Metrics)
library(loadeR)
library(transformeR)
library(downscaleR)
library(visualizeR)
library(ncdf4)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

### This function predicts future climate after tidying data

climatePredict <- function(trainData,trainModel, histLon, histLat,gcmPr, gcmTasmin,gcmTasmax, missValue = 1e+20,period, retConditon = 1){
  # Create variables
  predictors <- c(gcmPr,gcmTasmin,gcmTasmax)
  variables <- c("pr","tasmin","tasmax")
  retValue <- NULL
  aggr_nf <- NULL
  aggr_mf_ff <- list()
  nf_downscaled_df <- NULL
  mf_ff_prediction <- list()
  mf_ff_downscaled_df <- list()
  
  #Load data
  loaded_data_pr <- loadGridData(  
    dataset = predictors[1],
    var = variables[1],
    lonLim = histLon,
    latLim = histLat,
    season = 1:12,
    years = period[1]:period[2]
  )  
  loaded_data_tasmin <- loadGridData(
    dataset = predictors[2],
    var = variables[2],
    lonLim = histLon,
    latLim = histLat,
    season = 1:12,
    years = period[1]:period[2]
  ) 
  loaded_data_tasmax <- loadGridData(
    dataset = predictors[3],
    var = variables[3],
    lonLim = histLon,
    latLim = histLat,
    season = 1:12,
    years = period[1]:period[2]
  ) 
  
  # tidy data
  if(any(loaded_data_pr$Data==missValue)||any(is.na(loaded_data_pr$Data))){
    stop("Found missing values or NAs")
  }
  
  if(any(loaded_data_tasmin$Data==missValue)||any(is.na(loaded_data_tasmin$Data))){
    stop("Found missing values or NAs")
  }
  
  if(any(loaded_data_tasmax$Data==missValue)||any(is.na(loaded_data_tasmax$Data))){
    stop("Found missing values or NAs")
  }
  ##Convert future climate variables to desired units
  prData_mm <- gridArithmetics(loaded_data_pr,86400,operator = "*")
  tasminData_degC <- gridArithmetics(loaded_data_tasmin,273.15,operator = "-")
  tasmaxData_degC <- gridArithmetics(loaded_data_tasmax,273.15,operator = "-")
  
  ## Make multi grid of predictors
  predictors_multiGrid <- makeMultiGrid(prData_mm, tasminData_degC,tasmaxData_degC)
  
  ##Prepare and predict future climate:precipitation for each period
  
  prediction_data <- prepareNewData(newdata = predictors_multiGrid,data.structure = trainData)
  
  future_prediction <- downscalePredict(newdata = prediction_data, model = trainModel)
  
  ## Implementing function return value based on condition
  if(retConditon==1){
    
    if(period[1]==2015&&period[2]==2049){
      print("Starting Year: 2015")
      print("Ending Year: 2049")
      print("Aggregating for the period: 2015:2049")
      nf_prediction <- subsetGrid(future_prediction, years = 2023:2049)
      aggr_nf <- aggregateGrid(nf_prediction, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
      
      nf_date <- as.Date(aggr_nf$Dates$start)
      nf_years <- year(nf_date) 
      nf_downscaled <- aggr_nf$Data[,1,1]
      nf_downscaled_df <- data.frame(Period = nf_years, Downscaled = nf_downscaled)
      retValue = nf_downscaled_df
      print("Aggregation successfully done")
    }
    else if(period[1]==2050&&period[2]==2100){
      print("Starting Year: 2050")
      print("Ending Year: 2100")
      print("Aggregating for the period: 2050:2100")
      mf_prediction <- subsetGrid(future_prediction, years = 2050:2074)
      ff_prediction <- subsetGrid(future_prediction, years = 2075:2100)
      
      aggr_mf <- aggregateGrid(mf_prediction, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
      aggr_ff <- aggregateGrid(ff_prediction, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
      
      mf_date <- as.Date(aggr_mf$Dates$start)
      ff_date <- as.Date(aggr_ff$Dates$start)
      
      mf_years <- year(mf_date) 
      ff_years <- year(ff_date) 
      
      mf_downscaled <- aggr_mf$Data[,1,1]
      ff_downscaled <- aggr_ff$Data[,1,1]
      
      mf_downscaled_df <- data.frame(Period = mf_years, Downscaled = mf_downscaled)
      ff_downscaled_df <- data.frame(Period = ff_years, Downscaled = ff_downscaled)
      combine_mf_ff <- rbind(mf_downscaled_df,ff_downscaled_df)
      
      retValue = combine_mf_ff
      print("Aggregation successfully done")
    }
    
  }
  
  ## return daily prediction 
  if(retConditon==2){
    future_period_character <- future_prediction$Dates[[1]]
    future_period <- as.Date(future_period_character)
    
    daily_future_downscaled <- future_prediction$Data[,1,1]
    daily_future_downscaled_df <- data.frame(Period = future_period, Downscaled = daily_future_downscaled, GCM_prediction = prData_mm$Data[,1,1])
    retValue <- daily_future_downscaled_df
    print("Data frame of daily precipitation successful")
  }
  
  if(retConditon==3&&period[1]==2015&&period[2]==2049){
    nf_prediction <- subsetGrid(future_prediction, years = 2023:2049)
    aggr_nf <- aggregateGrid(nf_prediction, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
    
    nf_date <- as.Date(aggr_nf$Dates$start)
    nf_years <- year(nf_date) 
    nf_downscaled <- aggr_nf$Data[,1,1]
    nf_downscaled_df <- data.frame(Period = nf_years, Downscaled = nf_downscaled)
    print("Aggregation successfully done")
    
    longterm_future_mean_annual_nf <- data.frame(nf20232049=mean(nf_downscaled_df$Downscaled))
    retValue <- longterm_future_mean_annual_nf
    print("Long term mean annual rainfall successfully computed")
  }
  else if(retConditon==3&&period[1]==2050&&period[2]==2100){
    print("Starting Year: 2050")
    print("Ending Year: 2100")
    print("Aggregating for the period: 2050:2100")
    mf_prediction <- subsetGrid(future_prediction, years = 2050:2074)
    ff_prediction <- subsetGrid(future_prediction, years = 2075:2100)
    
    aggr_mf <- aggregateGrid(mf_prediction, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
    aggr_ff <- aggregateGrid(ff_prediction, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
    
    mf_date <- as.Date(aggr_mf$Dates$start)
    ff_date <- as.Date(aggr_ff$Dates$start)
    
    mf_years <- year(mf_date) 
    ff_years <- year(ff_date) 
    
    mf_downscaled <- aggr_mf$Data[,1,1]
    ff_downscaled <- aggr_ff$Data[,1,1]
    longterm_future_mean_annual_mf_ff <- data.frame(mf20502074=mean(mf_downscaled),ff20752100=mean(ff_downscaled))
    
    retValue <- longterm_future_mean_annual_mf_ff
    print("Aggregation successfully done")
  }
  ## Visualize time series of mean annual rainfall
  if(retConditon==4&&period[1]==2015&&period[2]==2049){
    nf_prediction <- subsetGrid(future_prediction, years = 2023:2049)
    
    print("Aggregating")
    aggr_nf <- aggregateGrid(nf_prediction, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
    
    retValue <- temporalPlot(aggr_nf)
    print("Plot Successful")
  } 
  else if(retConditon==4&&period[1]==2050&&period[2]==2100){
    
    mf_prediction <- subsetGrid(future_prediction, years = 2050:2074)
    ff_prediction <- subsetGrid(future_prediction, years = 2075:2100)
    
    print("Aggregating")
    aggr_mf <- aggregateGrid(mf_prediction, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
    aggr_ff <- aggregateGrid(ff_prediction, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))  
    
    retValue <- temporalPlot(aggr_mf,aggr_ff)
    print("Plot Successful")
  }
  return(retValue)
}



##############################################################
### This function train statistical downscale model after tidying data
climateTrain <- function(obsPr,histPr, histTasmin,histTasmax, obsLon, obsLat, histLon, histLat, missValue = 1e+20, period, retConditon = 1, n_analogs = 1,n_window = 3, n_sim, n_random, window_random, replace = FALSE, rsq = 0.5){
  
  # Create variables
  variables <- c("pr","tasmin","tasmax")
  retVal <- NULL 
  
  #Load data
  loaded_data_obs_pr <- loadGridData(  
    dataset = obsPr,
    var = variables[1],
    lonLim = obsLon,
    latLim = obsLat,
    season = 1:12,
    years = period[1]:period[2]
  )  
  loaded_data_hist_pr <- loadGridData(
    dataset = histPr,
    var = variables[1],
    lonLim = histLon,
    latLim = histLat,
    season = 1:12,
    years = period[1]:period[2]
  ) 
  loaded_data_hist_tasmin <- loadGridData(
    dataset = histTasmin,
    var = variables[2],
    lonLim = histLon,
    latLim = histLat,
    season = 1:12,
    years = period[1]:period[2]
  ) 
  loaded_data_hist_tasmax <- loadGridData(
    dataset = histTasmax,
    var = variables[3],
    lonLim = histLon,
    latLim = histLat,
    season = 1:12,
    years = period[1]:period[2]
  ) 
  
  # tidy data
  if(any(loaded_data_obs_pr$Data==missValue)||any(is.na(loaded_data_obs_pr$Data))){
    stop("Found missing values or NAs")
  }
  
  if(any(loaded_data_hist_pr$Data==missValue)||any(is.na(loaded_data_hist_pr$Data))){
    stop("Found missing values or NAs")
  }
  
  if(any(loaded_data_hist_tasmin$Data==missValue)||any(is.na(loaded_data_hist_tasmin$Data))){
    stop("Found missing values or NAs")
  }
  
  if(any(loaded_data_hist_tasmax$Data==missValue)||any(is.na(loaded_data_hist_tasmax$Data))){
    stop("Found missing values or NAs")
  } 
  
  ##Convert climate variables to desired units
  obsPr_mm <- loaded_data_obs_pr
  histPr_mm <- gridArithmetics(loaded_data_hist_pr,86400,operator = "*")
  histTasmin_degC <- gridArithmetics(loaded_data_hist_tasmin,273.15,operator = "-")
  histTasmax_degC <- gridArithmetics(loaded_data_hist_tasmax,273.15,operator = "-")
  
  print(paste0("Head of observation", " ", obsPr_mm$Data[1:5,1,1]))
  print(paste0("Head of GCM precipitation", " ",histPr_mm$Data[1:5,1,1]))
  print(paste0("Head of GCM minimum temperature", " ",histTasmin_degC$Data[1:5,1,1]))
  print(paste0("Head of GCM maximum temperature", " ", histTasmax_degC$Data[1:5,1,1]))
  
  ## Make multi grid of predictors
  predictors_multiGrid <- makeMultiGrid(histPr_mm, histTasmin_degC, histTasmax_degC)
  
  # Get temporal intersection 
  y_predictand <- getTemporalIntersection(obsPr_mm, predictors_multiGrid,"obs")
  x_predictors <- getTemporalIntersection(obsPr_mm, predictors_multiGrid,"prd")
  hist_pr_19882014 <- getTemporalIntersection(obsPr_mm,histPr_mm,"prd")
  
  ## Split data into training and testing by subsetting and prepare for training
  hist_pr_19882006 <- subsetGrid(hist_pr_19882014, years = 1988:2006)
  train_predictors <- subsetGrid(x_predictors, years = 1988:2006)
  train_predictand <- subsetGrid(y_predictand, years = 1988:2006)
  train_data <- prepareData(x = train_predictors, y= train_predictand)
  
  hist_pr_20072014 <- subsetGrid(hist_pr_19882014, years = 2007:2014)
  test_predictand <- subsetGrid(y_predictand, years = 2007:2014)
  test_predictors <- subsetGrid(x_predictors, years = 2007:2014)
  
  ##Prepare and train climate model 
  train_analogs <- downscaleTrain(obj = train_data, method = "analogs", n.analogs = n_analogs, window = n_window)
  
  if(retConditon==1){
    
    retVal <- train_analogs
    print("Model train successful")
  }
  else if(retConditon==2){
    retVal <- train_data
    print("Train data successfully returned")
  }
  
  if(retConditon==3){
    n <- 1
    retVal <- NULL
    while(n <= n_sim){
      #Random sample and simulate 
      samp_n_analog <- sample(seq(n_random[1],n_random[2]), size = 1, replace = replace)
      samp_window <- sample(seq(window_random[1],window_random[2]), size = 1, replace = replace)
      
      print(paste0("Start Simulation:", " ", n))
      print(paste0("Analogs:", " ", samp_n_analog))
      print(paste0("Windows:", " ", samp_window))
      
      train_analogs <- downscaleTrain(obj = train_data, method = "analogs", n.analogs = samp_n_analog, window = samp_window)
      
      ## Extract observation and model prediction
      hist_train_pr <- hist_pr_19882006$Data[,1,1]
      obs_train_pr <- train_predictand$Data[,1,1]
      analog_train_prediction <- train_analogs$pred$Data[,1,1]
      
      ## Assess model trained performance
      perform_analogs_train <- performance(observation = obs_train_pr, prediction = analog_train_prediction)
      
      if(perform_analogs_train$RSQ >=rsq){
        
        print(paste0("RSQ:", " ", perform_analogs_train$RSQ))
        print(paste0("RMSE:", " ", perform_analogs_train$RMSE))
        #print(paste0("Model prediction summary:", " ",summary(analog_train_prediction)))
        #print(paste0("Observation summary:", " ",summary(obs_train_pr)))
        
        ## plot series
        best_train_prediction <- train_analogs$pred$Data[,1,1]
        ## Visualize model prediction, observation and gcm historical
        train_period_character <- train_predictand$Dates[[1]]
        train_period <- as.Date(train_period_character)
        train_obs_pred <- data.frame(Period = train_period, Observation = obs_train_pr, Downscaled = best_train_prediction, GCM_prediction = hist_train_pr)
        
        best_plot <- ggplot(data = train_obs_pred)+
          geom_line(mapping = aes(x = train_period, y = obs_train_pr, colour ="Observation"))+
          geom_line(mapping = aes(x = train_period, y = best_train_prediction, colour = "Downscaled"))+
          geom_line(mapping = aes(x = train_period, y = hist_train_pr, colour = "GCM prediction", alpha=0.8))+
          labs(x= "Time (days)", y= "Daily Rainfall (mm)", title = "Training Period: Daboase Station")+
          scale_colour_manual(name = "", values = c("red","green","blue"))+
          theme(legend.position = "top", plot.title = element_text(size = 12), axis.title = element_text(size = 12))
        
        retVal <- best_plot
        print(paste0("Model reach objective function after simulation:", " ", n))
        print(paste0("End Simulation:", " ", n))
      }
      n <- n+1
    }
  }
  
  if(retConditon==4){
    # Test set, data preparation and prediction
    test_predictand <- subsetGrid(y_predictand, years = 2007:2014)
    test_predictors <- subsetGrid(x_predictors, years = 2007:2014)
    test_data <- prepareNewData(newdata = test_predictors,data.structure = train_data)
    test_analogs <- downscalePredict(newdata = test_data, model = train_analogs)
    
    ## Extract observation and model prediction
    hist_test_pr <- hist_pr_20072014$Data[,1,1]
    obs_test_pr <- test_predictand$Data[,1,1]
    analog_test_prediction <- test_analogs$Data[,1,1]
    
    ## Test model performance
    perform_analogs_test <- performance(observation = obs_test_pr, prediction = analog_test_prediction)
    print(paste0("RSQ:", " ", perform_analogs_test$RSQ))
    print(paste0("Model prediction summary:", " ",summary(analog_test_prediction)))
    print(paste0("Observation summary:", " ",summary(obs_test_pr)))
    
    # Visualize and assess the performance of test prediction
    test_period_character <- test_predictand$Dates[[1]]
    test_period <- as.Date(test_period_character)
    test_obs_pred <- data.frame(Period = test_period, Observation = obs_test_pr, Downscaled = analog_test_prediction, GCM_prediction = hist_test_pr)
    
    test_plot <- ggplot(data = test_obs_pred)+
      geom_line(mapping = aes(x = test_period, y = obs_test_pr, colour = "Observation"))+
      geom_line(mapping = aes(x = test_period, y = analog_test_prediction, colour = "Downscaled"))+
      geom_line(mapping = aes(x = test_period, y = hist_test_pr, colour = "GCM prediction"))+
      labs(x= "Time (days)", y= "Daily Rainfall (mm)", title = "Testing Period: Daboase Station")+
      scale_colour_manual(name = "", values = c("red","green","blue"))+
      theme(legend.position = "top", plot.title = element_text(size = 12), axis.title = element_text(size = 12))
    
    retVal <- test_plot
  }
  
  if(retConditon==5){
    
    ### Train process
    
    ## Extract observation and model prediction
    hist_train_pr <- hist_pr_19882006$Data[,1,1]
    obs_train_pr <- train_predictand$Data[,1,1]
    analog_train_prediction <- train_analogs$pred$Data[,1,1]
    
    train_period_character <- train_predictand$Dates[[1]]
    train_period <- as.Date(train_period_character)
    train_obs_pred <- data.frame(Period = train_period, Observation = obs_train_pr, Downscaled = analog_train_prediction, GCM_prediction = hist_train_pr)
    
    ### Test process
    
    # Test set, data preparation and prediction
    test_data <- prepareNewData(newdata = test_predictors,data.structure = train_data)
    test_analogs <- downscalePredict(newdata = test_data, model = train_analogs)
    
    ## Extract observation and model prediction
    hist_test_pr <- hist_pr_20072014$Data[,1,1]
    obs_test_pr <- test_predictand$Data[,1,1]
    analog_test_prediction <- test_analogs$Data[,1,1]
    
    test_period_character <- test_predictand$Dates[[1]]
    test_period <- as.Date(test_period_character)
    test_obs_pred <- data.frame(Period = test_period, Observation = obs_test_pr, Downscaled = analog_test_prediction, GCM_prediction = hist_test_pr)
    
    ## Combine training and testing results into one data frame
    train_test_df <- rbind(train_obs_pred,test_obs_pred)
    
    ##Aggregate grid for training and testing period
    aggr_observe_train  <- aggregateGrid(train_predictand, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
    aggr_observe_test <- aggregateGrid(test_predictand, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
    aggr_downscaled_train <- aggregateGrid(train_analogs$pred, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
    aggr_downscaled_test <- aggregateGrid(test_analogs, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
    
    ## Prepare data frame of aggregated grids
    train_years <- as.Date(aggr_observe_train$Dates$start)
    test_years <- as.Date(aggr_observe_test$Dates$start)
    train_years <- year(train_years)     
    test_years <- year(test_years)
    
    train_mean_annual <- data.frame(Period = train_years, Observation=aggr_observe_train$Data[,1,1], Downscaled = aggr_downscaled_train$Data[,1,1])
    test_mean_annual <- data.frame(Period = test_years, Observation=aggr_observe_test$Data[,1,1], Downscaled = aggr_downscaled_test$Data[,1,1])
    
    combined_mean_annual <- rbind(train_mean_annual,test_mean_annual)
    
    #retVal <- combined_mean_annual
    
    longterm_mean_annual <- data.frame(Observation = mean(combined_mean_annual$Observation), Downscaled = mean(combined_mean_annual$Downscaled))
    print(paste0("Long term mean annual rainfall:Observation:", " ",longterm_mean_annual$Observation))
    print(paste0("Long term mean annual rainfall:Downscaled:", " ",longterm_mean_annual$Downscaled))
    retVal <- longterm_mean_annual
  }
  return(retVal)
}



######################################################################
########################This function extract and aggregate station data and return

extract_station <- function(statFiles, obslon,obslat, period, retCondition = FALSE){
  retValue <- NULL
  annualRainfall <- vector("list", length(statFiles))
  for (i in seq(1:length(statFiles))) {
    observe_data <- loadGridData(
      dataset = statFiles[i],
      var = "pr",
      lonLim = oblon[[i]],
      latLim = oblat[[i]],
      season = 1:12,
      years = period[1]:period[2]
    )
    
    annual_grid <- aggregateGrid(observe_data, aggr.m = list(FUN = "sum"), aggr.y = list(FUN = "sum"))
    annualRainfall[[i]] <- annual_grid$Data[,1,1]
  }
  
  if(retCondition==TRUE){
    
    retValue <- annualRainfall
    return(retValue)
    
  } else{
    long_annual <- vector("list", length(annualRainfall))
    for (i in seq(1:length(annualRainfall))) {
      long_annual[[i]] <- mean(annualRainfall[[i]])
    }
    retValue <- long_annual
    return(retValue)
  }
  
  return(retValue)
}



####################################Iteration over train model
############################ This function is an iterator and iterates the climateTrain and climatePredict models
iterate_climateModel <- function(stationFile, histPr,histTasmin,histTasmax, statLon, statLat, modLon, modLat,missValue = 1e+20, trainPeriod, gcmPr, gcmTasmin,gcmTasmax,predPeriod = NULL, n_random = c(1,10), window_random, n_sim = 10, rsq = 0.001, retConditon = 1, obsNames = NULL, replace = TRUE){
 #Create global variables  
  retvalue <- NULL
  
  ###############train analog model  
  if(trainPeriod[1]==1988&&trainPeriod[2]==2014&&is.null(predPeriod)==TRUE){
    
    if(retConditon==1){
      modTrain <- vector("list", length(stationFile))
      for (i in seq(1:length(stationFile))) { 
        ##Start
        print(paste0("Starting Run", " ",i," ", "for", " ", stationFile[i], " ", "Station"))
        
        modTrain[[i]] <- climateTrain(stationFile[i], histPr,histTasmin,histTasmax,obsLon = statLon[[i]],obsLat = statLat[[i]], histLon = modLon[[i]], histLat = modLat[[i]], missValue = missValue, period = trainPeriod, n_random = n_random, window_random = window_random, n_sim = n_sim, rsq = rsq, retConditon = retConditon, replace = replace)
        
        ##End
        print(paste0("successfully completed run", " ",i," ", "for", " ", statFiles[i], " ", "Station"))
      }
      retValue <- modTrain
      return(retValue)
    }
    
    ################## return train data
    if(retConditon==2){
      modTrain <- vector("list", length(stationFile))
      for (i in seq(1:length(stationFile))) {
        ##Start
        print(paste0("Starting Run", " ",i," ", "for", " ", stationFile[i], " ", "Station"))
        
        modTrain[[i]] <- climateTrain(stationFile[i], histPr,histTasmin,histTasmax,obsLon = statLon[[i]],obsLat = statLat[[i]], histLon = modLon[[i]], histLat = modLat[[i]], missValue = missValue, period = trainPeriod, n_random = n_random, window_random = window_random, n_sim = n_sim, rsq = rsq, retConditon = retConditon, replace = replace)
        
        ##End
        print(paste0("successfully completed run", " ",i," ", "for", " ", statFiles[i], " ", "Station"))
      }
      retValue <- modTrain
      return(retValue)
    }
    
    ################## return train data
    if(retConditon==3){
      modTrain <- vector("list", length(stationFile))
      for (i in seq(1:length(stationFile))) {
        ##Start
        print(paste0("Starting Run", " ",i," ", "for", " ", stationFile[i], " ", "Station"))
        
        modTrain[[i]] <- climateTrain(stationFile[i], histPr,histTasmin,histTasmax,obsLon = statLon[[i]],obsLat = statLat[[i]], histLon = modLon[[i]], histLat = modLat[[i]], missValue = missValue, period = trainPeriod, n_random = n_random, window_random = window_random, n_sim = n_sim, rsq = rsq, retConditon = retConditon, replace = replace)
        
        ##End
        print(paste0("successfully completed run", " ",i," ", "for", " ", statFiles[i], " ", "Station"))
      }
      retValue <- modTrain
      return(retValue)
    }
    
    ################## return analog data
    if(retConditon==4){
      modTrain <- vector("list", length(stationFile))
      for (i in seq(1:length(stationFile))) {
        ##Start
        print(paste0("Starting Run", " ",i," ", "for", " ", stationFile[i], " ", "Station"))
        
        modTrain[[i]] <- climateTrain(stationFile[i], histPr,histTasmin,histTasmax,obsLon = statLon[[i]],obsLat = statLat[[i]], histLon = modLon[[i]], histLat = modLat[[i]], missValue = missValue, period = trainPeriod, n_random = n_random, window_random = window_random, n_sim = n_sim, rsq = rsq, retConditon = retConditon, replace = replace)
        
        ##End
        print(paste0("successfully completed run", " ",i," ", "for", " ", statFiles[i], " ", "Station"))
      }
      retValue <- modTrain
      return(retValue)
    }
    
    ################## return analog data
    if(retConditon==5){
      modTrain <- vector("list", length(stationFile))
      for (i in seq(1:length(stationFile))) {
        ##Start
        print(paste0("Starting Run", " ",i," ", "for", " ", stationFile[i], " ", "Station"))
        
        modTrain[[i]] <- climateTrain(stationFile[i], histPr,histTasmin,histTasmax,obsLon = statLon[[i]],obsLat = statLat[[i]], histLon = modLon[[i]], histLat = modLat[[i]], missValue = missValue, period = trainPeriod, n_random = n_random, window_random = window_random, n_sim = n_sim, rsq = rsq, retConditon = retConditon, replace = replace)
        
        ##End
        print(paste0("successfully completed run", " ",i," ", "for", " ", statFiles[i], " ", "Station"))
      }
      retValue <- modTrain
      return(retValue)
    }
  }
  
  ####################### #########################################
  #########################Make predictions based on the best analog model
  if(is.null(predPeriod)==FALSE&&predPeriod[1]!=trainPeriod[1]&&predPeriod[2]!=trainPeriod[2]&&predPeriod[1]!=trainPeriod[2]){
    if(is.null(obsNames)==TRUE){
      stop("Argument to the obsNames parameter is required. This is a string containing the names of the station.
           The length should be equal to the length of the stationFile.")
    }
    ### train analog model
    train_analogs <- vector("list", length(stationFile))
    for (i in seq(1:length(stationFile))) {
      ##Start  
      print(paste0("Starting Run", " ",i," ", "for", " ", stationFile[i], " ", "Station"))
      
      train_analogs[[i]] <- climateTrain(stationFile[i], histPr,histTasmin,histTasmax,obsLon = statLon[[i]],obsLat = statLat[[i]], histLon = modLon[[i]], histLat = modLat[[i]], missValue = missValue, period = trainPeriod, n_random = n_random, window_random = window_random, n_sim = n_sim, rsq = rsq, retConditon = 1, replace = replace)
      
      ##End
      print(paste0("successfully completed run", " ",i," ", "for", " ", statFiles[i], " ", "Station"))
    }
    
    ################ prepare train data
    train_data <- vector("list", length(stationFile))
    for (i in seq(1:length(stationFile))) {
      ##Start
      print(paste0("Starting Run", " ",i," ", "for", " ", stationFile[i], " ", "Station"))
      
      train_data[[i]] <- climateTrain(stationFile[i], histPr,histTasmin,histTasmax,obsLon = statLon[[i]],obsLat = statLat[[i]], histLon = modLon[[i]], histLat = modLat[[i]], missValue = missValue, period = trainPeriod, n_random = n_random, window_random = window_random, n_sim = n_sim, rsq = rsq, retConditon = 2, replace = replace)
      
      ##End
      print(paste0("successfully completed run", " ",i," ", "for", " ", statFiles[i], " ", "Station"))
    }
    
    ####Return Near future mean annual rainfall prediction
    if(retConditon==1&&predPeriod[1]==2015&&predPeriod[2]==2049){
    
      modPredict <- vector("list", length(train_analogs))
      for (i in seq(1:length(train_analogs))) {
        ##Start
        print(paste0("Starting Run", " ",i," ", "for", " ", obsNames[i], " ", "Station"))
        
        modPredict[[i]] <- climatePredict(trainData = train_data[[i]], trainModel = train_analogs[[i]], histLon = modLon[[i]], histLat = modLat[[i]], gcmPr = gcmPr, gcmTasmin = gcmTasmin,gcmTasmax = gcmTasmax, period = predPeriod, retConditon = 1)
        
        ##End
        print(paste0("successfully completed run", " ",i," ", "for", " ", statFiles[i], " ", "Station"))
      }
      retValue <- modPredict
      return(retValue)
    }
     ################## Return mid and far future mean annual rainfall predictions
    else if(retConditon==1&&predPeriod[1]==2050&&predPeriod[2]==2100){
      modPredict <- vector("list", length(train_analogs))
      for (i in seq(1:length(train_analogs))) {
        ##Start
        print(paste0("Starting Run", " ",i," ", "for", " ", obsNames[i], " ", "Station"))
        
        modPredict[[i]] <- climatePredict(trainData = train_data[[i]], trainModel = train_analogs[[i]], histLon = modLon[[i]], histLat = modLat[[i]], gcmPr = gcmPr, gcmTasmin = gcmTasmin,gcmTasmax = gcmTasmax, period = predPeriod, retConditon = 1)
        
        ##End
        print(paste0("successfully completed run", " ",i," ", "for", " ", statFiles[i], " ", "Station"))
      }
      retValue <- modPredict
      return(retValue)
    }
    
    #### Return near future long term mean annual rainfall 
    else if(retConditon==2&&predPeriod[1]==2015&&predPeriod[2]==2049){
      modPredict <- vector("list", length(train_analogs))
      for (i in seq(1:length(train_analogs))) {
        ##Start
        print(paste0("Starting Run", " ",i," ", "for", " ", obsNames[i], " ", "Station"))
        
        modPredict[[i]] <- climatePredict(trainData = train_data[[i]], trainModel = train_analogs[[i]], histLon = modLon[[i]], histLat = modLat[[i]], gcmPr = gcmPr, gcmTasmin = gcmTasmin,gcmTasmax = gcmTasmax, period = predPeriod, retConditon = 2)
        
        ##End
        print(paste0("successfully completed run", " ",i," ", "for", " ", statFiles[i], " ", "Station"))
      }
      retValue <- modPredict
      return(retValue)
    }
    
    ### Return mid and far future long term mean annual rainfall
    else if(retConditon==3&&predPeriod[1]==2050&&predPeriod[2]==2100){
      modPredict <- vector("list", length(train_analogs))
      for (i in seq(1:length(train_analogs))) {
        ##Start
        print(paste0("Starting Run", " ",i," ", "for", " ", obsNames[i], " ", "Station"))
        
        modPredict[[i]] <- climatePredict(trainData = train_data[[i]], trainModel = train_analogs[[i]], histLon = modLon[[i]], histLat = modLat[[i]], gcmPr = gcmPr, gcmTasmin = gcmTasmin,gcmTasmax = gcmTasmax, period = predPeriod, retConditon = 3)
        
        ##End
        print(paste0("successfully completed run", " ",i," ", "for", " ", statFiles[i], " ", "Station"))
      }
      retValue <- modPredict
      return(retValue)
    }
    
  }
  ### Return final output 
  return(retvalue)
}


