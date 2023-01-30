## Project: This script is used to load and downscale GCM climate data using station data
## Load packages for climate data science
library(loadeR)
library(transformeR)
library(downscaleR)
library(visualizeR)
library(convertR)
library(ncdf4)
library(fields)
library(climate4R.datasets)
library(Metrics)  
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(sf)
library(stars)
library(mapview)
library(leaflet)
source("performance_metrics.R") 
source("ncdfMaker.R") 
source("functionals.R") 
source("climateModel.R") 
## Set working directory 
setwd("C:/Users/HP/Documents/WREM PhD/PhD Project/First Level Datasets and maps/Secondary datasets/Climate/climate input output")

## Import data
    # Predictors
hist_pr_file <- "Input/pr_day_HadGEM3-GC31-LL_historical_r1i1p1f3_gn_19500101-20141230.nc"
hist_tasmin_file <- "Input/tasmin_day_HadGEM3-GC31-LL_historical_r1i1p1f3_gn_19500101-20141230.nc"
hist_tasmax_file <- "Input/tasmax_day_HadGEM3-GC31-LL_historical_r1i1p1f3_gn_19500101-20141230.nc"
 ## open predictors's nc file and check missing values

nc_pr <- nc_open(ddd)
nc_tasmin <- nc_open(hist_tasmin_file)
nc_tasmax <- nc_open(hist_tasmax_file)

(missval_pr <-nc_pr$var$pr$missval)
(missval_tasmin <- nc_tasmin$var$tasmin$missval)
(missval_tasmax <- nc_tasmax$var$tasmax$missval)

(nc_pr$var$pr$units)
(nc_tasmin$var$tasmin$units)
(nc_tasmax$var$tasmax$units)
nc_pr$var$time_bnds
 #Predictands
#obs_daboase_file  <- c("Input/Twifo.nc")
obs_daboase_file  <- c("Input/daboase_station5.nc","Input/Twifo.nc")
##Take inventory of datasets
invent_hist_pr <- dataInventory(hist_pr_file)
invent_hist_tasmin<- dataInventory(hist_tasmin_file)
invent_hist_tasmax<- dataInventory(hist_tasmax_file)
invent_obs_pr<- dataInventory(obs_daboase_file)
invent_hist_pr
invent_hist_tasmin$tasmin$Dimensions$time
invent_obs_pr$pr$Dimensions$time
invent_hist_pr$pr$Dimensions$lon
invent_hist_tasmin$tasmin$Dimensions$lon
invent_obs_pr$pr$Dimensions$lon


##Extract and load datasets
### Load predictors 
hist_pr_load <- loadGridData(
  dataset = hist_pr_file,
  var = "pr",
  lonLim = c(-2,-0.5),
  latLim = c(4,6),
  season = 1:12,
  years = 1988:2014
)     ##Load model historical precipitation prediction                               

hist_tasmin_load <- loadGridData(
  dataset = hist_tasmin_file,  
  var = "tasmin",
  lonLim = c(-2,-0.5),
  latLim = c(4,6),
  season = 1:12,
  years = 1988:2014  
)   ##Load model historical minimum temperature prediction  
hist_tasmax_load <- loadGridData(
  dataset = hist_tasmax_file,
  var = "tasmax",
  lonLim = c(-2,-0.5),
  latLim = c(4,6),
  season = 1:12,
  years = 1988:2014
)   ##Load model historical maximum temperature prediction  
## Load predictands
obs_daboase_load <- loadGridData(
  dataset = obs_daboase_file,
  var = "pr",
  lonLim = c(-1.8,-0.8),
  latLim = c(4,5.5),
  season = 1:12,
  years = 1988:2014
)  ##Load daboase station measured data of precipitation   
### Check validity of unconverted data 
hist_pr_load$Data[1:5,1,1]
hist_tasmax_load$Data[1:5,1,1]
hist_tasmin_load$Data[1:5,1,1]
obs_daboase_load$Data[1:5,1,1]

## Tidy up the data for NAs and missing values
any(hist_pr_load$Data==missval_pr)
any(hist_tasmin_load$Data==missval_tasmin)
any(hist_tasmax_load$Data==missval_tasmax)
any(obs_daboase_load$Data==missval_pr)

any(is.na(hist_pr_load$Data))
any(is.na(hist_tasmin_load$Data))
any(is.na(hist_tasmax_load$Data))
any(is.na(obs_daboase_load$Data)) 

which(is.na(obs_daboase_load$Data))
length(obs_daboase_load$Data[,1,1])
#obs_upb1_load$Data[c(50,422),,] <- array(c(0,0,0,0,0,0,0,0), dim = c(2,2,2))


##Convert variables
hist_pr_mm <- gridArithmetics(hist_pr_load,86400,operator = "*")# convert kg/m2/s to mm/day
hist_tasmin_degC <- gridArithmetics(hist_tasmin_load,273.15,operator = "-")# convert K to degC
hist_tasmax_degC <- gridArithmetics(hist_tasmax_load,273.15,operator = "-")# convert K to degC
obs_daboase_mm <-obs_daboase_load

### Check validity of converted data 
hist_pr_mm$Data[1:5,1,1]
hist_tasmax_degC$Data[1:5,1,1]
hist_tasmin_degC$Data[1:5,1,1]
obs_daboase_mm$Data[1:5,1,1]

length(hist_pr_mm$Data[,1,1])
length(hist_tasmin_degC$Data[,1,1])
length(hist_tasmax_degC$Data[,1,1])
length(obs_daboase_mm$Data[,1,1])

## Make multi - grid of predictors 
x_predictorsGrid <- makeMultiGrid(hist_pr_mm,hist_tasmin_degC,hist_tasmax_degC)
dim(x_predictorsGrid$Data)

## Get temporal intersection 

y_predictand <- getTemporalIntersection(obs_daboase_mm,x_predictorsGrid,"obs")
x_predictors <- getTemporalIntersection(obs_daboase_mm,x_predictorsGrid,"prd")
hist_pr_19882014 <- getTemporalIntersection(obs_daboase_mm,hist_pr_mm,"prd")

dim(hist_pr_19882014$Data)
dim(y_predictand$Data)
dim(x_predictors$Data)

## Split data into training and testing by subsetting and prepare for training
hist_pr_subset <- subsetGrid(hist_pr_19882014, years = 1988:2006)
train_predictors <- subsetGrid(x_predictors, years = 1988:2006)
train_predictand <- subsetGrid(y_predictand, years = 1988:2006)
train_data <- prepareData(x = train_predictors, y= train_predictand)

test_predictand <- subsetGrid(y_predictand, years = 2007:2014)
test_predictors <- subsetGrid(x_predictors, years = 2007:2014)
hist_pr_test <- subsetGrid(hist_pr_19882014, years = 2007:2014)
hist_pr_test  <- hist_pr_test$Data[,1,1]

dim(test_predictand$Data)
dim(test_predictors$Data)

dim(hist_pr_subset$Data)
dim(train_predictand$Data)
dim(train_predictors$Data)

hist_pr_19882006 <- hist_pr_subset$Data[1:6807,1,1]

## Downscale training for the period 1988-2006
analog_daboase <- downscale(y = test_predictand,x = test_predictors, method = "analogs", sel.fun = "mean", cross.val = "kfold", folds = 20)
glm_daboase <- downscale(y = train_predictand,x = train_predictors, method = "glm", simulate = TRUE, cross.val = "kfold", folds = 10)
lm_daboase <- downscale(y = train_predictand,x = train_predictors, method = "lm", cross.val = "kfold", folds = 10, n.pcs = 2)
range(analog_daboase$Dates) 

##Extract training period and convert to date
period <- analog_daboase$Dates[[1]]
period <- as.Date(period)
length(period)
observation <- y_predictand$Data[1:6807,1,1]
analog_prediction <- analog_daboase$Data[1:6807,1,1]
glm_prediction <- glm_daboase$Data[1:6807,1,1]
lm_prediction <- lm_daboase$Data[1:6807,1,1]

analog_prediction   
obs_pred <- data.frame(period,observation, analog_prediction)
obs_pred <- data.frame(period,observation, analog_prediction, glm_prediction)
View(obs_pred)

summary(observation)
summary(analog_prediction)
summary(glm_prediction)
summary(lm_prediction)

## Testing of downscale
observation_test <- test_predictand$Data[,1,1]
analog_prediction_test <- analog_daboase$Data[,1,1]
obs_pred_test <- data.frame(period,observation_test, analog_prediction_test)
performance(observation_test,analog_prediction_test)

ggplot(data = obs_pred_test)+
  geom_line(mapping = aes(x = period, y = observation_test), colour="blue")+
  geom_line(mapping = aes(x = period, y = analog_prediction_test), colour="red")

## Assess performance of model
analog_daboase_perform <- performance(observation = observation,prediction = analog_prediction)
glm_daboase_perform <- performance(observation = observation,prediction = glm_prediction)
lm_daboase_perform <- performance(observation = observation,prediction = lm_prediction)

analog_daboase_perform
glm_daboase_perform
lm_daboase_perform

## Visualize prediction and observation
ggplot(data = obs_pred)+
  geom_point(mapping = aes(x=analog_prediction,y=observation), colour="blue", shape=1, size = 2)
 
ggplot(data = obs_pred)+
  geom_line(mapping = aes(x = period, y = observation), colour="blue")
 

ggplot(data = obs_pred)+
  geom_line(mapping = aes(x = period, y = glm_prediction), colour="red")+
  geom_line(mapping = aes(x = period, y = observation), colour="blue")

ggplot(data = obs_pred)+
  geom_line(mapping = aes(x = period, y = observation), colour="blue")+
  geom_line(mapping = aes(x = period, y = glm_prediction), colour="red")

ggplot(data = obs_pred)+
  geom_line(mapping = aes(x = period, y = observation), colour="blue")+
  geom_line(mapping = aes(x = period, y = analog_prediction), colour="red")+
  geom_line(mapping = aes(x = train_period, y = hist_pr_19882006), colour="green")

###Train downscale model
## split data into training and testing: 70% train and 30% test
   # Train set and prepare data: 70% of the entire data ie 1988:2014 

train_analogs <- downscaleTrain(obj = train_data, method = "analogs", n.analogs = 1, window = 3)
train_glm <- downscaleTrain(obj = train_data, method = "GLM")
train_nn <- downscaleTrain(obj = train_data, method = "NN", hidden = c(20,20), numepochs = 5000, batchsize = 100, output = "linear")

dim(train_analogs$pred$Data)
train_analogs$pred$Data[1:10,1,1]
## Performance assessment metrics  
length(train_predictand$Data[,1,1])
length(train_analogs$pred$Data[,1,1])
length(train_glm$pred$Data[,1,1])
length(train_nn$pred$Data[,1,1])

daboase_observation <- train_predictand$Data[1:6807,1,1]
analog_train_prediction <- train_analogs$pred$Data[1:6807,1,1]
glm_train_prediction <- train_glm$pred$Data[1:6807,1,1]
nn_train_prediction <- train_nn$pred$Data[1:6807,1,1]

train_analogs$pred$Data[1:6807,1,1] 
train_nn$pred$Data[1:6807,1,1]
train_glm$pred$Data[1:6807,1,1]

perform_analogs_train <- performance(observation = daboase_observation, prediction = analog_train_prediction)
perform_glm_train <- performance(observation = daboase_observation, prediction = glm_train_prediction)
perform_nn_train <- performance(observation = daboase_observation, prediction = nn_train_prediction)

perform_analogs_train
perform_glm_train
perform_nn_train
cor(analog_train_prediction,daboase_observation)

performance(observation = daboase_observation, prediction = hist_pr_19882006)


summary(analog_train_prediction)
summary(daboase_observation)

## Visualize and assess the performance of train prediction
train_period_character <- train_predictand$Dates[[1]]
train_period <- as.Date(train_period_character)
train_obs_pred_daboase <- data.frame(Period = train_period, Observation = daboase_observation, Downscaled = analog_train_prediction, GCM_prediction = hist_pr_19882006)
#train_obs_pred <- data.frame(train_period[1:6807], daboase_observation, analog_train_prediction,glm_train_prediction,nn_train_prediction)

View(train_obs_pred_daboase)
ggplot(data = train_obs_pred_daboase)+
  geom_point(mapping = aes(x=analog_train_prediction,y=daboase_observation), colour="blue", shape=1, size = 2)

ggplot(data = train_obs_pred)+
  geom_histogram(mapping = aes(daboase_observation), colour="blue", bins = 5, binwidth = 5)
ggplot(data = train_obs_pred)+
  geom_histogram(mapping = aes(glm_train_prediction), colour="red", bins = 5, binwidth = 5)

ggplot(data = train_obs_pred)+
  geom_line(mapping = aes(x = train_period, y = daboase_observation, colour ="Observation"))+
  geom_line(mapping = aes(x = train_period, y = analog_train_prediction, colour = "Downscaled"))+
  geom_line(mapping = aes(x = train_period, y = hist_pr_19882006, colour = "GCM prediction"))+
  labs(x= "Time (days)", y= "Daily Rainfall (mm)", title = "Training Period: Daboase Station")+
  scale_colour_manual(name = "", values = c("red","green","blue"))+
  theme(legend.position = "top", plot.title = element_text(size = 12), axis.title = element_text(size = 12))


ggplot(data = train_obs_pred)+
  geom_line(mapping = aes(x = train_period, y = daboase_observation), colour="blue")+
  geom_line(mapping = aes(x = train_period, y = hist_pr_19882006), colour="green")


ggplot(data = train_obs_pred)+
  geom_line(mapping = aes(x = train_period, y = daboase_observation), colour="blue")+
  geom_line(mapping = aes(x = train_period, y = analog_train_prediction), colour="red")+
  geom_line(mapping = aes(x = train_period, y = glm_train_prediction), colour="green")+
  geom_line(mapping = aes(x = train_period, y = nn_train_prediction), colour="black")

  # Test set and data preparation
test_predictand <- subsetGrid(y_predictand, years = 2007:2014)
test_predictors <- subsetGrid(x_predictors, years = 2007:2014)
test_data <- prepareNewData(newdata = test_predictors,data.structure = train_data)
test_analogs <- downscalePredict(newdata = test_data, model = train_analogs)

test_glm <- downscalePredict(newdata = test_data, model = train_glm)
test_nn <- downscalePredict(newdata = test_data, model = train_nn)

# Visualize and assess the performance of test prediction
test_period_character <- test_predictand$Dates[[1]]
test_period <- as.Date(test_period_character)
observation_test_daboase <- test_predictand$Data[,1,1]
analog_test_daboase <- test_analogs$Data[,1,1]
test_obs_pred_daboase <- data.frame(Period = test_period, Observation = observation_test_daboase, Downscaled = analog_test_daboase, GCM_prediction = hist_pr_test)
View (test_obs_pred_daboase)

ggplot(data = test_obs_pred_daboase)+
  geom_line(mapping = aes(x = test_period, y = observation_test_daboase, colour = "Observation"))+
  geom_line(mapping = aes(x = test_period, y = analog_test_daboase, colour = "Downscaled"))+
  geom_line(mapping = aes(x = test_period, y = hist_pr_test, colour = "GCM prediction"))+
  labs(x= "Time (days)", y= "Daily Rainfall (mm)", title = "Testing Period: Daboase Station")+
  scale_colour_manual(name = "", values = c("red","green","blue"))+
  theme(legend.position = "top", plot.title = element_text(size = 12), axis.title = element_text(size = 12))

## Combine traing and testing results into one data frame
table_daboase <- rbind(train_obs_pred_daboase,test_obs_pred_daboase)
View(table_daboase)

length(test_obs_pred_daboase$Period)
length(train_obs_pred_daboase$Period)
length(table_daboase$Period)

## Write training and testing results to file
csv_daboase <- write.csv(table_daboase,"Output/Daboase/daboasetraintest.csv")
              
## Aggregate grid to compute long term mean annual rainfall
grid_observe_train_daboase<- train_predictand
grid_observe_test_daboase <- test_predictand
grid_downscaled_train_daboase <- train_analogs$pred
grid_downscaled_test_daboase <-test_analogs

aggr_observe_train_daboase  <- aggregateGrid(grid_observe_train_daboase, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
aggr_observe_test_daboase <- aggregateGrid(grid_observe_test_daboase, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
aggr_downscaled_train_daboase <- aggregateGrid(grid_downscaled_train_daboase, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
aggr_downscaled_test_daboase <- aggregateGrid(grid_downscaled_test_daboase, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))

train_years <- as.Date(aggr_observe_train_daboase$Dates$start)
test_years <- as.Date(aggr_observe_test_daboase$Dates$start)
train_years <- year(train_years)
test_years <- year(test_years)

train_mean_annual_daboase <- data.frame(Period = train_years, Observation=aggr_observe_train_daboase$Data[,1,1], Downscaled = aggr_downscaled_train_daboase$Data[,1,1])
test_mean_annual_daboase <- data.frame(Period = test_years, Observation=aggr_observe_test_daboase$Data[,1,1], Downscaled = aggr_downscaled_test_daboase$Data[,1,1])
combined_mean_annual_daboase <- rbind(train_mean_annual_daboase,test_mean_annual_daboase)
longterm_mean_annual_daboase <- data.frame(Observation = mean(combined_mean_annual_daboase$Observation), Downscaled = mean(combined_mean_annual_daboase$Downscaled))
View(combined_mean_annual_daboase)
View(longterm_mean_annual_daboase)

write.csv(combined_mean_annual_daboase,"Output/Daboase/daboasemeanannual_pr_19882014.csv")
write.csv(longterm_mean_annual_daboase,"Output/Daboase/daboasemeanannual_pr_19882014_longeterm.csv")
## plots aggregated grids time series 
temporalPlot(aggr_observe_train_daboase,aggr_observe_test_daboase,aggr_downscaled_train_daboase,aggr_downscaled_test_daboase)
##performance assessment metrics
daboase_perform_analogs_test <- performance(observation = observation_test_daboase, prediction = analog_test_daboase)
perform_analogs_test

perform_glm_test <- performance(observation = "", prediction = "")
perform_nn_test <- performance(observation = "", prediction = "")
###Predict future climate: precipitation
   ## Import future datasets for climate scenarios: SSP1, SSP2 and SSP3 for HADGEM3_GC31-LL

## Make aggregated data for the period 2015-2100

#makeAggregatedDataset(source.dir = "future", ncml.file = "ssp126_pr_2015-2100.ncml")

## Take inventory
#invent_ssp126_pr <- dataInventory("ssp126_pr_2015-2100.ncml")
#str(invent_ssp126_pr$pr)
invent_ssp245pr <- dataInventory(ssp245_pr_file_2015_2049)
str(invent_ssp245pr)
##Load using local directory
ssp245_pr_file_2015_2049 <- "future/pr_day_HadGEM3-GC31-LL_ssp245_r1i1p1f3_gn_20150101-20491230.nc"
ssp245_pr_file_2050_2100 <- "future/pr_day_HadGEM3-GC31-LL_ssp245_r1i1p1f3_gn_20500101-21001230.nc"
ssp245_tasmin_file_2015_2049 <- "future/tasmin_day_HadGEM3-GC31-LL_ssp245_r1i1p1f3_gn_20150101-20491230.nc"
ssp245_tasmin_file_2050_2100 <- "future/tasmin_day_HadGEM3-GC31-LL_ssp245_r1i1p1f3_gn_20500101-21001230.nc"
ssp245_tasmax_file_2015_2049 <- "future/tasmax_day_HadGEM3-GC31-LL_ssp245_r1i1p1f3_gn_20150101-20491230.nc"
ssp245_tasmax_file_2050_2100 <- "future/tasmax_day_HadGEM3-GC31-LL_ssp245_r1i1p1f3_gn_20500101-21001230.nc"

str(dataInventory(ssp126_tasmax_file_2015_2049))

# Load future data for ssp scenarios  NF
ssp126_load_pr_20152049 <- loadGridData(
  dataset = ssp126_pr_file_2015_2049,
  var = "pr",
  lonLim = c(-2,-0.5),
  latLim = c(4,6),
  season = 1:12,
  years = 2015:2049
)                       ## Load future precipitation for ssp126 scenarios

ssp126_load_tasmin_20152049 <- loadGridData(
  dataset = ssp126_tasmin_file_2015_2049,
  var = "tasmin",
  lonLim = c(-2,-0.5),
  latLim = c(4,6),
  season = 1:12,
  years = 2015:2049
)                         ## Load future minimum temperature for ssp126 scenarios

ssp126_load_tasmax_20152049 <- loadGridData(
  dataset = ssp126_tasmax_file_2015_2049,
  var = "tasmax",
  lonLim = c(-2,-0.5),
  latLim = c(4,6),
  season = 1:12,
  years = 2015:2049
)                         ## Load future maximum temperature for ssp126 scenarios


# Load future data for ssp scenarios  MF and FF
ssp126_load_pr_20502100 <- loadGridData(
  dataset = ssp126_pr_file_2050_2100,
  var = "pr",
  lonLim = c(-2,-0.5),
  latLim = c(4,6),
  season = 1:12,
  years = 2050:2100
)                       ## Load future precipitation for ssp126 scenarios

ssp126_load_tasmin_20502100 <- loadGridData(
  dataset = ssp126_tasmin_file_2050_2100,
  var = "tasmin",
  lonLim = c(-2,-0.5),
  latLim = c(4,6),
  season = 1:12,
  years = 2050:2100
)                         ## Load future minimum temperature for ssp126 scenarios

ssp126_load_tasmax_20502100 <- loadGridData(
  dataset = ssp126_tasmax_file_2050_2100,
  var = "tasmax",
  lonLim = c(-2,-0.5),
  latLim = c(4,6),
  season = 1:12,
  years = 2050:2100
)                         ## Load future maximum temperature for ssp126 scenarios

## Tidy up the data for NAs and missing values
any(ssp126_load_pr_20502100$Data==missval_pr)
any(ssp126_load_tasmin_20502100$Data==missval_tasmin)
any(ssp126_load_tasmax_20502100$Data==missval_tasmax)

any(is.na(ssp126_load_pr_20502100$Data))
any(is.na(ssp126_load_tasmin_20502100$Data))
any(is.na(ssp126_load_tasmax_20502100$Data))

which(is.na(ssp126_load_pr$Data))
length(ssp126_load_pr$Data[,1,1])

##Convert future climate variables for NF
ssp126_pr_mm_20152049 <- gridArithmetics(ssp126_load_pr_20152049,86400,operator = "*")# convert kg/m2/s to mm/day
ssp126_tasmin_degC_20152049 <- gridArithmetics(ssp126_load_tasmin_20152049,273.15,operator = "-")# convert K to degC
ssp126_tasmax_degC_20152049 <- gridArithmetics(ssp126_load_tasmax_20152049,273.15,operator = "-")# convert K to degC
ssp126_pr_mm_20152049$Data[1:5]
ssp126_tasmin_degC_20152049$Data[1:5]
ssp126_tasmax_degC_20152049$Data[1:5]

##Convert future climate variables for MF and FF
ssp126_pr_mm_20502100 <- gridArithmetics(ssp126_load_pr_20502100,86400,operator = "*")# convert kg/m2/s to mm/day
ssp126_tasmin_degC_20502100 <- gridArithmetics(ssp126_load_tasmin_20502100,273.15,operator = "-")# convert K to degC
ssp126_tasmax_degC_20502100 <- gridArithmetics(ssp126_load_tasmax_20502100,273.15,operator = "-")# convert K to degC
ssp126_pr_mm_20502100$Data[1:5]
ssp126_tasmin_degC_20502100$Data[1:5]
ssp126_tasmax_degC_20502100$Data[1:5]

## Extract GCM prediction for 2015-2049
gcm_pr_daboase_20152049 <- ssp126_pr_mm_20152049$Data[,1,1]
gcm_pr_daboase_20502100 <- ssp126_pr_mm_20502100$Data[,1,1]

## Make multi grid of predictors
ssp126_multi_predictors_20152049 <- makeMultiGrid(ssp126_pr_mm_20152049, ssp126_tasmin_degC_20152049, ssp126_tasmax_degC_20152049)
ssp126_multi_predictors_20502100 <- makeMultiGrid(ssp126_pr_mm_20502100, ssp126_tasmin_degC_20502100, ssp126_tasmax_degC_20502100)
##Prepare and predict future climate:precipitation for each period

all_future_data_20152049 <-  prepareNewData(newdata = ssp126_multi_predictors_20152049,data.structure = train_data)
ssp126_future_data_20502100 <- prepareNewData(newdata = ssp126_multi_predictors_20502100,data.structure = train_data)

all_future_analogs_20152049 <- downscalePredict(newdata = all_future_data_20152049, model = train_analogs)
ssp126_future_analog_model_20502100 <- downscalePredict(newdata = ssp126_future_data_20502100, model = train_analogs)

##Visualization, aggregation and plotting 
future_period_character <- all_future_analogs_20152049$Dates[[1]]
ssp1_future_Period_character_20502100 <- ssp126_future_analog_model_20502100$Dates[[1]]

future_period_20152049 <- as.Date(future_period_character)
ssp1_future_period_20502100 <- as.Date(ssp1_future_Period_character_20502100)

ssp1_future_downscaled_20502100 <- ssp126_future_analog_model_20502100$Data[,1,1]
ssp1_future_downscaled_20502100_df <- data.frame(Period = ssp1_future_period_20502100, Downscaled = ssp1_future_downscaled_20502100, GCM_prediction = gcm_pr_daboase_20502100)
View(ssp1_future_downscaled_20502100_df)

ggplot(data = ssp1_future_downscaled_20502100_df)+
  geom_line(mapping = aes(x = Period, y = Downscaled), colour="red")+
  geom_line(mapping = aes(x = Period, y = GCM_prediction), colour="green")

## Write future prediction results to file: 2015-2049
write.csv(ssp1_future_downscaled_20502100_df,"Output/Daboase/daboasefuture_20502100.csv")

##Partition predicted future data into near, mid and far future for aggregation
near_future_daboase_20232049 <- subsetGrid(all_future_analogs_20152049, years = 2023:2049)
mid_future_daboase_20502074 <- subsetGrid(subsetGrid(ssp126_future_analog_model_20502100, years = 2050:2074))
far_future_daboase_20752100 <- subsetGrid(subsetGrid(ssp126_future_analog_model_20502100, years = 2075:2100))

## Aggregate grid to compute long term mean annual rainfall

aggr_near_future_daboase_20232049  <- aggregateGrid(near_future_daboase_20232049, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
aggr_mid_future_daboase_20502074 <- aggregateGrid(mid_future_daboase_20502074, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))
aggr_far_future_daboase_20752100 <- aggregateGrid(far_future_daboase_20752100, aggr.m = list(FUN = "sum"),aggr.y = list(FUN = "sum"))

nf_ssp1_20232049 <- as.Date(aggr_near_future_daboase_20232049$Dates$start)
mf_ssp1_20502074 <- as.Date(aggr_mid_future_daboase_20502074$Dates$start)
ff_ssp1_20752100 <- as.Date(aggr_far_future_daboase_20752100$Dates$start)

nf_ssp1_years <- year(nf_ssp1_20232049) 
mf_ssp1_years <- year(mf_ssp1_20502074) 
ff_ssp1_years <- year(ff_ssp1_20752100) 

ssp1_nf_daboase_20232049 <- aggr_near_future_daboase_20232049$Data[,1,1]
ssp1_mf_daboase_20502074 <- aggr_mid_future_daboase_20502074$Data[,1,1]
ssp1_ff_daboase_20752100 <- aggr_far_future_daboase_20752100$Data[,1,1]

nf_ssp1_daboase_20232049 <- data.frame(Period = nf_ssp1_years,Downscaled = ssp1_nf_daboase_20232049)
mf_ssp1_daboase_20502074 <- data.frame(Period = mf_ssp1_years,Downscaled = ssp1_mf_daboase_20502074)
ff_ssp1_daboase_20752100 <- data.frame(Period = ff_ssp1_years,Downscaled = ssp1_fff_daboase_20752100)



View(nf_ssp1_daboase_20232049)   
View(mf_ssp1_daboase_20502074)
View(ff_ssp1_daboase_20752100)

combined_mean_annual_daboase <- rbind(train_mean_annual_daboase,test_mean_annual_daboase)
ssp1_nf_mf_ff_mar_daboase_20232100 <- rbind(nf_ssp1_daboase_20232049,mf_ssp1_daboase_20502074,ff_ssp1_daboase_20752100)

View(combined_mean_annual_daboase)
View(ssp1_nf_mf_ff_mar_daboase_20232100)

ssp1_longterm_future_mar_20232100_daboase <- data.frame(nf20232049=mean(ssp1_nf_daboase_20232049),mf20502074=mean(ssp1_mf_daboase_20502074),ff20752100=mean(ssp1_ff_daboase_20752100))
View(ssp1_longterm_future_mar_20232100_daboase)

write.csv(combined_mean_annual_daboase,"Output/Daboase/daboasemeanannual_pr_19882014.csv")
write.csv(longterm_mean_annual_daboase,"Output/Daboase/daboasemeanannual_pr_19882014_longeterm.csv")

write.csv(ssp1_nf_mf_ff_mar_daboase_20232100,"Output/Daboase/daboasefuturemeanannual_pr_20232100.csv")
write.csv(ssp1_longterm_future_mar_20232100_daboase,"Output/Daboase/daboasefuturemeanannual_pr_20232100_longeterm.csv")

## Visualize time series of mean annual rainfall
temporalPlot(aggr_near_future_daboase_20232049,aggr_mid_future_daboase_20502074,aggr_far_future_daboase_20752100)

## Perform simulation 
simulate_model(y=train_predictand, x=train_predictors,nsim = 20,sim = TRUE,rangeanalog = c(1,30),rangefold = c(10,10),fun.analog = c("wmean","wmean"))


####################################Iteration over train model
############################Test climateTrain function
oblon <- list(c(-1.8,-0.8), c(-1.7,-0.7),c(-2.2,-1.2), c(-1.9,-0.9), c(-2.3,-1.3), c(	-0.9,-0.1), c(-2.6,-1.6), c(-2.7,-1.7), c(-2.6,-1.6), c(-3.2,-2.2), c(-2.3,-1.3), c(-2.1,-1.1), c(-1.6,-0.6), c(-1.3,-0.3), c(-2.0,-0.001))
hilon <- list(c(-2,-0.5), c(-3.3,-0.7), c(-3.8,-0.1), c(-3.5,-0.5), c(-3.9,-0.9), c(-2.5,-0.1), c(-3.0,-1.8), c(-3.0,-1.8), c(-2.8,-1.8), c(-3.2,-1.8), c(-2.5,-0.8), c(-2.2,-0.8), c(-2.0,-0.8), c(-2.0,-0.4), c(-2.0,-0.001))	


oblat <- list(c(4,5.5), c(4.6,6.1), c(4.6,6.1), c(5.3,6.8), c(5.9,7.4), c(5.4,6.9), c(3.9,5.4), c(4.5,6), c(5.2,6.7), c(9.7,11.2), c(	9,10.5), c(10,11.5), c(	9.6,11.1), c(10,11.5), c(10,11.5))
hilat <- list(c(4,5.5), c(3.6,7.6), c(3.6,7.6	), c(4.3,8.3), c(4.9,8.9), c(4.4,8.4), c(2.9,6.9), c(3.5,7.5), c(4.2,8.2), c(8.7,12.7), c(8,12), c(9,13), c(8.6,12.6), c(9,13), c(9,13))

statFiles <- c("Input/Daboase.nc","Input/Twifo.nc","Input/Dunkwa.nc","Input/upb01.nc","Input/upb02.nc","Input/upb03.nc","Input/ab01.nc","Input/ab02.nc","Input/ab03.nc","Input/uw01.nc","Input/uw02.nc","Input/uw03.nc","Input/ue01.nc","Input/ue02.nc","Input/ue03.nc")
statFiles[[1]]
###data frame
latlon <- data.frame(obsLon=oblon,obsLat=oblat,histLon=hilon,histLat=hilat)
View(latlon)
write.csv(latlon,"stationlatlon.csv")

##Test function
for_climTrain <- climateTrain(statFiles[15], hist_pr_file,hist_tasmin_file,hist_tasmax_file,obsLon = oblon[[15]],obsLat = oblat[[15]], histLon = hilon[[15]], histLat=hilat[[14]], missValue = 1e+20, period = c(1988,2014), n_random = c(1,20), window_random = c(1,10), n_sim = 20, rsq = 0.001, retConditon = 2, replace = TRUE)
for_climTrain

## iterate model
train_data <- vector("list", length(statFiles))
for (i in seq(1:length(statFiles))) {
  ##Start
  print(paste0("Starting Run", " ",i," ", "for", " ", statFiles[i], " ", "Station"))
  
  train_data[[i]] <- climateTrain(statFiles[i], hist_pr_file,hist_tasmin_file,hist_tasmax_file,obsLon = oblon[[i]],obsLat = oblat[[i]], histLon = hilon[[i]], histLat = hilat[[i]], missValue = 1e+20, period = c(1988,2014), n_random = c(1,20), window_random = c(1,10), n_sim = 20, rsq = 0.001, retConditon = 2, replace = TRUE)
  
  ##End
  print(paste0("successfully completed run", " ",i," ", "for", " ", statFiles[i], " ", "Station"))
}
length(train_analogs)
length(train_data)
train_data[[1]]
###Write aggregated results to file
for (i in seq(1:length(fit_mod_aggr))) {
  
  write.csv(fit_mod_aggr[[i]],paste0("Output/",obsNames[i],".csv"))
}

####### viewing results
fit_mod[[2]]
fit_mod[[1]]$pred$Data[1:10,1,1]
fit_mod[[2]]$pred$Data[1:10,1,1]

dab=fit_mod[1]

############################################ sim end
#####################################################dd
### Test functionality of climatePredict function

mod_predict <- climatePredict(trainData = train_data[[1]], trainModel = train_analogs[[1]], histLon = hilon[[1]], histLat = hilat[[1]], gcmPr = ssp585_pr_file_2015_2049, gcmTasmin = ssp585_tasmin_file_2015_2049,gcmTasmax = ssp585_tasmax_file_2015_2049, period = c(2015,2049), retConditon = 1)
mod_predict

##### Iterate future model prediction
length(train_data)
train_analogs

mod_ssp245_aggr <- vector("list", length(train_analogs))
for (i in seq(1:length(train_analogs))) {
  ##Start
  print(paste0("Starting Run", " ",i," ", "for", " ", obsNames[i], " ", "Station"))
  
  mod_ssp245_aggr[[i]] <- climatePredict(trainData = train_data[[i]], trainModel = train_analogs[[i]], histLon = hilon[[i]], histLat = hilat[[i]], gcmPr = ssp245_pr_file_2050_2100, gcmTasmin = ssp245_tasmin_file_2050_2100,gcmTasmax = ssp245_tasmax_file_2050_2100, period = c(2050,2100), retConditon = 1)
  
  ##End
  print(paste0("successfully completed run", " ",i," ", "for", " ", statFiles[i], " ", "Station"))
}
#####################################################writing
##Write results to file 
mod_ssp245_aggr[[14]]
for (i in seq(1:length(mod_ssp245_aggr))) {
  
  write.csv(mod_ssp245_aggr[[i]],paste0("Output/ssp245/",obsNames[i],".csv"))
  
}
mod_ssp245_aggr[[1]]$Downscaled[1:25]
mod_ssp245_aggr[[1]]$Downscaled
## Write long mean annual
##Calculate mean for mid future
aggr_mean_ff <- vector("list", length(mod_ssp245_aggr))
for (i in seq(1:length(mod_ssp245_aggr))) {
  aggr_mean_ff[[i]] <- mean(mod_ssp245_aggr[[i]]$Downscaled[26:51]) 
  
}
      names(aggr_mean_nf) <- obsNames
      names(aggr_mean_mf) <- obsNames
      names(aggr_mean_ff) <- obsNames
      
  write.csv(aggr_mean_mf,paste0("Output/ssp245/longterm_mf20502074.csv"))
  write.csv(aggr_mean_ff,paste0("Output/ssp245/longterm_ff20752100.csv"))
  write.csv(aggr_mean_nf,paste0("Output/ssp245/longterm_nf20152049.csv"))
### Test write_netcdf function
dir <- "C:/Users/HP/Documents/WREM PhD/PhD Project/First Level Datasets and maps/Secondary datasets/Climate/observation/stationSeriesFormat.csv"
lon <- "C:/Users/HP/Documents/WREM PhD/PhD Project/First Level Datasets and maps/Secondary datasets/Climate/observation/longitude.csv"
lat <- "C:/Users/HP/Documents/WREM PhD/PhD Project/First Level Datasets and maps/Secondary datasets/Climate/observation/latitude.csv"


lpDir <- "C:/Users/HP/Documents/WREM PhD/PhD Project/First Level Datasets and maps/Secondary datasets/Climate/created ncdf files/"
created_file <- write_netcdf(dataFile = dir,lon = lon, lat = lat, loopDir = lpDir, retCondition = 3)
created_file

##############iterate climate model 
pred <- iterate_climateModel(stationFile = statFiles,histPr = hist_pr_file,histTasmin = hist_tasmin_file,histTasmax = hist_tasmax_file,statLon = oblon,statLat = oblat,modLon = hilon,modLat = hilat,trainPeriod = c(1988,2014),gcmPr = ssp245_pr_file_2015_2049,gcmTasmin = ssp245_tasmin_file_2015_2049,gcmTasmax = ssp245_tasmax_file_2015_2049, n_random = c(1,20), window_random = c(1,15), n_sim = 20, retConditon = 3,rsq = 0.3)
pred[15]

#n_sim=2, n_random=c(1,10), window_random=c(1,7), replace = FALSE, rsq = 0.5,
###################################### iterating climateTrain
obsD <- "C:/Users/HP/Documents/WREM PhD/PhD Project/First Level Datasets and maps/Secondary datasets/Climate/created ncdf files/"
obsNames <- c("Daboase","Twifo","Dunkwa","upb01","upb02","upb03","ab01","ab02","ab03","uw01","uw02","uw03","ue01","ue02","ue03")
histF <- c("pr_day_HadGEM3-GC31-LL_historical_r1i1p1f3_gn_19500101-20141230.nc","tasmin_day_HadGEM3-GC31-LL_historical_r1i1p1f3_gn_19500101-20141230.nc","tasmax_day_HadGEM3-GC31-LL_historical_r1i1p1f3_gn_19500101-20141230.nc")
lon <- "C:/Users/HP/Documents/WREM PhD/PhD Project/First Level Datasets and maps/Secondary datasets/Climate/observation/longitude.csv"
lat <- "C:/Users/HP/Documents/WREM PhD/PhD Project/First Level Datasets and maps/Secondary datasets/Climate/observation/latitude.csv"

########Extract observe data from1988to 2021

## Load predictands




############Testing function
obs_meanAnnual <- extract_station(statFiles = statFiles, obslon = oblon, obslat = oblat, period = c(1988,2021))
obs_meanAnnual
names(obs_meanAnnual) <- obsNames
write.csv(obs_meanAnnual, "Output/longterm19882021.csv")
obs_annual <- extract_station(statFiles = statFiles, obslon = oblon, obslat = oblat, period = c(1988,2021), retCondition = TRUE)
obs_annual[[2]]  

############### GIS processing of long term mean annual rainfall of 15 stations
rainfall_file <- "Output/Long term mean annual rainfall.csv"
mean_rainfall <- read.csv(rainfall_file, header = TRUE)
class(mean_rainfall)

## Convert data frame to vector layer
sf_rainfall <- st_as_sf(mean_rainfall, coords=c("Lon","Lat"), crs=4326)
up_prakob <- st_read("C:/Users/HP/Documents/WREM PhD/PhD Project/First Level Datasets and maps/Secondary datasets/Study area map/Study area/study_area.shp")
up_prakob$Area
class(up_prakob)
class(sf_rainfall)
View(sf_rainfall)
plot(sf_rainfall)

plot(st_geometry(up_prakob), border= "red",)
plot(sf_rainfall[, "SSP245_FF"], col="green", fill = "blue", add = TRUE)
mapview(up_prakob, zcol = "Area")
leaflet(st_geometry(sf_rainfall))
st_crs(sf_rainfall)
st_crs(up_prakob)

### write sf layer to file
st_write(sf_rainfall,"Output/meanAR.shp")
