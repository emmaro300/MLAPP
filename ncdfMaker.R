## This module contains functions for creating netcdf files 
##Load packages
library(ncdf4)
library(lubridate)
library(loadeR)


## This function writes netcdf files into a specified directory using csv data

write_netcdf <- function(dataFile, station_name, lon, lat,loopDir, retCondition = 1){
  ## Create global variables 
  retValue <- NULL
  today <- today()
  today <- as.character(today)
  ## Import the csv file and reshape matrix into vector
  data_csv <- read.csv(dataFile, header = TRUE)
  data_length <- length(data_csv[[3]])
  data_column <- cbind(data_csv[[3]],data_csv[[3]],data_csv[[3]],data_csv[[3]])
  data_vector <- as.vector(data_column)
 
  data_matrix <- matrix(data_vector,nrow = 4,ncol=12419,byrow = TRUE)
  data_vector2 <- as.vector(data_matrix)
  
  #Extract and define pr variable
  array_data <- array(data_vector2, dim = c(2,2,data_length))
  print(head(array_data[1,1,],10))
  print(dim(array_data)) 
  print(class(array_data))
  
  #Import,Extract and define lon variable
  longitude <-read.csv(lon, header=TRUE)
  
  longitude <- as.matrix(longitude)
  lon_chc <- longitude[3,4:5]
  lon <- as.numeric(lon_chc)
  station_longitude <- lon
  
  array_longitude <- as.array(station_longitude)
 
  #Import, Extract and define lat variable
  latitude <-read.csv(lat, header=TRUE)
  
  latitude <- as.matrix(latitude)
  lat_chc <- latitude[3,4:5]
  lat <- as.numeric(lat_chc)
  station_latitude <- lat
  array_latitude <- as.array(station_latitude)
  
  #Extract and define time variable
  time_elapsed <- data_csv[[2]]
  time_length <- length(time_elapsed)
  time_column <- cbind(time_elapsed,time_elapsed,time_elapsed,time_elapsed)
  time_vector <- as.vector(time_column)
  time_matrix <- matrix(time_vector,nrow = 4,ncol=12419,byrow = TRUE)
  time_vector2 <- as.vector(time_matrix)
  
  array_time <- array(time_vector2, dim = c(2,2,time_length))
  
  ### Define netcdf dimensions
  longitude_dimension <- ncdim_def("lon","degrees-east",vals = as.double(station_longitude), longname = "WGS84 GCS easting")
  latitude_dimension <- ncdim_def("lat","degrees-north",vals = as.double(station_latitude), longname = "WGS84 GCS northing")
  time_dimension <- ncdim_def("time","days since 1988-01-01",vals = time_elapsed, longname = "leap days since 1988-01-01",calendar = "standard")
  
  ###Define netcdf variables template    
  pr_var <- ncvar_def("pr","mm",dim = list(longitude_dimension,latitude_dimension,time_dimension),missval = 1e20,longname = "daily precipitation at Daboase met station",prec = "float")
  lon_var <- ncvar_def("lon_bnds","degrees-east",dim = longitude_dimension,missval = NULL,longname = "WGS84 GCS easting",prec = "double")
  lat_var <- ncvar_def("lat_bnds","degrees-north",dim = latitude_dimension,missval = NULL,longname = "WGS84 GCS northing",prec = "double")
  time_var <- ncvar_def("time_bnds","days since 1988-01-01",dim = list(longitude_dimension,latitude_dimension,time_dimension),missval = NULL,longname = "leap days since 1988-01-01",prec = "integer")
  
  ### Create netcdf file template
  ncfile <- paste0(loopDir,names(data_csv[[3]]),".nc")
  nc_template <- nc_create(ncfile,vars = list(pr_var,lon_var,lat_var,time_var))
 
   ###Put variables values into file
  ncvar_put(nc_template,pr_var,array_data)
  ncvar_put(nc_template,lon_var,array_longitude)
  ncvar_put(nc_template,lat_var,array_latitude)
  ncvar_put(nc_template,time_var,array_time)
  
  ###Put additional attributes in netcdf file
  ncatt_put(nc_template,lon_var,attname = "Axis",attval = "Ln")
  ncatt_put(nc_template,lat_var,attname = "Axis",attval = "Lt")
  
  ###Put global attributes in file
  ncatt_put(nc_template,0,attname = "Title",attval = paste0("Gauged daily rainfall:", " ", "Daboase Station"))
  ncatt_put(nc_template,0,attname = "Prepared on",attval = today)
  ncatt_put(nc_template,0,attname = "Prepared by:",attval = "Emmanuel Arthur")
  ncatt_put(nc_template,0,attname = "Project",attval = "Risk_Based Groundwater Framework:PhD Project")
  
  ###Close nc file
  nc_close(nc_template)
  
  ##Verify if file created and is as expected
  if(retCondition == 1){
    
    station_inventory <- dataInventory(ncfile)
    str_inventory <- str(station_inventory)
    print("File successfully created and inventory saved to memory")
    
    station_pr <- nc_open(ncfile)
    print(station_pr)
    retValue <- str_inventory
  }
  
  if(retCondition == 2){
    station_pr <- nc_open(ncfile)
    
    pr_data <- ncvar_get(station_pr,"pr")
    lon_data <- ncvar_get(station_pr,"lon_bnds")
    lat_data <- ncvar_get(station_pr,"lat_bnds")
    time_data <- ncvar_get(station_pr,"time_bnds")
    
    time <- time_data[1,1,1:10]
    pr <- pr_data[1,1,1:10]
    prTime_df <- data.frame(time,pr)
    retValue <- prTime_df
  }
  
  
  ##Iteration if multiple data variables 
  if(retCondition==3){
    
   ## Create variables 
    
   precip <- vector("list", (length(data_csv)-2))
    ## get all data variables from csv files
    for (i in seq(3,length(data_csv))) {
      data_csv <- read.csv(dataFile, header = TRUE)
      data_length <- length(data_csv[[i]])
      data_column <- cbind(data_csv[[i]],data_csv[[i]],data_csv[[i]],data_csv[[i]])
      data_vector <- as.vector(data_column)
      
      data_matrix <- matrix(data_vector,nrow = 4,ncol=12419,byrow = TRUE)
      data_vector2 <- as.vector(data_matrix)
      
      #Extract and define pr variable
      array_data <- array(data_vector2, dim = c(2,2,data_length))
      print(head(array_data[1,1,],10))
      print(dim(array_data)) 
      print(class(array_data))
      
      #Extract and define lon variable

      lon_chc <- longitude[i,4:5]
      lon <- as.numeric(lon_chc)
      station_longitude <- lon
      
      array_longitude <- as.array(station_longitude)
      
      #Extract and define lat variable

      lat_chc <- latitude[i,4:5]
      lat <- as.numeric(lat_chc)
      station_latitude <- lat
      array_latitude <- as.array(station_latitude)
      
      #Extract and define time variable
      time_elapsed <- data_csv[[2]]
      time_length <- length(time_elapsed)
      time_column <- cbind(time_elapsed,time_elapsed,time_elapsed,time_elapsed)
      time_vector <- as.vector(time_column)
      time_matrix <- matrix(time_vector,nrow = 4,ncol=12419,byrow = TRUE)
      time_vector2 <- as.vector(time_matrix)
      
      array_time <- array(time_vector2, dim = c(2,2,time_length))
      
      ### Define netcdf dimensions
      longitude_dimension <- ncdim_def("lon","degrees-east",vals = as.double(station_longitude), longname = "WGS84 GCS easting")
      latitude_dimension <- ncdim_def("lat","degrees-north",vals = as.double(station_latitude), longname = "WGS84 GCS northing")
      time_dimension <- ncdim_def("time","days since 1988-01-01",vals = time_elapsed, longname = "leap days since 1988-01-01",calendar = "standard")
      
      ###Define netcdf variables template    
      pr_var <- ncvar_def("pr","mm",dim = list(longitude_dimension,latitude_dimension,time_dimension),missval = 1e20,longname = paste0("daily precipitation at", " ", names(data_csv[i])," ", "climate station"), prec = "float")
      lon_var <- ncvar_def("lon_bnds","degrees-east",dim = longitude_dimension,missval = NULL,longname = "WGS84 GCS easting",prec = "double")
      lat_var <- ncvar_def("lat_bnds","degrees-north",dim = latitude_dimension,missval = NULL,longname = "WGS84 GCS northing",prec = "double")
      time_var <- ncvar_def("time_bnds","days since 1988-01-01",dim = list(longitude_dimension,latitude_dimension,time_dimension),missval = NULL,longname = "leap days since 1988-01-01",prec = "integer")
      
      ### Create netcdf file template
      nc_file <- paste0(loopDir,names(data_csv[i]),".nc")
      nc_template <- nc_create(nc_file,vars = list(pr_var,lon_var,lat_var,time_var))
      
      ###Put variables values into file
      ncvar_put(nc_template,pr_var,array_data)
      ncvar_put(nc_template,lon_var,array_longitude)
      ncvar_put(nc_template,lat_var,array_latitude)
      ncvar_put(nc_template,time_var,array_time)
      
      ###Put additional attributes in netcdf file
      ncatt_put(nc_template,lon_var,attname = "Axis",attval = "Ln")
      ncatt_put(nc_template,lat_var,attname = "Axis",attval = "Lt")
      
      ###Put global attributes in file
      ncatt_put(nc_template,0,attname = "Title",attval = paste0("Gauged daily rainfall:", " ", names(data_csv[i])))
      ncatt_put(nc_template,0,attname = "Prepared on",attval = today)
      ncatt_put(nc_template,0,attname = "Prepared by:",attval = "Emmanuel Arthur")
      ncatt_put(nc_template,0,attname = "Project",attval = "Risk_Based Groundwater Framework:PhD Project")
      
      ##
      print(paste0(names(data_csv[i]),".nc", " ","file successfully created on", " ", today))
    
      
      ### take inventory of created files 
      station_inventory <- dataInventory(nc_file)
      str_inventory <- str(station_inventory)
      print("File successfully created and inventory saved to memory")
      
      station_pr <- nc_open(nc_file)
      print(station_pr)
      
      ## Open created nc files and save head to data frame
      station_pr <- nc_open(nc_file)
      
      pr_data <- ncvar_get(station_pr,"pr")
      lon_data <- ncvar_get(station_pr,"lon_bnds")
      lat_data <- ncvar_get(station_pr,"lat_bnds")
      time_data <- ncvar_get(station_pr,"time_bnds")
      
      time <- time_data[1,1,1:10]
      precip[[i]] <- pr_data[1,1,1:10]
      
      ###Close nc file
      nc_close(nc_template)
    }
   retValue <- precip
  }
  return(retValue)
}


