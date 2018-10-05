setwd("~/B/VU/2018-2019/AML/Kaggle")

library(data.table)

airports <- data.table::fread("airports.csv")
sample_submission <- data.table::fread("sample_submission.csv")
test <- data.table::fread("test.csv")
train <- data.table::fread("train.csv")
weather <- data.table::fread("weather.csv")

weather$time_hour <- as.POSIXct(weather$time_hour, format = "%Y-%m-%d %H:%M:%S")

train$sched_dep_time <- floor(ifelse(train$sched_dep_time %% 100 >= 30,train$sched_dep_time+100,train$sched_dep_time)/100)*100
train$sched_arr_time <- floor(ifelse(train$sched_arr_time %% 100 >= 30,train$sched_arr_time+100,train$sched_arr_time)/100)*100
train$sched_arr_time <- ifelse(train$sched_arr_time == 2400,0000,train$sched_arr_time)
train$sched_dep_time <- ifelse(train$sched_dep_time == 2400,0000,train$sched_dep_time)

train$dep_hour <- sub("(.*)(.{2})", "\\1:\\2:00", sprintf("%04d", train$sched_dep_time))
train$arr_hour <- sub("(.*)(.{2})", "\\1:\\2:00", sprintf("%04d", train$sched_arr_time))
train$dep_date <- as.POSIXct(paste0(train$year, "-", sprintf("%02d",train$month), "-", sprintf("%02d",train$day), " ", train$dep_hour), format = "%Y-%m-%d %H:%M:%S")
train$arr_date <- as.POSIXct(paste0(train$year, "-", sprintf("%02d",train$month), "-", sprintf("%02d",train$day), " ", train$arr_hour), format = "%Y-%m-%d %H:%M:%S")

temp_weather <- weather[,c("origin","temp", "dewp", "humid", "wind_dir", 
                           "wind_speed", "wind_gust", "precip", "pressure", 
                           "visib", "time_hour")]

data.table::setnames(temp_weather,c("origin","dep_temp", "dep_dewp", "dep_humid", "dep_wind_dir", 
                                    "dep_wind_speed", "dep_wind_gust", "dep_precip", "dep_pressure", 
                                    "dep_visib", "time_hour"))
train2 <- merge(train,temp_weather,by.x = c("dep_date","origin"), by.y = c("time_hour","origin") ,all.x = TRUE)
# data.table::setnames(temp_weather,c("origin","arr_temp", "arr_dewp", "arr_humid", "arr_wind_dir", 
#                                     "arr_wind_speed", "arr_wind_gust", "arr_precip", "arr_pressure", 
#                                     "arr_visib", "time_hour"))
# train2 <- merge(train2,temp_weather, by.x = c("arr_date","dest"), by.y = c("time_hour","origin") ,all.x = TRUE)
rm(temp_weather)

temp_airports <- airports[,c("faa", "name", "lat", "lon", "alt", "tz", "dst", "tzone")]

data.table::setnames(temp_airports,c("faa", "dep_name", "dep_lat", "dep_lon", "dep_alt", "dep_tz", "dep_dst", "dep_tzone"))
train3 <- merge(train2,temp_airports, by.x = "origin", by.y = "faa" ,all.x = TRUE)
data.table::setnames(temp_airports,c("faa", "arr_name", "arr_lat", "arr_lon", "arr_alt", "arr_tz", "arr_dst", "arr_tzone"))
train3 <- merge(train3,temp_airports, by.x = "dest", by.y = "faa" ,all.x = TRUE)

write.csv(train3,"train_full.csv")

