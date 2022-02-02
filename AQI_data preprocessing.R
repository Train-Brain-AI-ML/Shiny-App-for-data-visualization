rm(list=ls()) ######## to clear the environment
############### import the data######
city_day_data <- read.csv("AQI_Data/city_day.csv")
dim(city_day_data)
head(city_day_data)
#################################### checking null values in each column#####
sum(is.na(city_day_data$City))
sum(is.na(city_day_data$Date))
sum(is.na(city_day_data$PM2.5))
sum(is.na(city_day_data$PM10))
sum(is.na(city_day_data$NO))
sum(is.na(city_day_data$NO2))
sum(is.na(city_day_data$NOx))
sum(is.na(city_day_data$NH3))
sum(is.na(city_day_data$CO))
sum(is.na(city_day_data$SO2))
sum(is.na(city_day_data$O3))
sum(is.na(city_day_data$Benzene))
sum(is.na(city_day_data$Toluene))
sum(is.na(city_day_data$Xylene))
sum(is.na(city_day_data$AQI))
sum(is.na(city_day_data$AQI_Bucket))
sum(is.na(city_day_data))

###############################################################
##############################drop null values #################
city_day_data <- na.omit(city_day_data)
################################################################
############################ data save in new csv file ############

write.csv(city_day_data, file = "AQI_Data/clean_city_day_data.csv")
dim(city_day_data)
sum(is.na(city_day_data))
head(city_day_data)
str(city_day_data)

