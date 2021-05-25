# Reading the dataset
require(curl)
data <- read.csv(curl("https://raw.githubusercontent.com/CloudDemo/traffic-analysis/master/basic-features-2.csv"))

head(data) # List the first 6 rows 
colnames(data) # Shows column names
dim(data) # dimentions of the data 

# Data statistics
summary(data)

# Handeling missing data
is.na(data) # # returns TRUE of elements in 'data' are missing
newdata <- na.omit(data) # create new dataset without missing data
# Replace missing values with column mean
# Instead of Mean can be used Median, Standard Deviation
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

# Removing duplicate rows
duplicated(x) # Find the position
x[!duplicated(x)]

# Another method for removing duplicate rows using 'dplyr' package
install.packages("dplyr") # Install
library("dplyr") # Load

distinct(my_data) # Remove duplicate based on all columns
distinct(my_data, '') # Remove duplicated rows based on ''
