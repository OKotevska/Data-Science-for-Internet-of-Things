# install the required packages
Needed <- c("Hmisc", "timeDate", "plyr", "ggplot2", "corrplot", "arules", "arulesViz", "stringr")
install.packages(Needed, dependencies = TRUE)

# set up working directory
setwd("C:\\Users\\onk3\\OD\\OneDrive - National Institute of Standards and Technology (NIST)\\code\\articles\\article-event-model\\data\\")

# load the data
mydat <- read.csv("Pedestrian_Safety-2015.csv", stringsAsFactors = FALSE)
#mydat <- read.csv("backup-datasets\\data-for-experiments-traffic\\pedestrian-safety-data-sets\\pedestrain-safety-2015-all.csv",stringsAsFactors = FALSE)

# libraries
library(Hmisc)
library(timeDate)
library(plyr)
library(ggplot2)
library(corrplot)
library(arules)
library(arulesViz)
library(stringr)

names(mydat)
dim(mydat)
summary(mydat$City)
attach(mydat)
describe(mydat) # shows statistics for data
 
# PREPROCESSING
mydat$Date = as.Date(mydat$date, "%m/%d/%Y")
mydat$Weekdays <- weekdays(mydat$Date)
mydat$Month <- strftime(mydat$Date,"%m")
mydat$Year <- strftime(mydat$Date,"%Y")
mydat$Day <- strftime(mydat$Date, "%d")
mydat$DateNumeric <- as.numeric(mydat$Date)
# make category for each hour
for (i in 1:dim(mydat)[1]) {
  hour = strftime(strptime(mydat[i,4], format="%H:%M"),"%H")
  mydat[i,13] = hour
}
mydat$Hour = as.numeric(mydat[,13])
heatmap_data = heatmap_data[heatmap_data$zip_code != '',]   # remove records with empty city name
heatmap_data = heatmap_data[complete.cases(heatmap_data), ] # remove n/a

# filter the columns
a = mydat[,c(5,27)] # charge and description
mydat = mydat[,c(4,5,6,40,39,38,37,34,31,25)] 

# filter by state
mydat = mydat[mydat$State == "MD",]

# filter by day
mydatDay = mydat[mydat$Date == "2016-01-22",]

# filter by charge
mydatCharge = mydat[mydat$Charge == "21-801(a)",]

# filter by location
myloc = mydat[mydat$address == "8434 Colesville Rd, Silver Spring, MD 20910, USA",]
myloc = myloc[,c(1,2,5,9,23,24,32,43)]

# filter by traffic violation category
myCat = mydat[mydat$V43 == "SPEED RESTRICTIONS",]

# filter by zip code
myZip = mydat[mydat$zip_code == "20814",]

# filter by cities that don't belong to Montgomery County
mydatMC = mydat[mydat$City != "Braddock Heights" & mydat$City != "Chillum" & mydat$City != "Columbia" &
                 mydat$City != "East Riverdale" & mydat$City != "Frederick" & mydat$City != "Glenn Dale" &
                 mydat$City != "Green Valley" & mydat$City != "Highland" & mydat$City != "Hughesville" &
                 mydat$City != "Jessup" & mydat$City != "Langley Park" & mydat$City != "Linganore" &
                 mydat$City != "Lowes Island" & mydat$City != "Milford Mill" & mydat$City != "Mount Airy" &
                 mydat$City != "Mount Pleasant" & mydat$City != "Myersville" & mydat$City != "New Carrollton" &
                 mydat$City != "Ocean Grove" & mydat$City != "Peppermill Village" & mydat$City != "South Laurel" &
                 mydat$City != "Spring Grove" & mydat$City != "Urbana" & mydat$City != "Washington, D.C." &
                 mydat$City != "West Laurel",]

# filter by cities that we are interested
bycities = mydatMC[mydatMC$City == "Germantown" | mydatMC$City == "Silver Spring" 
              | mydatMC$City == "Rockville" | mydatMC$City == "Bethesda" | mydatMC$City == "Gaithersburg" 
              | mydatMC$City == "Potomac" | mydatMC$City == "North Bethesda"
              | mydatMC$City == "Montgomery Village" | mydatMC$City == "North Potomac"
              | mydatMC$City == "Chevy Chase", ]
attach(bycities)

# filter by zipcode
myZip = mydat[mydat$zip_code == "20910" | mydat$zip_code == "20814" 
                   | mydat$zip_code == "20906" | mydat$zip_code == "20874" 
                   | mydat$zip_code == "20877" | mydat$zip_code == "20878"
                   | mydat$zip_code == "20850" | mydat$zip_code == "20902"
                   | mydat$zip_code == "20886" | mydat$zip_code == "20876", ]
attach(myZip)

#Wheaton,  Potomac;  Onley, North Potomac; Takoma Park, Chevy Chase, Burtonsville, Poolesville, Clarksburg, Damascus
# statistics

# VISUALIZATION
# number of traffic violations per day
daily <- as.data.frame(table(myZip$Date))
names(daily) <- c('Weekdays', 'zip_code', 'Freq')
daily$Day <- as.Date(daily$Day)
ggplot(daily, aes(x = Day, y = Freq)) + geom_line()

### Sorting the weekdays per month
remove(daily)
daily <- as.data.frame(table(mydatMC$Weekdays, mydatMC$Month))
names(daily) <- c('Weekdays', 'Month', 'Freq')
daily$Month <- as.numeric(as.character(daily$Month))
daily$Month <- factor(daily$Month, ordered = TRUE, 
                         levels = c('3','4', '5', '6', '7', '8', '9', '10', '11', '12','1', '2'))
daily$Weekdays <- factor(daily$Weekdays, ordered = TRUE, 
                               levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
#Plotting the number of crimes each day (line graph)
ggplot(daily, aes(x = Month, y = Freq)) + geom_line(aes(group = Weekdays, color = Weekdays)) + xlab('Month') + ylab('Total number of traffic violation events') 
ggplot(daily, aes(x = Month, y = Weekdays)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = 'Total Number of Traffic violations', low = 'white', high = 'black') + theme(axis.title.y = element_blank())

### Sorting the weekdays per zip
remove(daily)
daily <- as.data.frame(table(myZip$Weekdays, myZip$zip_code))
names(daily) <- c('Weekdays', 'Zip', 'Freq')
daily$Zip <- as.numeric(as.character(daily$Zip))
daily$Zip = factor(daily$Zip)
daily$Weekdays <- factor(daily$Weekdays, ordered = TRUE, 
                         levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
#Plotting the number of crimes each day (line graph)
ggplot(daily, aes(x = Zip, y = Freq)) + geom_line(aes(group = Weekdays, color = Weekdays)) + xlab('Zip code') + ylab('Total number of pedestrian incidents') 
ggplot(daily, aes(x = Zip, y = Weekdays)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = 'Total Number of pedestrian incidents', low = 'white', high = 'black') + theme(axis.title.y = element_blank())


### Sorting the weekdays per hour
remove(daily)
daily <- as.data.frame(table(mydatMC$Weekdays, mydatMC$Hour))
names(daily) <- c('Weekdays', 'Hour', 'Freq')
daily$Hour <- as.numeric(as.character(daily$Hour))
daily$Hour <- factor(daily$Hour, ordered = TRUE, 
                      levels = c('0','1','2','3','4', '5', '6', '7', '8', '9', '10', '11', '12','13', '14', '15', '16', '17', '18', '19','20','21','22','23'))
daily$Weekdays <- factor(daily$Weekdays, ordered = TRUE, 
                               levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
# Plotting the number of crimes each day (line graph)
ggplot(daily, aes(x = Hour, y = Freq)) + geom_line(aes(group = Weekdays, color = Weekdays)) + xlab('Hour') + ylab('Total number of traffic violation events') 
ggplot(daily, aes(x = Hour, y = Weekdays)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = 'Total Number of Traffic violations', low = 'white', high = 'black') + theme(axis.title.y = element_blank())

### Sorting the month per hour
remove(daily)
daily <- as.data.frame(table(mydatMC$Month, mydatMC$Hour))
names(daily) <- c('Month', 'Hour', 'Freq')
daily$Hour <- as.numeric(as.character(daily$Hour))
daily$Hour <- factor(daily$Hour, ordered = TRUE, 
                     levels = c('0','1','2','3','4', '5', '6', '7', '8', '9', '10', '11', '12','13', '14', '15', '16', '17', '18', '19','20','21','22','23'))
daily$Month <- as.numeric(as.character(daily$Month))
daily$Month <- factor(daily$Month, ordered = TRUE, 
                      levels = c('3','4', '5', '6', '7', '8', '9', '10', '11', '12','1', '2'))
# Plotting the number of crimes each day (line graph)
ggplot(daily, aes(x = Hour, y = Freq)) + geom_line(aes(group = Month, color = Month)) + xlab('Month') + ylab('Total number of traffic violation events') 
ggplot(daily, aes(x = Hour, y = Month)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = 'Total Number of Traffic violations', low = 'white', high = 'black') + theme(axis.title.y = element_blank())

#### number of violations per day
myZip = mydat[mydat$label == "traffic" | mydat$label == "travel"  | mydat$label == "news" |
              mydat$label == "shopping"  | mydat$label == "sport", ]
daily <- as.data.frame(table(myZip$Month, myZip$label))
#daily = daily[1:31,]
names(daily) <- c('Month', 'Label', 'Freq')
ggplot(daily[1:10,], aes(x = Date, y = Freq)) + geom_line()
#daily$Month <- as.numeric(as.character(daily$Month))
#daily$Month <- factor(daily$Month, ordered = TRUE, 
#                      levels = c('3','4', '5', '6', '7', '8', '9', '10', '11', '12','1', '2'))
daily$V14 <- factor(daily$V14, ordered = TRUE, 
                         levels = c('Weekend', 'Working day'))
daily$Weekdays <- factor(daily$Weekdays, ordered = TRUE, 
                         levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
daily$Hour <- as.numeric(as.character(daily$Hour))
daily$Hour <- factor(daily$Hour, ordered = TRUE, 
                     levels = c('0','1','2','3','4', '5', '6', '7', '8', '9', '10', '11', '12','13', '14', '15', '16', '17', '18', '19','20','21','22','23'))
#Plotting the number of crimes each day (line graph)
ggplot(daily, aes(x = Month, y = Freq)) + geom_line(aes(group = Label, color = Label)) + xlab('Month') + ylab('Total number of public events') + theme_bw()
ggplot(daily, aes(x = Date, y = Freq)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = 'Total Number of Traffic violations', low = 'white', high = 'black') + theme(axis.title.y = element_blank())

# heapmap by city and day
remove(heatmap_data)
City = myLabel$label
Date = myLabel$Date
heatmap_data = ddply(myLabel,.(label),nrow)
#heatmap_data$Percentage = (heatmap_data/86395)*100 # this will work only for one city
heatmap_data$City <- factor(heatmap_data$City, ordered = TRUE, levels = c('Germantown', 'Silver Spring', 'Rockville', 
                                                                     'Bethesda', 'Gaithersburg', 'Wheaton', 'Potomac',
                                                                     'Montgomery Village', 'Takoma Park', 'Chevy Chase', 'Poolesville'))
ggplot(heatmap_data, aes(x = Date, y = City)) + geom_tile(aes(fill = V1)) + scale_fill_gradient(name = 'Total Number of Traffic violations', low = 'blue', high = 'red') + theme(axis.title.y = element_blank())
heatmap_data$zip_code = factor(heatmap_data$zip_code)
ggplot(heatmap_data, aes(x = heatmap_data$zip_code)) + geom_bar()
ggplot(data=df,aes(x=heatmap_data$zip_code, y=heatmap_data$V1))+ geom_point()

# statistics
data = ddply(mydatMC,.(City),nrow)
sum(data[data$City == 'Germantown',2])/sum(data[,2])

# normalize by population
sum(heatmap_data[,3])/sum(data[,2])
sum(heatmap_data[heatmap_data$City == 'Germantown',3])
sum(heatmap_data[heatmap_data$City == 'Silver Spring',3])
sum(heatmap_data[heatmap_data$City == 'Rockville',3])
sum(heatmap_data[heatmap_data$City == 'Bethesda',3])
sum(heatmap_data[heatmap_data$City == 'Gaithersburg',3])
sum(heatmap_data[heatmap_data$City == 'Wheaton',3])
sum(heatmap_data[heatmap_data$City == 'Potomac',3])
sum(heatmap_data[heatmap_data$City == 'Montgomery Village',3])
sum(heatmap_data[heatmap_data$City == 'Germantown',3])


# # by traffic type and city 
remove(heatmap_data)
City = bycities$City
Type = bycities$Violation.Type
heatmap_data = ddply(bycities,.(City, Type),nrow)
heatmap_data$City <- factor(heatmap_data$City, ordered = TRUE, levels = c('Germantown', 'Silver Spring', 'Rockville', 
                                                                          'Bethesda', 'Gaithersburg', 'Wheaton', 'Potomac',
                                                                          'Montgomery Village', 'Takoma Park', 'Chevy Chase', 'Poolesville'))
heatmap_data$Type <- factor(heatmap_data$Type, ordered = TRUE, levels = c('Warning', 'Citation', 'ESERO'))
ggplot(heatmap_data, aes(x = Type, y = V1)) + geom_line(aes(group = City, color = City)) + xlab('Violation Type') + ylab('Total number of traffic violation events') 
ggplot(heatmap_data, aes(x = Type, y = City)) + geom_tile(aes(fill = V1)) + scale_fill_gradient(name = 'Total Number of Traffic violations', low = 'white', high = 'black') + theme(axis.title.y = element_blank())


# probability event to happen during the weekdays
remove(daily)
daily <- as.data.frame(table(mydatMC$Weekdays))
monday = sum(daily[2,2])/sum(as.numeric(daily[,2]))
tuesday = sum(daily[6,2])/sum(as.numeric(daily[,2]))
wednesday = sum(daily[7,2])/sum(as.numeric(daily[,2]))
thursday = sum(daily[5,2])/sum(as.numeric(daily[,2]))
friday = sum(daily[1,2])/sum(as.numeric(daily[,2]))
saturday = sum(daily[3,2])/sum(as.numeric(daily[,2]))
sunday = sum(daily[4,2])/sum(as.numeric(daily[,2]))

# probability event to happen during the months
remove(daily)
daily <- as.data.frame(table(mydatMC$Month))
(as.numeric(daily[1,2])+as.numeric(daily[2,2])+as.numeric(daily[3,2])+as.numeric(daily[10,2])+as.numeric(daily[11,2])+as.numeric(daily[12,2]))/sum(as.numeric(daily[,2]))
summer = (as.numeric(daily[6,2])+as.numeric(daily[7,2])+as.numeric(daily[8,2]))/sum(as.numeric(daily[,2]))
spring = (as.numeric(daily[4,2])+as.numeric(daily[5,2])+as.numeric(daily[6,2]))/sum(as.numeric(daily[,2]))
winter = (as.numeric(daily[12,2])+as.numeric(daily[1,2])+as.numeric(daily[2,2]))/sum(as.numeric(daily[,2]))

# http://www.sunrisesunset.com/usa/Maryland.asp
# calculate number of events in day hours for 12 month
m = matrix(0, 12, 2)
size = nrow(daily)
for (i in 1:30) {
  if(daily[i,1] == '1') {
    if(daily[i,2] >= '7' || daily[i,2] <= 17) {
      m[1,1] = 1
      m[1,2] = m[1,2] + as.numeric(daily[i,3])
    }
  } else if(daily[i,1] == '2') {
    if(daily[i,2] >= '7' || daily[i,2] <= 18) {
      m[2,1] = 2
      m[2,2] = m[2,2] + as.numeric(daily[i,3])
    }
  } else if(daily[i,1] == '3') {
    if(daily[i,2] >= '6' || daily[i,2] <= 19) {
      m[3,1] = 3
      m[3,2] = m[3,2] + as.numeric(daily[i,3])
    }
  } else if(daily[i,1] == '4') {
    if(daily[i,2] >= '6' || daily[i,2] <= 19) {
      m[4,1] = 4
      m[4,2] = m[4,2] + as.numeric(daily[i,3])
    }
  } else if(daily[i,1] == '5') {
    if(daily[i,2] >= '5' || daily[i,2] <= 20) {
      m[5,1] = 5
      m[5,2] = m[5,2] + as.numeric(daily[i,3])
    }
  } else if(daily[i,1] == '6') {
    if(daily[i,2] >= '5' || daily[i,2] <= 20) {
      m[6,1] = 6
      m[6,2] = m[6,2] + as.numeric(daily[i,3])
    }
  } else if(daily[i,1] == '7') {
    if(daily[i,2] >= '5' || daily[i,2] <= 20) {
      m[7,1] = 7
      m[7,2] = m[7,2] + as.numeric(daily[i,3])
    }
  } else if(daily[i,1] == '8') {
    if(daily[i,2] >= '6' || daily[i,2] <= 20) {
      m[8,1] = 8
      m[8,2] = m[8,2] + as.numeric(daily[i,3])
    }
  } else if(daily[i,1] == '9') {
    if(daily[i,2] >= '6' || daily[i,2] <= 19) {
      m[9,1] = 9
      m[9,2] = m[9,2] + as.numeric(daily[i,3])
    }
  } else if(daily[i,1] == '10') {
    if(daily[i,2] >= '7' || daily[i,2] <= 18) {
      m[10,1] = 10
      m[10,2] = m[10,2] + as.numeric(daily[i,3])
    }
  } else if(daily[i,1] == '11') {
    if(daily[i,2] >= '6' || daily[i,2] <= 17) {
      m[11,1] = 11
      m[11,2] = m[11,2] + as.numeric(daily[i,3])
    }
  } else if(daily[i,1] == '12') {
    if(daily[i,2] >= '7' || daily[i,2] <= 16) {
      m[12,1] = 12
      m[12,2] = m[12,2] + as.numeric(daily[i,3])
    }
  }
}
# probability event to happen between sunrise and sunset
sum(m[,2])/sum(as.numeric(daily[,3]))


# dominant traffic violation types (charges) ??
remove(heatmap_data)
a = mydatMC[mydatMC$City == 'Chevy Chase',]
heatmap_data = ddply(a,.(City, Charge),nrow)
tt = heatmap_data[order(-heatmap_data$V1),]
tt = t(head(arrange(tt,desc(tt$V1)), n=10)) # sort desc and get top 10 records
# write the output to csv file
# write.csv(data.frame(head(arrange(heatmap_data,desc(V1)), n=80)), file="aa.csv", row.names=FALSE, na="", col.names=FALSE)
# 
# heatmap_data$City <- factor(heatmap_data$City, ordered = TRUE, levels = c('Germantown', 'Silver Spring', 'Rockville', 
#                                                                           'Bethesda', 'Gaithersburg', 'Wheaton', 'Potomac',
#                                                                           'Montgomery Village', 'North Potomac',
#                                                                           'Takoma Park', 'Chevy Chase', 'Poolesville'))
# ggplot(heatmap_data, aes(x = City, y = Charges)) + geom_tile(aes(fill = V1)) + scale_fill_gradient(name = 'Total Number of Traffic violations', low = 'white', high = 'black') + theme(axis.title.y = element_blank())

# dominant categories per city
grup = ddply(bycities,.(City, V40),nrow)
cityname = c('Germantown', 'Silver Spring', 'Rockville','Bethesda','Gaithersburg','Wheaton','Potomac','North Bethesda','Montgomery Village','North Potomac','Takoma Park','Chevy Chase')
m = matrix(0,12,10)
mm = matrix(0,12,10)
for(i in 1:12) {
  grupC = grup[grup$City == cityname[i],]
  tmp = grupC[order(-grupC$V1),]
  gg = head(arrange(tmp,desc(tmp$V1)), n=10) 
  mm[i,] = gg[,2]
  m[i,] = gg[,3]
}
write.csv(data.frame(mm), file="type-categories.csv", row.names=FALSE, na="", col.names=FALSE)

# number of raffic violations in categories
topcategories = ddply(mydat,.(mydat$V43),nrow)

# top locations
toplocations = ddply(mydatMC,.(mydatMC$Location, mydatMC$City),nrow)

# 
s = mydatMC[mydatMC$VLocation == 'MONTROSE PKWY/E JEFFERSON ST' & mydatMC$City == 'Wheaton',]
ddply(s,.(s$Charge), nrow)

q = mydatMC[mydatMC$Contributed.To.Accident == 'Yes' & mydatMC$VehicleType == '06 - Heavy Duty Truck',]
tmp = ddply(q,.(q$Charge),nrow)
tmp[order(-tmp$V1),]

# PEDESTRIAN'S RIGHTS AND RULES
q = bycities[bycities$V47 == "PEDESTRIAN'S RIGHTS AND RULES" & bycities$City == 'Silver Spring',]
tmp = ddply(q,.(q$Location),nrow)
qq = t(data.frame(tmp[order(-tmp$V1),]))

#
atsametime = ddply(mydat,.(mydat$Time.Of.Stop, mydat$Description, mydat$Location),nrow)

# statistics
# number of traffic violations per city
c$City = as.factor(c$City)
a = ddply(c, .(c$City), nrow)
summary(a)
a$p = round(a$V1/sum(a$V1), 3)*100

for(i in 1: dim(mydat)[1]) {
  if (str_detect(mydat[i,8], "^Sunday")) {
    mydat[i,14] = "Weekend"
  } else if (str_detect(mydat[i,8], "^Saturday")) {
    mydat[i,14] = "Weekend"
  } else {
    mydat[i,14] = "Working day"
  }
}

# categorization of charges to official groups
for(i in 1: dim(bycities)[1]) {
  if (str_detect(bycities[i,30], "^7")) {
    bycities[i,47] = "MARYLAND TRANSIT"
  } else if (str_detect(bycities[i,30], "^8")) {
    bycities[i,47] = "HIGHWAYS"
  } else if (str_detect(bycities[i,30], "^11-39")) {
    bycities[i,47] = "MOTOR CARRIER SAFETY INSPECTION REGULATIONS"
  } else if (str_detect(bycities[i,30], "^12")) {
    bycities[i,47] = "MOTOR VEHICLE ADMINISTRATION"
  } else if (str_detect(bycities[i,30], "^13")) {
    bycities[i,47] = "CERTIFICATES OF TITLE AND REGISTRATION OF VEHICLES"
  } else if (str_detect(bycities[i,30], "^14")) {
    bycities[i,47] = "ANTI THEFT LAWSE"
  } else if (str_detect(bycities[i,30], "^15")) {
    bycities[i,47] = "LICENSING OF BUSINESSES AND OCCUPATIONS"
  } else if (str_detect(bycities[i,30], "^16")) {
    bycities[i,47] = "DRIVER'S LICENSE"
  } else if (str_detect(bycities[i,30], "^17")) {
    bycities[i,47] = "REQUIRED SECURITY"
  } else if (str_detect(bycities[i,30], "^18")) {
    bycities[i,47] = "FOR RENT VEHICLES"
  } else if (str_detect(bycities[i,30], "^20")) {
    bycities[i,47] = "ACCIDENTS & ACCIDENT REPORTS"
  } else if (str_detect(bycities[i,30], "^23")) {
    bycities[i,47] = "INSPECTION OF USED VEHICLES AND WARNINGS FOR DEFECTIVE EQUIPMENT"
  } else if (str_detect(bycities[i,30], "^24")) {
    bycities[i,47] = "SIZE, WEIGHT & LOAD: HIGHWAY PRESERVATION"
  } else if (str_detect(bycities[i,30], "^25")) {
    bycities[i,47] = "RESPECTIVE POWERS OF STATE & LOCAL AUTHORITIES; DISPOSITION OF ABANDONED VEHICLES"
  } else if (str_detect(bycities[i,30], "^26")) {
    bycities[i,47] = "PARTIES & PROCEDURES ON CITATION, ARREST, TRIAL & APPEAL"
  } else if (str_detect(bycities[i,30], "^27")) {
    bycities[i,47] = "PENALTIES; DISPOSITION OF FINES AND FORFEITURES"
  } else if (str_detect(bycities[i,30], "^BR10")) {
    bycities[i,47] = "BUSINESS REGULATIONS"
  } else if (str_detect(bycities[i,30], "^TG")) {
    bycities[i,47] = "MOTOR VEHICLE FUEL TAX"
  } else if (str_detect(bycities[i,30], "^11.15")) {
    bycities[i,47] = "MARYLAND REGULATIONS"
  } else if (str_detect(bycities[i,30], "^172")) {
    bycities[i,47] = "HAZARDOUS MATERIALS VIOLATIONS"
  } else if (str_detect(bycities[i,30], "^38")) {
    bycities[i,47] = "MOTOR CARRIER SAFETY INSPECTION REGULATIONS"
  } else if (str_detect(bycities[i,30], "^39")) {
    bycities[i,47] = "MOTOR CARRIER SAFETY INSPECTION REGULATIONS"
  }
}

for(i in 1: dim(bycities)[1]) {
  if (str_detect(bycities[i,30], "^21-1")) {
    bycities[i,47] = "RULES OF THE ROAD - GENERAL PROVISIONS"
  } else if (str_detect(bycities[i,30], "^21-2")) {
    bycities[i,47] = "TRAFFIC SIGNS, SIGNALS & MARKINGS"
  } else if (str_detect(bycities[i,30], "^21-3")) {
    bycities[i,47] = "DRIVING ON RIGHT SIDE OF ROADWAY; OVERTAKING & PASSING; USE OF ROADWAY"
  } else if (str_detect(bycities[i,30], "^21-4")) {
    bycities[i,47] = "RIGHT-OF-WAY"
  } else if (str_detect(bycities[i,30], "^21-5")) {
    bycities[i,47] = "PEDESTRIAN'S RIGHTS AND RULES"
  } else if (str_detect(bycities[i,30], "^21-6")) {
    bycities[i,47] = "TURNING & STARTING; SIGNALS ON STOPPING, TURNING & STARTING"
  } else if (str_detect(bycities[i,30], "^21-7")) {
    bycities[i,47] = "SPECIAL STOPS REQUIRED"
  } else if (str_detect(bycities[i,30], "^21-8")) {
    bycities[i,47] = "SPEED RESTRICTIONS"
  } else if (str_detect(bycities[i,30], "^21-9")) {
    bycities[i,47] = "RECKLESS, NEGLIGENT, AGGRESSIVE, OR IMPAIRED DRIVING; FLEEING OR ELUDING POLICE"
  } else if (str_detect(bycities[i,30], "^21-10")) {
    bycities[i,47] = "STOPPING, STANDING, AND PARKING"
  } else if (str_detect(bycities[i,30], "^21-11")) {
    bycities[i,47] = "MISCELLANEOUS RULES"
  } else if (str_detect(bycities[i,30], "^21-12")) {
    bycities[i,47] = "OPERATION OF BICYCLES & PLAY VEHICLES"
  } else if (str_detect(bycities[i,30], "^21-13")) {
    bycities[i,47] = "OPERATION OF MOTORCYCLES"
  } else if (str_detect(bycities[i,30], "^21-14")) {
    bycities[i,47] = "OPERATION OF VEHICLES ON CERTAIN TOLL FACILITIES"
  } else if (str_detect(bycities[i,30], "^22-1")) {
    bycities[i,47] = "SCOPE AND EFFECT OF EQUIPMENT PROVISIONS"
  } else if (str_detect(bycities[i,30], "^22-2")) {
    bycities[i,47] = "LAMPS & OTHER LIGHTING EQUIPMENT"
  } else if (str_detect(bycities[i,30], "^BUMPERS")) {
    bycities[i,47] = "LAMPS & OTHER LIGHTING EQUIPMENT"
  } else if (str_detect(bycities[i,30], "^22-3")) {
    bycities[i,47] = "BRAKES"
  } else if (str_detect(bycities[i,30], "^22-4")) {
    bycities[i,47] = "OTHER EQUIPMENT"
  } else if (str_detect(bycities[i,30], "^22-6")) {
    bycities[i,47] = "NOISE ABATEMENT PROGRAM"
  } else if (str_detect(bycities[i,5], "^WINDOW TINT")) {
    bycities[i,47] = "OTHER EQUIPMENT"
  } else if (str_detect(bycities[i,5], "^GLASS")) { 
    bycities[i,47] = "OTHER EQUIPMENT"
  } else if (str_detect(bycities[i,5], "^EXHAUST")) { 
    bycities[i,47] = "OTHER EQUIPMENT"
  } else if (str_detect(bycities[i,5], "^TAG LIGHTS")) {
    bycities[i,47] = "LAMPS & OTHER LIGHTING EQUIPMENT"
  } else if (str_detect(bycities[i,5], "^STOP LIGHTS")) {
    bycities[i,47] = "LAMPS & OTHER LIGHTING EQUIPMENT"
  } else if (str_detect(bycities[i,5], "^HEADLIGHTS")) {
    bycities[i,47] = "LAMPS & OTHER LIGHTING EQUIPMENT"
  } else if (str_detect(bycities[i,5], "^HEADLIGHT")) {
    bycities[i,47] = "LAMPS & OTHER LIGHTING EQUIPMENT"
  } else if (str_detect(bycities[i,5], "^TAILLIGHTS")) {
    bycities[i,47] = "LAMPS & OTHER LIGHTING EQUIPMENT"
  } else if (str_detect(bycities[i,5], "^VEHICLE BRAKE SYSTEM")) {
    bycities[i,47] = "BRAKES"
  } else if (str_detect(bycities[i,30], "^9-220(a)")) {
    bycities[i,47] = "MOTOR VEHICLE FUEL TAX"
  } else if (str_detect(bycities[i,30], "^68*")) {
    bycities[i,47] = "MOTOR CARRIER SAFETY INSPECTION REGULATIONS"
  } else if (str_detect(bycities[i,30], "^59")) {
    bycities[i,47] = "MOTOR CARRIER SAFETY INSPECTION REGULATIONS"
  } else if (str_detect(bycities[i,30], "^11-383.23")) {
    bycities[i,47] = "HAZARDOUS MATERIALS VIOLATIONS"
  } else if (str_detect(bycities[i,5], "^INTRODUCING ADDITIVES INTO SPECIAL FUEL BY UN- AUTHORIZED PERSON AND CONTRARY TO REGULATIONS")) {
    bycities[i,47] = "BUSINESS REGULATIONS"
  } else if (str_detect(bycities[i,5], "^FAIL OF RETAIL SER STATION DEALER TO PROVIDE SELF-SERV PRICE TO DISABLED DR DISPLAYING SPECIAL PLATE")) {
    bycities[i,47] = "BUSINESS REGULATIONS"
  } else if (str_detect(bycities[i,5], "^DISPLAY ID# ON TRANS VEH NOT CONTAINING HAZ MATERIAL ASSOC W/THAT NO.")) {
    bycities[i,47] = "HAZARDOUS MATERIALS VIOLATIONS"
  } else if (str_detect(bycities[i,5], "^FAILURE TO AFFIX PLACARD IN CONFORMANCE WITH REQUIREMENTS FOR VISIBILITY AND DISPLAY")) {
    bycities[i,47] = "HAZARDOUS MATERIALS VIOLATIONS"
  } else if (str_detect(bycities[i,5], "^PLACARDING MOTOR VEHICLE WITH PLACARD NOT MEETING GENERAL SPECIFICATIONS FOR PLACARDS")) {
    bycities[i,47] = "HAZARDOUS MATERIALS VIOLATIONS"
  } else if (str_detect(bycities[i,5], "^OPERATING A COMMERCIAL MOTOR VEHICLE W/O A CDL WHEN REQUIRED")) {
    bycities[i,47] = "MOTOR CARRIER SAFETY INSPECTION REGULATIONS"
  }
}
# SUSPENSION / SHOCKS; BUMPERS; REARVIEW MIRRORS; TIRES
for(i in 1: dim(bycities)[1]) {
  if (str_detect(bycities[i,30], "^50")) {
    bycities[i,47] = "OTHER EQUIPMENT"
  } else if (str_detect(bycities[i,30], "^51")) {
    bycities[i,47] = "OTHER EQUIPMENT"
  } else if (str_detect(bycities[i,30], "^52")) {
    bycities[i,47] = "OTHER EQUIPMENT"
  } else if (str_detect(bycities[i,30], "^53")) {
    bycities[i,47] = "OTHER EQUIPMENT"
  } else if (str_detect(bycities[i,30], "^54*")) {
    bycities[i,47] = "OTHER EQUIPMENT"
  } else if (str_detect(bycities[i,30], "^55*")) {
    bycities[i,47] = "OTHER EQUIPMENT"
  } else if (str_detect(bycities[i,30], "^56*")) {
    bycities[i,47] = "OTHER EQUIPMENT"
  } else if (str_detect(bycities[i,30], "^57*")) {
    bycities[i,47] = "OTHER EQUIPMENT"
  } else if (str_detect(bycities[i,5], "^LANE DIRECTION")) {
    bycities[i,47] = "DRIVING ON RIGHT SIDE OF ROADWAY"
  } else if (str_detect(bycities[i,5], "^FAILURE OF MOTOR CARRIER TO DISPLAY ID MARKER FOR CMV AS COMPTROLLER REQUIRES BY REGULATION")) {
    bycities[i,47] = "MOTOR VEHICLE FUEL TAX"
  } 
}

#write.csv(data.frame(bycities), file="mydataMC.csv", row.names=FALSE, na="", col.names=FALSE)

# group by date and merge with weather dataset
weather <- read.csv("weather-data\\2016.csv", stringsAsFactors = FALSE)
weather <- weather[,-9]
heatmap_data = ddply(mydat,.(Date),nrow)
merged.data <- merge(heatmap_data, weather, by="Date")

# correlation
merged.data$V43 = as.factor(merged.data$V43)
cordata <- merged.data[,c(-1)]
tw = ddply(cordata,.(V1,cordata$Mean.TemperatureF),nrow)
head(tw)
M<-cor(tw) # http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
head(round(M,2))
corrplot(M, method="color")
tw <- tw[tw$V43 == 'ACCIDENTS & ACCIDENT REPORTS' | tw$V43 == 'MARYLAND TRANSIT' | tw$V43 == 'HIGHWAYS',]
tw$`cordata$Mean.TemperatureF` = as.numeric(tw$`cordata$Mean.TemperatureF`)
ggplot(tw, aes(x=tw$Mean.TemperatureF,y=tw$V1)) + geom_point() + xlab ('Temperature (F)') + ylab('Number of traffic violation events')
plot(x = tw$Mean.TemperatureF, y = tw$V1)
with(tw, cor(tw$Mean.TemperatureF, tw$V1)) # correlation is 0.1331096
# least squere
fit = lm(tw$V1 ~ tw$Mean.TemperatureF, data = tw)
coef(fit)
ggplot(tw, aes(x=tw$Mean.TemperatureF,y=tw$V1)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth(method="lm", se=FALSE) + xlab ('Temperature (F)') + ylab('Number of traffic violation events')

# humidity, temperature and number of traffic violation
htw = cordata[,c(1,2,3)]
ggplot(htw, aes(x = htw$Mean.TemperatureF, y = htw$Mean.Humidity)) + geom_tile(aes(fill = htw$V1)) + scale_fill_gradient(name = 'Total Number of Traffic violations', low = 'yellow', high = 'red') + theme(axis.title.y = element_blank()) + ylab('Humidity') + xlab ('Temperature (F)')

# Mine association rules.
subset = mydat[,c(6,9)] #mydat[,c(6,9,32,36,37,42,43)]
subset$Location = as.factor(subset$Location)
subset$City = as.factor(subset$City)
subset$Driver.City = as.factor(subset$Driver.City)
subset$Date = as.factor(subset$Date)
subset$Weekdays = as.factor(subset$Weekdays)
subset$Hour = as.factor(subset$Hour)
subset$V43 = as.factor(subset$V43)
rules <- apriori(subset, parameter = list(supp = 0.1, conf = 0.1)) # , target = "rules"
inspect(rules)
summary(rules)
plot(rules)
plot(rules, method="graph", control=list(type="items"))
plot(rules, method="paracoord", control=list(reorder=TRUE))

# http://www.r-bloggers.com/d3heatmap-interactive-heat-maps/
#install.packages("d3heatmap")
library(d3heatmap)
d3heatmap(violation_matrix, scale = "column", dendrogram = "none", color = "Blues")
d3heatmap(violation_matrix, colors = "Blues", scale = "column", dendrogram = "row", k_row = 3)

#
tab = with(bycities, table(City,Date))
tab/sum(tab)
round(tab/sum(tab), 3)


myTable = data.frame()
myTable = edit(myTable)

