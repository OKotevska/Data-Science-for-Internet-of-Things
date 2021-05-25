# ensure the results are repeatable
set.seed(7)

# required libraries
library(timeDate)
library(plyr)
library(ggplot2)
library(stringr)

# Reading the dataset
require(curl)
data <- read.csv(curl("https://raw.githubusercontent.com/CloudDemo/traffic-analysis/master/basic-features-2.csv"), header = FALSE, dec=".", stringsAsFactors = FALSE)
attach(data)

# 
heatmap_data = ddply(data,.(App.protocol, Destination.adr, Destination.port, Source.adr, Source.port),nrow)
head(heatmap_data)

heatmap_data$protocol = factor(heatmap_data$App.protocol)
ggplot(heatmap_data, aes(x = heatmap_data$App.protocol)) + geom_bar() + xlab('Protocol type') + ylab('Total number of packets') 

selected = heatmap_data[heatmap_data$Source.adr == "74:da:38:80:79:fc" | heatmap_data$Source.adr == "74:da:38:80:7a:08" 
                        | heatmap_data$Source.adr == "'b0:c5:54:1c:71:85" | heatmap_data$Source.adr == "b0:c5:54:25:5b:0e" 
                        | heatmap_data$Source.adr == "5c:cf:7f:07:ae:fb" 
                        | heatmap_data$Source.adr == "5c:cf:7f:06:d9:02", ]
heatmap_port = ddply(selected,.(Source.adr, App.protocol),nrow)
names(heatmap_port) <- c('SourceAdr', 'Protocol', 'Freq')

ggplot(heatmap_port, aes(x = Protocol)) + geom_bar()
ggplot(data=heatmap_port, aes(x=Protocol, y=SourceAdr)) + geom_point() + xlab('Protocol') + ylab('Source IP') 

ggplot(heatmap_port, aes(x = Freq, y = SourceAdr)) + geom_bar(aes(group = Protocol, color = Protocol)) + xlab('Adress') + ylab('Total number') 
ggplot(heatmap_port, aes(x = Protocol, y = SourceAdr)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = 'Total Number', low = 'blue', high = 'red') + theme(axis.title.y = element_blank())

# 'EdimaxCam': ['74:da:38:80:79:fc', '74:da:38:80:7a:08']
# 'D-LinkDayCam': ['b0:c5:54:1c:71:85']
# 'D-LinkCam': ['b0:c5:54:25:5b:0e']
# 'SmarterCoffee': ['5c:cf:7f:07:ae:fb']
# 'iKettle2': ['5c:cf:7f:06:d9:02']

previous = 0
IA_times = c()
j = 1
x.num = 0
y.num = 0
z.num = 0
i = 1
for (i in 1:dim(basic.features)[1]) {
  if (previous == 0) {
    previous = as.numeric(basic.features[i,4])
    IA_times[j] = 0
  } else {
    x.num <- as.numeric(basic.features[i,4])
    y.num <- as.numeric(previous)
    z.num <- x.num - y.num
    if (z.num < 0) {
      IA_times[j] = 0
    } else {
      IA_times[j] = z.num
    }
    previous = basic.features[i,4]
  }
  j = j + 1
}
IAT_times <- as.data.frame(IA_times)
ggplot(IAT_times, aes(x=IAT_times$c, y=IAT_times$IA_times)) + geom_line(color='steelblue') + xlab('Number of packets') + ylab('Inter Arrival Time') 

# add IAT to the list of features
basic.features$IAT = IAT_times$IA_times

