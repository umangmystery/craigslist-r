#installing required packages
#install.packages("readxl")
#install.packages("lubridate")
#install.packages("writexl")
#install.packages("funModeling")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("e1071")
#install.packages("treemap")
#install.packages("hrbrthemes")
#install.packages("treemapify")
#install.packages("forcats")

#loading libraries
library(readxl)
library(lubridate)
library(tidyverse) 
library(funModeling)
library(writexl)
library(ggplot2)
library(e1071)
library(forcats)
library(hrbrthemes)
library(treemap)
library(treemapify)
library(maps)

#Reading the data file
Vehicle_Data<-read_xlsx("vehicles_changed.xlsx")
StateName <- read_xlsx("StateNames.xlsx")


#-------------------------------------------------------------
#Data Processing  
#Removing all null values
#Vehicle_Data <- na.omit(Vehicle_Data)

#Converting few column values to factors
Vehicle_Data$manufacturer <- as.factor(Vehicle_Data$manufacturer)
Vehicle_Data$condition <- as.factor(Vehicle_Data$condition)
Vehicle_Data$cylinders <- as.factor(Vehicle_Data$cylinders)
Vehicle_Data$fuel <- as.factor(Vehicle_Data$fuel)
Vehicle_Data$title_status <- as.factor(Vehicle_Data$title_status)
Vehicle_Data$transmission <- as.factor(Vehicle_Data$transmission)
Vehicle_Data$drive <- as.factor(Vehicle_Data$drive)
Vehicle_Data$size <- as.factor(Vehicle_Data$size)
Vehicle_Data$type <- as.factor(Vehicle_Data$type)
Vehicle_Data$state <- as.factor(Vehicle_Data$state)

#Converting to date format
Vehicle_Data$year <- ymd(Vehicle_Data$year, truncated = 2L)

#Validating data in each column
summary(Vehicle_Data)
glimpse(Vehicle_Data)

#trimming values of price
Vehicle_Data <- Vehicle_Data[(Vehicle_Data$price <= 200000) & (Vehicle_Data$price >= 500),]

#coverting null odometer values to 0
Vehicle_Data$odometer[is.na(Vehicle_Data$odometer)] <- 0

#Removing null values from the columns which we will need for hypothesis
sort(sapply(Vehicle_Data, function(x) sum(is.na(x))))
Vehicle_Data <- Vehicle_Data[!is.na(Vehicle_Data$year),]
Vehicle_Data <- Vehicle_Data[!is.na(Vehicle_Data$title_status),]
Vehicle_Data <- Vehicle_Data[!is.na(Vehicle_Data$transmission),]
Vehicle_Data <- Vehicle_Data[!is.na(Vehicle_Data$fuel),]
Vehicle_Data <- Vehicle_Data[!is.na(Vehicle_Data$type),]
Vehicle_Data <- Vehicle_Data[!is.na(Vehicle_Data$drive),]
Vehicle_Data <- Vehicle_Data[!is.na(Vehicle_Data$condition),]
Vehicle_Data <- Vehicle_Data[!is.na(Vehicle_Data$size),]
Vehicle_Data <- Vehicle_Data[!is.na(Vehicle_Data$cylinders),]


#Giving each row a sequential number by converting ID
Vehicle_Data$id <- seq(from = 1, to = nrow(Vehicle_Data))

#Getting full State Names
Vehicle_Data <- left_join(Vehicle_Data, StateName, by = c("state"="ShortForm"))

# Writing data to xlsx
#write_xlsx(Vehicle_Data,"Final_Vehicle2.xlsx")



#--------------------------------------------
#Statistical Analysis
#mean
mean_price <- mean(Vehicle_Data$price)
mean_price
mean_odometer <- mean(Vehicle_Data$odometer)
mean_odometer

#median
median_price <- median(Vehicle_Data$price)
median_price
median_odometer <- median(Vehicle_Data$odometer)
median_odometer

#min and Max
min_price <- min(Vehicle_Data$price)
min_price
max_price <- max(Vehicle_Data$price)
max_price

min_odometer <- min(Vehicle_Data$odometer)
max_odometer <- max(Vehicle_Data$odometer)

#Range
range_price <- range(Vehicle_Data$price)
range_price
range_odometer <- range(Vehicle_Data$odometer)
range_odometer

#Variance
variance_price <- var(Vehicle_Data$price)
variance_price
variance_odometer <- var(Vehicle_Data$odometer)
variance_odometer

#Standard Variance
sd_price <- sd(Vehicle_Data$price)
sd_price
sd_odometer <- sd(Vehicle_Data$odometer)
sd_odometer

#Coefficient of Variation 
cov_price <- (sd_price/mean_price)*100
cov_price
cov_odometer <- (sd_odometer/mean_odometer)*100
cov_odometer

#Skewness
skewness_price <- skewness(Vehicle_Data$price)
skewness_price
skewness_odometer <- skewness(Vehicle_Data$odometer) #highly skewed
skewness_odometer

#Kurtosis
kurtosis_price <- kurtosis(Vehicle_Data$price)
kurtosis_price
kurtosis_odometer <- kurtosis(Vehicle_Data$odometer)
kurtosis_odometer




#--------------------------------------------
#Data Visualization
#histogram for price
ggplot(Vehicle_Data, aes(x=price)) + geom_histogram() + ggtitle("Price Distribution")

#bar graph for price based on condition of the car
Vehicle_Data %>%
  select(id, price, condition) %>%
  group_by(condition) %>%
  drop_na() %>%
  summarise(Average = mean(price)) %>%
  arrange(desc(Average)) %>%
ggplot(aes(x=Average, y=fct_inorder(condition))) + geom_bar(stat="identity", fill="red")+ xlab("Average Price") + ylab("Condition of the car")+ggtitle("Average Price for each Condition")


#Area graph for Average price for last 30 years
Vehicle_Data %>%
  select(id, price, year) %>%
  filter(year>="1990-01-01") %>%
  group_by(year) %>%
  drop_na() %>%
  summarise(Average = mean(price)) %>%
ggplot(aes(x=year, y=Average)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2) +
  geom_point(size=2, color="#69b3a2") +
  theme_light() +
  ggtitle("Average price for last 30 years")


#tree map for Average Price of each Manufacture
Vehicle_Data %>%
  select(id, price, manufacturer) %>%
  group_by(manufacturer) %>%
  drop_na() %>%
  summarise(Average = mean(price)) %>%
  arrange(desc(Average)) %>%
  ggplot(aes(area = Average,
                 fill = Average,
                 label=manufacturer)) +geom_treemap()+
  geom_treemap_text(colour = "black",
                    place = "top")+scale_fill_distiller(palette = "RdPu", direction = 1)+
  ggtitle("Average Price of each Manufacture")


#Line plot for number of cars listed each year
Vehicle_Data %>%
  select(id,year) %>%
  group_by(year) %>%
  drop_na() %>%
  summarise(Count = n()) %>%
  ggplot(aes(x=year, y=Count)) +
  geom_line(color="#69b3a2", size=2) +
  theme_light() +
  ggtitle("Number of cars listed each year")


#Pie chart showing average Price for each of type of car listed
Vehicle_Data %>%
  select(id,price,type) %>%
  group_by(type) %>%
  drop_na() %>%
  summarise(Average = mean(price)) %>%
ggplot(aes(x="", y=Average, fill=type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + theme_void() +
  ggtitle("Average Price for each of type of car listed")


#Map to show price for each state
mapdata <- Vehicle_Data %>%
  select(id, price, `US STATE`) %>%
  group_by(`US STATE`) %>%
  drop_na() %>%
  summarise(Average = mean(price)) %>%
  arrange(desc(Average))
mapdata$`US STATE` <- tolower(mapdata$`US STATE`)

us_states <- map_data("state")
us_states %>% 
  left_join(mapdata, by=c("region"="US STATE")) %>%
  ggplot(aes(x=long,y=lat,group=group, fill=Average)) +
  geom_polygon(color = "gray90", size = 0.2)+scale_fill_distiller(palette = "YlGnBu", direction = 1)+theme(axis.title.x=element_blank(),
                                                                                                            axis.text.x=element_blank(),
                                                                                                            axis.ticks.x=element_blank(),axis.title.y=element_blank(),
                                                                                                           axis.text.y=element_blank(),
                                                                                                           axis.ticks.y=element_blank())
#--------------------------------------------
#Statistical Test

#--------------------------------------------
#Advance Analysis