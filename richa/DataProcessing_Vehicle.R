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
#install.packages("reshape2")
#install.packages("mapproj")
#install.packages("maps")
#install.packages("fitdistrplus")

#loading libraries
library(readxl)
library(reshape2)
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
library(mapproj)
library(fitdistrplus)

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
#glimpse(Vehicle_Data)

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

#Creating 6 bins/categories for Price
Vehicle_Data$Price_Cat <- cut(Vehicle_Data$price, breaks = 6, labels = c("VeryLow", "Low", "Low to Medium","Medium to High","High","VeryHigh"))

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
# Frequency of used vehicles based on price
price_freq <- Vehicle_Data %>%
  select(id, Price_Cat) %>%
  group_by(Price_Cat) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(p_pmf = round(count/sum(count),6)) %>% #pmf
  mutate(p_cdf = round(cumsum(p_pmf),6)) #cmf

price_freq

#expected value
price_freq_val <- weighted.mean(price_freq$count, price_freq$p_pmf)
price_freq_val


# Frequency of used vehicles listed based on condition
cond_freq <- Vehicle_Data %>%
  select(id, condition) %>%
  group_by(condition) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(c_pmf = round(count/sum(count),6)) %>% #pmf
  mutate(c_cdf = round(cumsum(c_pmf),6)) #cmf

cond_freq

#expected value
cond_freq_val <- weighted.mean(cond_freq$count, cond_freq$c_pmf)
cond_freq_val


# joint probability for price and condition
# frequency table
joint_freq <- outer(price_freq$count, cond_freq$count, FUN = "+")
rownames(joint_freq) <- price_freq$Price_Cat
colnames(joint_freq) <- cond_freq$condition
joint_freq

# probabilities
joint_prob <- round(joint_freq/sum(joint_freq),6)
joint_prob

# restructuring the data
joint_df <- melt(joint_prob)
colnames(joint_df) <- c('Price', 'Condition', 'frequency')
joint_df

#heat map for Joint freq of price and condition
ggplot(joint_df,aes(x=Price, y=Condition, fill=frequency))+
  geom_tile()+scale_fill_distiller(palette = "YlGn", direction = 1) +geom_text(aes(label = frequency),hjust=1, vjust = 2.0, colour = "black")+
  theme_light()+ggtitle("Joint freq Heat Map of price and condition")


# calculating the coefficient
cor(price_freq$count, cond_freq$count)




#--------------------------------------------
#Data Visualization
#histogram for price
ggplot(Vehicle_Data, aes(x=price)) + geom_histogram(fill="red",color="white",alpha=0.7,bins = 20)+ggtitle("Price Distribution")


#boxplot for price
#ggplot(data = Vehicle_Data, aes(x = "", y = price)) + geom_boxplot

#bar graph for price based on condition of the car
Vehicle_Data %>%
  select(id, price, condition) %>%
  group_by(condition) %>%
  drop_na() %>%
  summarise (Average = mean(price)) %>%
  arrange(desc(Average)) %>%
ggplot(aes(x=Average, y=fct_inorder(condition))) + geom_bar(stat="identity", fill="grey")+ xlab("Average Price") + ylab("Condition of the car")+ geom_text(aes(label = round(Average,3)),hjust=1, vjust = 2.0, colour = "black")+ggtitle("Average Price for each Condition")


#Area graph for Average price for last 30 years
Vehicle_Data %>%
  select(id, price, year) %>%
  filter(year>="1990-01-01") %>%
  group_by(year) %>%
  drop_na() %>%
  summarise(Average = mean(price)) %>%
ggplot(aes(x=year, y=Average)) +
  geom_area( fill="#39a4a5", alpha=0.4) +
  geom_line(color="#69c4a2", size=2) +
  geom_point(size=2, color="#69c4c2") +
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
  geom_line(color="black", size=1) +
  theme_light() +
  ggtitle("Number of vehicles listed each year")


#Pie chart showing number of vehicle of each type listed
Vehicle_Data %>%
  select(id,price,type) %>%
  group_by(type) %>%
  drop_na() %>%
  summarise(Count = n()) %>%
  ggplot(aes(x="", y=Count, fill=type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + theme_void() +
  ggtitle("Number of vehicle of each type listed")+
  scale_fill_manual(values=c("#551A8B", "#E69F00", "#56B4E9","#00008B", "#00611C", "#00EE00","#2F2F4F", "#E35152", "#999999","#F08080", "#EEEE00", "#8B5A00","#55141C"))


#Map to show price for each state
mapdata <- Vehicle_Data %>%
  select(id, price, `US STATE`) %>%
  group_by(`US STATE`) %>%
  drop_na() %>%
  summarise(Average = mean(price)) %>%
  arrange(desc(Average))
mapdata$`US STATE` <- tolower(mapdata$`US STATE`)

us_states <- map_data("state")

#for labels
us_states_labels<-us_states %>% 
  left_join(mapdata, by=c("region"="US STATE")) %>%
  select(long, lat, group, Average, region ) %>%
  group_by(`region`) %>%
  summarise(across(long:Average, ~ mean(.x, na.rm = TRUE)))

us_states %>% 
  left_join(mapdata, by=c("region"="US STATE")) %>%
  ggplot(aes(x=long,y=lat,group=group, fill=Average))+ggtitle("State-wise Average price of vehicles")+
  geom_polygon(color = "gray90", size = 0.1)+geom_text(data=us_states_labels,aes(long, lat, label = region), size=2, vjust = 0, nudge_y = -0.05,hjust = 0, nudge_x = -0.7)+scale_fill_distiller(palette = "YlGnBu", direction = 1)+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
  
  
#scatter plot for price and odometer
#Vehicle_Data %>%
#  select(id,year,Price_Cat,price,odometer) %>%
#  group_by(year,Price_Cat) %>%
#  summarise(across(price:odometer, ~ mean(.x, na.rm = TRUE))) %>%
#ggplot(aes(x=price, y=odometer, color=Price_Cat)) + 
#  geom_point(size=1) +
#  theme_ipsum()


#-------------------------------------------- 
#Chisquare
#Null hypothesis (H0): There is no significant difference between the observed and the expected value.
#Alternative hypothesis (Ha): There is a significant difference between the observed and the expected value.

#Chisquare for state
#H0 -> Used vehicles listed for each state are equally distributed
#Ha -> Used vehicles listed for each state are not equally distributed
Chi_state <- Vehicle_Data %>%
  select(state) %>%
  group_by(state) %>%
  drop_na() %>%
  summarise(Count = n())


chisq.test(Chi_state$Count,p = rep(1/nrow(Chi_state), nrow(Chi_state)))


#goodness of fit
plot(Vehicle_Data$price, pch=1)
hist(Vehicle_Data$price)
plotdist(Vehicle_Data$price, histo = TRUE, demp = TRUE)

#Price is continuous , so evaluating gamma, m=normal, lognormal, exponential and uniform distributions
fit_g  <- fitdist(Vehicle_Data$price/100, "gamma")
fit_n  <- fitdist(Vehicle_Data$price/100, "norm")
fit_ln <- fitdist(Vehicle_Data$price/100, "lnorm")
fit_e  <- fitdist(Vehicle_Data$price/100, "exp")
fit_u <- fitdist(Vehicle_Data$price/100, "unif")
gofstat(list(fit_g,fit_n,fit_ln,fit_u,fit_e), fitnames = c("gamma", "normal", "lognormal","exponential","uniform"))

#plotting to find the best fit
par(mfrow=c(2,2))
plot.legend <- c("gamma","normal","lognormal","exponential","uniform")
denscomp(list(fit_g,fit_n,fit_ln,fit_u,fit_e), legendtext = plot.legend) #gamma
cdfcomp (list(fit_g,fit_n,fit_ln,fit_u,fit_e), legendtext = plot.legend) #gamma
qqcomp  (list(fit_g,fit_n,fit_ln,fit_u,fit_e), legendtext = plot.legend) #unifrom
ppcomp  (list(fit_g,fit_n,fit_ln,fit_u,fit_e), legendtext = plot.legend) #gamma
#overall gamma is the best fit


#--------------------------------------------
#Statistical Test

#--------------------------------------------
#Advance Analysis