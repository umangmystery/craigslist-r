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
  dplyr::select(id, Price_Cat) %>%
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
  dplyr::select(id, condition) %>%
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
  dplyr::select(id, price, condition) %>%
  group_by(condition) %>%
  drop_na() %>%
  summarise (Average = mean(price)) %>%
  arrange(desc(Average)) %>%
  ggplot(aes(x=Average, y=fct_inorder(condition))) + geom_bar(stat="identity", fill="grey")+ xlab("Average Price") + ylab("Condition of the car")+ geom_text(aes(label = round(Average,3)),hjust=1, vjust = 2.0, colour = "black")+ggtitle("Average Price for each Condition")


#Area graph for Average price for last 30 years
Vehicle_Data %>%
  dplyr::select(id, price, year) %>%
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
  dplyr::select(id, price, manufacturer) %>%
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
  dplyr::select(id,year) %>%
  group_by(year) %>%
  drop_na() %>%
  summarise(Count = n()) %>%
  ggplot(aes(x=year, y=Count)) +
  geom_line(color="black", size=1) +
  theme_light() +
  ggtitle("Number of vehicles listed each year")


#Pie chart showing number of vehicle of each type listed
Vehicle_Data %>%
  dplyr::select(id,price,type) %>%
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
  dplyr::select(id, price, `US STATE`) %>%
  group_by(`US STATE`) %>%
  drop_na() %>%
  summarise(Average = mean(price)) %>%
  arrange(desc(Average))
mapdata$`US STATE` <- tolower(mapdata$`US STATE`)

us_states <- map_data("state")

#for labels
us_states_labels<-us_states %>% 
  left_join(mapdata, by=c("region"="US STATE")) %>%
  dplyr::select(long, lat, group, Average, region ) %>%
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
  dplyr::select(state) %>%
  group_by(state) %>%
  drop_na() %>%
  summarise(Count = n())


chisq.test(Chi_state$Count,p = rep(1/nrow(Chi_state), nrow(Chi_state)))


#goodness of fit
# visualizing the data
ggplot(Vehicle_Data, aes(price)) +
  geom_histogram(bins = 50, color = 'Black', fill = 'steelblue')
plotdist(Vehicle_Data$price, histo = TRUE, demp = TRUE, breaks = 50)

# descriptive statistics
descdist(Vehicle_Data$price)

#Price is continuous , so evaluating gamma, m=normal, lognormal, exponential and uniform distributions
fit_g  <- fitdist(Vehicle_Data$price/100, "gamma")
fit_n  <- fitdist(Vehicle_Data$price/100, "norm")
fit_ln <- fitdist(Vehicle_Data$price/100, "lnorm")
fit_e  <- fitdist(Vehicle_Data$price/100, "exp")
fit_u <- fitdist(Vehicle_Data$price/100, "unif")
gofstat(list(fit_g,fit_n,fit_ln,fit_u,fit_e), fitnames = c("gamma", "normal", "lognormal","uniform","exponential"))

#plotting to find the best fit
par(mfrow=c(2,2))
plot.legend <- c("gamma","normal","lognormal","uniform","exponential")
denscomp(list(fit_g,fit_n,fit_ln,fit_u,fit_e), legendtext = plot.legend)
cdfcomp (list(fit_g,fit_n,fit_ln,fit_u,fit_e), legendtext = plot.legend) 
qqcomp  (list(fit_g,fit_n,fit_ln,fit_u,fit_e), legendtext = plot.legend) 
ppcomp  (list(fit_g,fit_n,fit_ln,fit_u,fit_e), legendtext = plot.legend) 
#overall gamma is the best fit


#--------------------------------------------
#Statistical Tests

#1: one sample Z-test
#Question: Is the population price and the sample price equal?
#H0 -> population mean = sample mean
#H1 -> population mean != sample mean

z.test1 <- function(sample, pop){
  sample_mean = mean(sample) 
  pop_mean = mean(pop)
  n = length(sample) 
  var = var(pop)
  z = (sample_mean - pop_mean) / sqrt((var/(n))) 
  result<-data.frame("Z_calc"=z,"P_value"=pnorm(z))
  return(result)
}
set.seed(100)
vehicleSample <- sample_n(Vehicle_Data, 1000)

sample     = vehicleSample$price
population = Vehicle_Data$price

z.test1(sample, population)
##As P_value > 0.05 we fail to reject the null hypothesis, therefore population price and the sample price are equal
#this forms the basis that further analysis can be done using sample instead of population, since mean values are equal



#2: two sample Z-test
##Question: Is the price of two random samples generated from the data equal or not?
#H0 -> sample mean 1 = sample mean 2 (difference is zero)
#H1 -> sample mean 1 != sample mean 2

z_test2 = function(s1, s2, var_s1, var_s2){
  n_s1 = length(s1)
  n_s2 = length(s2)
  z = ((mean(s1) - mean(s2))-0) / (sqrt((var_s1)/n_s1 + (var_s2)/n_s2))
  result<-data.frame("Zcal"=z,"P_value"=pnorm(z))
  return(result)
}
set.seed(100)
vehicle_1 <- Vehicle_Data[1:round(nrow(Vehicle_Data)/2),] #select all columns and rows from 1 to 49286  
vehicle_2 <- Vehicle_Data[(round(nrow(Vehicle_Data)/2)+1):nrow(Vehicle_Data),] #select all columns and rows from 49287 to 98571
vehicle_1_sample <- sample_n(vehicle_1, 1000) #sample 1000 rows from the first sample
vehicle_2_sample <- sample_n(vehicle_2, 1000) #sample 1000 rows from the second sample

sample1<-vehicle_1_sample$price
sample2<-vehicle_2_sample$price
var_1<-var(vehicle_1_sample$price)
var_2<-var(vehicle_2_sample$price)

z_test2(sample1,sample2,var_1,var_2)
##As P_value > 0.05 we fail to reject the null hypothesis, therefore price of two random samples generated from the data are equal




#3: Two sample t-test
##Question: Is the mean odometer of two random samples generated from the data equal or not?
#H0 -> sample mean 1 = sample mean 2 (difference is zero)
#H1 -> sample mean 1 != sample mean 2

set.seed(100)
vehicle_1 <- Vehicle_Data[1:round(nrow(Vehicle_Data)/2),] #select all columns and rows from 1 to 49286  
vehicle_2 <- Vehicle_Data[(round(nrow(Vehicle_Data)/2)+1):nrow(Vehicle_Data),] #select all columns and rows from 49287 to 98571
vehicle_1_sample <- sample_n(vehicle_1, 1000) #sample 1000 rows from the first sample
vehicle_2_sample <- sample_n(vehicle_2, 1000) #sample 1000 rows from the second sample

sample1<-vehicle_1_sample$odometer
sample2<-vehicle_2_sample$odometer

t.test(x=sample1,y=sample2)
#As P_value > 0.05 we fail to reject the null hypothesis, therefore odometer of two random samples generated from the data are equal




#4 Two sample t-test -
# Question: Compare if the mean price of cars in ca is equivalent to the mean of cars in ny
#H0 -> sample mean 1 = sample mean 2 (difference is zero)
#H1 -> sample mean 1 != sample mean 2

vehicle_ca <- Vehicle_Data %>%
  dplyr::select(id,price,state) %>%
  filter(state == 'ca')

vehicle_ny <- Vehicle_Data %>%
  dplyr::select(id,price,state) %>%
  filter(state == 'ny')

sample1<-vehicle_ca$price
sample2<-vehicle_ny$price

t.test(x=sample1,y=sample2)
#As P_value < 0.05 we reject the null hypothesis, therefore mean price of cars in ca is not equivalent to the mean of cars in ny
#Could be because of income, weather, population etc




#5 Two sample proportion  -
##Question: Comparing the proportion of price of automatic with manual
#H0 -> proportion 1 = proportion 2
#H1 -> proportion 1 != proportion 2

n1 <-length(which(Vehicle_Data$transmission == 'automatic'))
n2 <-length(which(Vehicle_Data$transmission == 'manual'))

vehicle_automatic <- Vehicle_Data %>%
  dplyr::select(id,price,transmission) %>%
  filter(transmission == 'automatic' & price > 50000) %>%
  nrow()

vehicle_manual <- Vehicle_Data %>%
  dplyr::select(id,price,transmission) %>%
  filter(transmission == 'manual' & price > 50000) %>%
  nrow()

prop.test(x=c(vehicle_automatic, vehicle_manual),n=c(n1, n2))
#As P_value < 0.05 we reject the null hypothesis, therefore proportion of price of automatic and manual are not equal
#shows us that proportion of automatic to manual is different, further analysis can be done to know which transmission type is more prefered and y


#6 Ratio of Variances test (two-sample test)- F-test
## Question: Check whether the ratio of variance of odometer of like new condition and new condition is equal 

#H0 -> variance 1 = variance 2
#H1 -> variance 1 != variance 2

vehicle_new <- Vehicle_Data %>%
  dplyr::select(id,odometer,condition) %>%
  filter(condition == 'new')

vehicle_likenew <- Vehicle_Data %>%
  dplyr::select(id,odometer,condition) %>%
  filter(condition == 'like new')

var.test(x=vehicle_new$odometer,y=vehicle_likenew$odometer)
##As P_value < 0.05 we reject the null hypothesis, therefore the ratio of variance of odometer of like new condition and new condition is not equal


#--------------------------------------------
#Advance Analysis