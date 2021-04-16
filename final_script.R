# install.packages("treemapify")
#install.packages("maps")
#install.packages("readxl")
# install.packages("e1071")


library(skimr)
library(tidyr)
library(reshape2)
library(tidyverse)
library(dplyr)
library(skimr)
library(naniar)
library(fitdistrplus)
library(caret)
library(devtools)
library(treemapify)
library(maps)
library(readxl)
library(lubridate)
library(e1071)


# Importing this package for plotting correlation matrix
source("http://www.sthda.com/upload/rquery_cormat.r")

# Set the directory to where the raw data and state names files are located
setwd("C:\\Northeastern\\Probs & Stats\\Project")


# Importing the dataset
cars_df <- read.csv('vehicles.csv', header = TRUE, sep=",")
head(cars_df, 2)
StateName <- read_xlsx("StateNames.xlsx")

colnames(cars_df)


# Filtering out important columns
cars_df <- dplyr::select(cars_df, id, region, price, year, manufacturer,
                             model, condition, cylinders, fuel,
                             odometer, title_status, transmission, drive,
                             size, type, state, posting_date, title_status, state)


head(cars_df, 2)

# Skimming data to get good overview 
df_skimmed <- skim(cars_df)


# Observing the missing values in the dataset
colSums(is.na(cars_df))

# Plotting the missing data to get a better idea
vis_miss(cars_df, warn_large_data = FALSE)

# Sorting WRT odometer 
cars_df = cars_df[order(-cars_df$odometer),]
head(cars_df,2)

# Odometer and price are 2 key columns. Hence visualising the spread of data
ggplot(cars_df, aes(x="", y = odometer)) + 
  geom_boxplot(fill="#0c4c8a") +
  labs(y = "Odometer in Thousands") +
  scale_y_continuous( labels = function(x) x / 1000) + # Dividing values by 1000 
  theme_minimal() 

ggplot(cars_df, aes(x="", y = price)) + 
  geom_boxplot(fill="#0c4c8a") +
  labs(y = "Odometer in Thousands") +
  scale_y_continuous( labels = function(x) x / 1000) + # Dividing values by 1000 
  theme_minimal() 

# Getting rid of outliers for odometer rating and price
# Only 1% of the dataset has price listed above $60000

cars_df = filter(cars_df, (price<60000 & price >600 ) & 
                              odometer < 700000 & (year>1960)) 
cars_df <- filter(cars_df, condition!="", 
                             fuel!="", 
                             transmission!="",
                             cylinders!="",
                             title_status!="",
                             price!="",type!="")

vis_miss(cars_df, warn_large_data = FALSE)
# Here we can see all missing values are removed

# Cleaning more columns 
cars_df$odometer[is.na(cars_df$odometer)] <- 0
cars_df$odometer[is_empty(cars_df$odometer)] <- 0


cars_df <- cars_df[!is.na(cars_df$odometer),]
cars_df <- cars_df[!is.na(cars_df$year),]
cars_df <- cars_df[!is.na(cars_df$title_status),]
cars_df <- cars_df[!is.na(cars_df$transmission),]
cars_df <- cars_df[!is.na(cars_df$fuel),]
cars_df <- cars_df[!is.na(cars_df$type),]
cars_df <- cars_df[!is.na(cars_df$drive),]
cars_df <- cars_df[!is.na(cars_df$condition),]
cars_df <- cars_df[!is.na(cars_df$size),]
cars_df <- cars_df[!is.na(cars_df$cylinders),]


head(cars_df,2)


# Getting an idea of how the cars are listed as per year
ggplot(cars_df, aes(x=year))+
  geom_histogram(bins=50, stat="count")


# There very few cars listed before 1950
cars_df <- filter(cars_df, (year>1950))

#Getting full State Names
cars_df <- left_join(cars_df, StateName, by = c("state"="ShortForm"))

#Creating 6 bins/categories for Price
cars_df$Price_Cat <- cut(cars_df$price, breaks = c(
  0,5000,10000,15000,20000,40000,max(cars_df$price)), 
  labels = c("0-5k", "5-10k", "10-15k","15-20k","20-40k","VeryHigh"))


## Data Visualizations
################-----------

# Visualizing the distibution and variability of Odometer
ggplot(cars_df, aes(x="", y = odometer)) + 
  geom_boxplot(fill="#0c4c8a") +
  labs(y = "Odometer in Thousands") +
  scale_y_continuous( labels = function(x) x / 1000) + # Dividing values by 1000 
  theme_minimal() 


# Visualizing the distibution and variability of Price
ggplot(cars_df, aes(x="", y = price)) + 
  geom_boxplot(fill="#0c4c8a") +
  labs(y = "Price") +
  theme_minimal()

# Histogram of the price and count
ggplot(cars_df, aes(x=price)) + 
  geom_histogram(fill="red",color="white",alpha=0.7,bins = 20)+
  ggtitle("Price Distribution")


cars_df %>% 
  dplyr::select(fuel, price) %>% 
  group_by(fuel) %>% 
  drop_na() %>% 
  summarise(Avg = mean(price)) %>% 
  arrange(desc(Avg)) %>% 
  ggplot(aes(x=Avg, y=fuel)) + 
  geom_bar(stat="identity", fill="dark green") +
  xlab("Fuel Type") + ylab("Avg Price")



cars_df %>% 
  dplyr::select(cylinders, price) %>% 
  group_by(cylinders) %>% 
  drop_na() %>% 
  arrange(desc(cylinders)) %>% 
  summarise(Avg = mean(price)) %>% 
  arrange(desc(Avg)) %>% 
  ggplot(aes(x=Avg, y=cylinders)) + 
  geom_bar(stat="identity", fill="dark green") +
  xlab("Number of Cylinders") + ylab("Avg Price")


#bar graph for price based on condition of the car
cars_df %>%
  dplyr::select(id, price, condition) %>%
  group_by(condition) %>%
  drop_na() %>%
  summarise (Average = mean(price)) %>%
  arrange(desc(Average)) %>%
  ggplot(aes(x=Average, y=fct_inorder(condition))) + geom_bar(stat="identity", fill="grey")+ 
  xlab("Average Price") + ylab("Condition of the car")+ 
  geom_text(aes(label = round(Average,3)),hjust=1, vjust = 2.0, colour = "black")+
  ggtitle("Average Price for each Condition")


#Area graph for Average price for last 30 years
cars_df %>%
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
cars_df %>%
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
cars_df %>%
  dplyr::select(id,year) %>%
  group_by(year) %>%
  drop_na() %>%
  summarise(Count = n()) %>%
  ggplot(aes(x=year, y=Count)) +
  geom_line(color="black", size=1) +
  theme_light() +
  ggtitle("Number of vehicles listed each year")


#Pie chart showing number of vehicle of each type listed
cars_df %>%
  dplyr::select(id,price,type) %>%
  group_by(type) %>%
  drop_na() %>%
  summarise(Count = n()) %>%
  ggplot(aes(x="", y=Count, fill=type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + theme_void() +
  ggtitle("Number of vehicle of each type listed")+
  scale_fill_manual(values=c("#551A8B", "#E69F00", "#56B4E9","#00008B", "#00611C", "#00EE00","#2F2F4F", "#E35152", "#999999","#F08080", "#EEEE00", "#8B5A00","#55141C", "#000000"))

## REVIEW
#Map to show price for each state
mapdata <- cars_df %>%
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


#--------------------------------------------
#Statistical Analysis
#mean
mean_price <- mean(cars_df$price)
mean_price
mean_odometer <- mean(cars_df$odometer)
mean_odometer

#median
median_price <- median(cars_df$price)
median_price
median_odometer <- median(cars_df$odometer)
median_odometer

#min and Max
min_price <- min(cars_df$price)
min_price
max_price <- max(cars_df$price)
max_price

min_odometer <- min(cars_df$odometer)
max_odometer <- max(cars_df$odometer)

#Range
range_price <- range(cars_df$price)
range_price
range_odometer <- range(cars_df$odometer)
range_odometer

#Variance
variance_price <- var(cars_df$price)
variance_price

variance_odometer <- var(cars_df$odometer)
variance_odometer

#Standard Variance
sd_price <- sd(cars_df$price)
sd_price
sd_odometer <- sd(cars_df$odometer)
sd_odometer

#Coefficient of Variation 
cov_price <- (sd_price/mean_price)*100
cov_price
cov_odometer <- (sd_odometer/mean_odometer)*100
cov_odometer

#Skewness
skewness_price <- skewness(cars_df$price)
skewness_price
skewness_odometer <- skewness(cars_df$odometer) #highly skewed
skewness_odometer

#Kurtosis
kurtosis_price <- kurtosis(cars_df$price)
kurtosis_price
kurtosis_odometer <- kurtosis(cars_df$odometer)
kurtosis_odometer




# Probability 
#--------------------------------------------
# PDF & CDF Caclulation
# Frequency of used vehicles based on price
price_freq <- cars_df %>%
  #select(id, Price_Cat) %>%
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
cond_freq <- cars_df %>%
  #select(id, condition) %>%
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



## Goodness of Fit
###-----------------

# visualizing the data
ggplot(cars_df, aes(price)) +
  geom_histogram(bins = 50, color = 'Black', fill = 'steelblue')
plotdist(cars_df$price, histo = TRUE, demp = TRUE, breaks = 50)

# descriptive statistics
descdist(cars_df$price)

#Price is continuous , so evaluating gamma, m=normal, lognormal, exponential and uniform distributions
fit_g  <- fitdist(cars_df$price/100, "gamma")
fit_n  <- fitdist(cars_df$price/100, "norm")
fit_ln <- fitdist(cars_df$price/100, "lnorm")
fit_e  <- fitdist(cars_df$price/100, "exp")
fit_u <- fitdist(cars_df$price/100, "unif")
gofstat(list(fit_g,fit_n,fit_ln,fit_u,fit_e), fitnames = c("gamma", "normal", "lognormal","uniform","exponential"))

#plotting to find the best fit
par(mfrow=c(2,2))
plot.legend <- c("gamma","normal","lognormal","uniform","exponential")
denscomp(list(fit_g,fit_n,fit_ln,fit_u,fit_e), legendtext = plot.legend)
cdfcomp (list(fit_g,fit_n,fit_ln,fit_u,fit_e), legendtext = plot.legend) 
qqcomp  (list(fit_g,fit_n,fit_ln,fit_u,fit_e), legendtext = plot.legend) 
ppcomp  (list(fit_g,fit_n,fit_ln,fit_u,fit_e), legendtext = plot.legend) 
#overall gamma is the best fit



#Statistical Tests
#--------------------------------------------

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
vehicleSample <- sample_n(cars_df, 1000)

sample     = vehicleSample$price
population = cars_df$price

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
vehicle_1 <- cars_df[1:round(nrow(cars_df)/2),] 
vehicle_2 <- cars_df[(round(nrow(cars_df)/2)+1):nrow(cars_df),] 
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
vehicle_1 <- cars_df[1:round(nrow(cars_df)/2),]   
vehicle_2 <- cars_df[(round(nrow(cars_df)/2)+1):nrow(cars_df),] 
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

vehicle_ca <- cars_df %>%
  dplyr::select(id,price,state) %>%
  filter(state == 'ca')

vehicle_ny <- cars_df %>%
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

n1 <-length(which(cars_df$transmission == 'automatic'))
n2 <-length(which(cars_df$transmission == 'manual'))

vehicle_automatic <- cars_df %>%
  dplyr::select(id,price,transmission) %>%
  filter(transmission == 'automatic' & price > 50000) %>%
  nrow()

vehicle_manual <- cars_df %>%
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

vehicle_new <- cars_df %>%
  dplyr::select(id,odometer,condition) %>%
  filter(condition == 'new')

vehicle_likenew <- cars_df %>%
  dplyr::select(id,odometer,condition) %>%
  filter(condition == 'like new')

var.test(x=vehicle_new$odometer,y=vehicle_likenew$odometer)
##As P_value < 0.05 we reject the null hypothesis, therefore the ratio of variance of odometer of like new condition and new condition is not equal




# Advanced Analytics
### -----------------


# Converting categorical variables to numeric values 
no_of_cy <- cars_df$cylinders
head(no_of_cy)
no_of_cy <- as_factor(no_of_cy)
cars_df$cylinders <- as.numeric(no_of_cy)
head(cars_df)

cond <-as_factor(cars_df$condition)
cars_df$condition <- as.numeric(cond)
head(cars_df)


status <- as_factor(cars_df$title_status)
cars_df$title_status <- as.numeric(status)
head(cars_df)

trans <- as_factor(cars_df$transmission)
cars_df$transmission <- as.numeric(trans)
head(cars_df)

fuel <- as_factor(cars_df$fuel)
cars_df$fuel <- as.numeric(fuel)
head(cars_df)

# Plotting the correlation matrix to identify the features to train the model
rquery.cormat(cars_df  %>% select_if(is.numeric))

# Creating a new DF for training and testing the model
df_learn <- dplyr::select(cars_df, price, cylinders, condition,
                          transmission, title_status, fuel, year, odometer)
head(df_learn)


# Set seed and split data in train and test
#Splitting into Train & Test Data
set.seed(837)
partition <- createDataPartition(y = df_learn$price, p=0.7, list=FALSE)
trainingdata = df_learn[partition,]
test <- df_learn[-partition,]

# Splitting Training and Validation data
set.seed(837)
partition_training <- createDataPartition(y=trainingdata$price, p=0.8, list=FALSE)

training <- trainingdata[partition_training,]
validation <- trainingdata[-partition_training,]


# Training the Linear Regression Model
set.seed(837)
model <- lm(price ~ odometer + cylinders + transmission + year +
              title_status + fuel + condition, data=training)

summary(model)
plot(model)
# R Squared value
summary(model)$r.squared

# Calculating the error vs the validation data
validation$predict <- predict(model, validation)
validation$error <- (validation$predict - validation$price)
RMSE_Model_validation <- sqrt(mean(validation$error^2))

# Calculating the error vs the testing data 
test$predict <- predict(model, test)
test$error <- (test$predict - test$price)
RMSE_Model_testing <- sqrt(mean(test$error^2))

# Creating error and accuracy table 
method <- c("Test Train Split")
Validation_RMSE <- c(RMSE_Model_validation)
Testing_RMSE <- c(RMSE_Model_validation)
r_sqd <- c(summary(model)$r.squared)

error_table <- data.frame(method, Validation_RMSE, Testing_RMSE, r_sqd)

error_table

# Plotting the predicted value vs the actual value to visualize how well the model was fitted
test %>%
  dplyr::select(price,predict) %>%
  ggplot(aes(x=price, y=predict))+geom_point(size=1)+geom_abline(color="red")

 




