#install.packages("devtools")
#devtools::install_github("ropensci/skimr")

#
# install.packages("naniar")
# install.packages("caret")
library(skimr)
library(tidyr)
library(reshape2)
library(tidyverse)
library(dplyr)
library(skimr)
library(naniar)
library(fitdistrplus)
library(caret)

# Importing this package for plotting correlation matrix
source("http://www.sthda.com/upload/rquery_cormat.r")


setwd("C:\\Northeastern\\Probs & Stats\\Project")



# Changing the limit of values to avoid exponential values in df & plots
# options(scipen = 999)



cars_df <- read.csv('vehicles.csv', header = TRUE, sep=",")
head(cars_df, 2)


colnames(cars_df)



new_cars_df <- dplyr::select(cars_df, id, region, price, year, manufacturer,
                      model, condition, cylinders, fuel,
                      odometer, title_status, transmission, drive,
                      size, type, state, posting_date, title_status)

head(new_cars_df, 1)

dim(new_cars_df)

str(new_cars_df)


# CHecking values for title_status
# unique(new_cars_df['title_status'])


#view(new_cars_df)


# Skimming data to get good overview 
df_skimmed <- skim(new_cars_df)


view(df_skimmed)


# Observing the missing values in the dataset
colSums(is.na(new_cars_df))

# Plotting the missing data to get a better idea
vis_miss(new_cars_df, warn_large_data = FALSE)


### PLotting
# Sorting 
new_cars_df_sorted = new_cars_df[order(-new_cars_df$odometer),]
head(new_cars_df_sorted,2)

# Getting rid of outliers for vehicles greater than 700k odometer rating
# new_cars_df_sorted <- new_cars_df_sorted[new_cars_df_sorted$odometer < 700000
#                                          | new_cars_df_sorted$price > 400
#                                          | new_cars_df_sorted$price < 60000,]

new_cars_df_sorted = filter(new_cars_df, (price<60000 & price >600 ) & 
                              odometer < 700000 & (year>1960)) 
new_cars_df_sorted <- filter(new_cars_df_sorted, condition!="", 
                             fuel!="", 
                             transmission!="",
                             cylinders!="",
                             title_status!="")


max(new_cars_df_sorted$odometer)
min(new_cars_df_sorted$odometer)
min(new_cars_df_sorted$price)

max(new_cars_df_sorted$price)

new_cars_df_sorted$odometer[is.na(new_cars_df_sorted$odometer)] <- 0

ggplot(new_cars_df_sorted, aes(x=year))+
  geom_histogram(bins=50, stat="count")


# We can see that year < 1950 is not relevant 
# Hence we filter it out
new_cars_df_sorted = filter(new_cars_df_sorted, (year>1950))

ggplot(new_cars_df_sorted, aes(x=year))+
  geom_histogram(bins=100, stat="count", position="dodge")


# Box plot helps us visually get a sense of outliers in the dataset
ggplot(new_cars_df_sorted, aes(x="", y = odometer)) + 
  geom_boxplot(fill="#0c4c8a") +
  labs(y = "Odometer in Thousands") +
  scale_y_continuous( labels = function(x) x / 1000) + # Dividing values by 1000 
  theme_minimal() 





# WE can see that there are still many outliers.
# Using IQR to remove outliers is not effective 
# Hence use percentile to determine the 
#   the upper and lower 2.5 percentile 
# 
# lower_bound <- quantile(na.omit(new_cars_df_sorted$odometer), 0.025)
# lower_bound
# 
# upper_bound <- quantile(na.omit(new_cars_df_sorted$odometer), 0.975)
# upper_bound
# 
# outlier_index <- which(new_cars_df_sorted$odometer < lower_bound | 
#                          new_cars_df_sorted$odometer > upper_bound)

#count(new_cars_df_sorted[outlier_index,])
#count(new_cars_df_sorted)

# 
# clean_cars_df<- new_cars_df_sorted[!seq_len(nrow(new_cars_df_sorted)) 
#                                    %in% outlier_index, ]
# count(clean_cars_df,2)
# 
# 
# view(head(clean_cars_df[order(-clean_cars_df$odometer),],1000))

# Plotting box plot again will confirm that we removed the outliers correctly
ggplot(new_cars_df_sorted, aes(x="", y = odometer)) + 
  geom_boxplot(fill="#0c4c8a") +
  labs(y = "Odometer in Thousands") +
  scale_y_continuous( labels = function(x) x / 1000) + # Dividing values by 1000 
  theme_minimal() 

ggplot(new_cars_df_sorted, aes(x="", y = price)) + 
  geom_boxplot(fill="#0c4c8a") +
  labs(y = "Price") +
  #scale_y_continuous( labels = function(x) x / 1000) + # Dividing values by 1000 
  theme_minimal()


new_cars_df_sorted %>% 
  dplyr::select(fuel, price) %>% 
  group_by(fuel) %>% 
  drop_na() %>% 
  summarise(Avg = mean(price)) %>% 
  arrange(desc(Avg)) %>% 
  ggplot(aes(x=Avg, y=fuel)) + 
  geom_bar(stat="identity", fill="dark green") +
  xlab("Fuel Type") + ylab("Avg Price")



new_cars_df_sorted %>% 
  dplyr::select(cylinders, price) %>% 
  group_by(cylinders) %>% 
  drop_na() %>% 
  arrange(desc(cylinders)) %>% 
  summarise(Avg = mean(price)) %>% 
  arrange(desc(Avg)) %>% 
  ggplot(aes(x=Avg, y=cylinders)) + 
  geom_bar(stat="identity", fill="dark green") +
  xlab("Number of Cylinders") + ylab("Avg Price")


# Plotting odometer rating wrt car manufacturers
# ggplot(clean_cars_df, aes(manufacturer, odometer / 1000)) +
#   geom_bar(stat="identity") + 
#   labs(y = "Odometer in Thousands") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
#   scale_y_continuous( labels = function(x) x / 1000) # Dividing values by 1000 

# clean_cars_df 

# Here we can see there are several Outlier in the price as well
# We will use the same method to remove the outliers
 
# 
# lower_bound2 <- quantile(na.omit(clean_cars_df$price), 0.025)
# lower_bound2
# 
# upper_bound2 <- quantile(na.omit(clean_cars_df$price), 0.975)
# upper_bound2
# 
# outlier_index2 <- which(clean_cars_df$price < lower_bound2 |
#                          clean_cars_df$price > upper_bound2)
# 
# count(clean_cars_df[outlier_index2,])
# view(head(clean_cars_df[order(-clean_cars_df$price),],1000))
# 
# 
# 
# ######
# price_outliers <- boxplot.stats(clean_cars_df$price)$out
# price_out_index <- which(clean_cars_df$price %in% c(price_outliers))
# 
# count(clean_cars_df[price_out_index, ])
# count(clean_cars_df)
# 
# clean_cars_df<- clean_cars_df[!seq_len(nrow(clean_cars_df)) 
#                                    %in% outlier_index, ]



## 

ggplot(new_cars_df_sorted, aes(x=year, y=price, color=odometer)) +
  geom_point() +
  scale_color_gradientn(colours = rainbow(5))


ggplot(new_cars_df_sorted, aes(price))+
  geom_histogram(bins=50, fill="grey")



# Distribution 
descdist(new_cars_df_sorted$price, discrete = FALSE)

fit_n <- fitdist(new_cars_df_sorted$price, "norm")
summary(fit_n)


fit_g <- fitdist(new_cars_df_sorted$price/1000, "gamma")
summary(fit_g)
plot(fit_g)

fit_ln <-fitdist(new_cars_df_sorted$price, "lnorm")
summary(fit_ln)


# Gamma is a good fit
no_of_cy <- new_cars_df_sorted$cylinders
head(no_of_cy)
no_of_cy <- as_factor(no_of_cy)
new_cars_df_sorted$cylinders <- as.numeric(no_of_cy)
head(new_cars_df_sorted)

cond <-as_factor(new_cars_df_sorted$condition)
new_cars_df_sorted$condition <- as.numeric(cond)
head(new_cars_df_sorted)


status <- as_factor(new_cars_df_sorted$title_status)
new_cars_df_sorted$title_status <- as.numeric(status)
head(new_cars_df)

trans <- as_factor(new_cars_df_sorted$transmission)
new_cars_df_sorted$transmission <- as.numeric(trans)
head(new_cars_df_sorted)

fuel <- as_factor(new_cars_df_sorted$fuel)
new_cars_df_sorted$fuel <- as.numeric(fuel)
head(new_cars_df_sorted)


rquery.cormat(new_cars_df_sorted  %>% select_if(is.numeric))


df_learn <- dplyr::select(new_cars_df_sorted, price, cylinders, condition,
                          transmission, title_status, fuel, year, odometer)
head(df_learn)


# Set seed and split data in train and test

#Splitting into Train & Test Data
set.seed(50)
partition <- createDataPartition(y = df_learn$price, p=0.7, list=FALSE)
trainingdata = df_learn[partition,]
test <- df_learn[-partition,]

# Splitting Training and Validatation data
set.seed(50)
partition_training <- createDataPartition(y=trainingdata$price, p=0.8, list=FALSE)
training <- trainingdata[partition_training,]
validation <- trainingdata[-partition_training,]



model <- lm(price ~ odometer + cylinders + transmission + year +
            title_status + fuel + condition, data=training)

summary(model)
plot(model)

p <- predict(model, validation)
error <- (p- validation$price)
RMSE_Model <- sqrt(mean(error^2))

ptest <- predict(model, test)
error2 <- (ptest - test$price)
RMSE_New <- sqrt(mean(error2^2))

method <- c("Test Train Split")
ModelRMSE <- c(RMSE_Model)
NewData_RMSE <- c(RMSE_New)

error_table <- data.frame(method, ModelRMSE, NewData_RMSE)

error_table






