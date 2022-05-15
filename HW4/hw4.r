# Homework 4
# Artur Cabral, Marta Bras, Pedro Pinto, Katie Price

library(ggplot2)
library(kernlab)
library(fpp2)
library(tidyverse)
library(knitr)
library(kableExtra)
library(colorspace)
library(lubridate)
library(tidyr)

setwd("C:/Users/pedro/Documents/Georgia Tech/M.S. Analytics/Classes/Fall 2019/ISYE 6501/Homeworks/ISyE6501/hw4")

# Question 7.1

# Cellphone manufacturers such as Apple or Samsumg, are constantly trying to improve the battery life of their devices. 
# They could study how users' use of the device can impact the deterioration of battery life. 
# For users who agree to share their usage data, the company would select a sample, and record screen time for app usage, and charging time of the devices every day. 
# To eliminate randomness and variability of battery life, all devices in the sample should be purchased at the same day.   
# For each type of device, the company would build an eponential smoothing model with screen time for app usage and daily charging time. 
# The data could include cyclic effects (such as times of the day the user utilises the phone more often, or days in the week where the usage is also increased) 
# and the trend would show which type of usage has a higher impact battery life. Combining the CUSUM with exponential smoothing models, 
# it would be possible to constantly monitor usage behavior that result in battery deterioration, and forecast how long it would take for a device to have its baterry life decreased.

# Question 7.2

#reading the data 
t_data <- read.table("temps.txt", header = TRUE)

#creating a date column
date <- t_data %>% gather(X, Temp, X1996:X2015)
date$X <- gsub("^.{0,1}", "", date$X)
date <- date %>% mutate(x=str_c(DAY,"-",X))
date[,4] <- as.Date(date[,4], format = "%d-%b-%Y")
colnames(date) = c("Day", "Year", "Temperature", "Date")

#plotting tempeature over time
ggplot(date, aes(x=Date, y=Temperature)) +
  geom_line(color="steelblue4", lwd=0.4) +
  ggtitle("Daily temperature over the years (1996-2015)") +
  ylab("temperature (Fº)") +
  xlab("date") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=18)) 

#saving data as vector
years <- as.vector(unlist(t_data[,2: length(t_data)]))

#converting data to time series
ts <- ts(years,frequency = 123)

#decomposition between trend and seasonality
autoplot(decompose(ts))

# Looking at the decomposition of the time series data between seasonality and trend, we can see that there is high seasonality in the data. This means that from July to October, some days have much higher temperatures than others.

#As we had concluded in the previous assignment, the trend in the daily temperatures is inconclusive, with a visible positive trend around 2010, followed by a clear negative trend around 2013.

# HoltWinters Models

#Simple Exponential Smoothing:
smooth_1 <- HoltWinters(ts,beta = FALSE, gamma = FALSE)

#plot of smooth_1
plot(smooth_1, main = "Simple exponential smoothing")

# Double Exponential Smoothing with trend:s
smooth_2 <- HoltWinters(ts,gamma = FALSE)

#plot of smooth_2
plot(smooth_2,  main = "Double exponential smoothing")

#Triple Exponential Smoothing with trend with additive seasonality:
smooth_3 <- HoltWinters(ts)

#plot of smooth_3
plot(smooth_3,  
     main = "Triple exponential smoothing with trend with additive seasonality")

#Triple Smoothing with trend with multiplicative seasonality:
smooth_4 <- HoltWinters(ts, seasonal="multiplicative")

#plot of smooth_4
plot(smooth_4,  main = "Triple exponential with multiplicative seasonality")

#decomposition  of smooth_4 - level, trend and seasonality
plot(fitted(smooth_4),  main = "Multiplicative seasonality - Decomposition")

#coefficients of smooth_4
smooth_4_coefficients <- (coefficients(smooth_4)[3:125])

plot(smooth_4_coefficients, main = "Multiplicative seasonality - Coefficients",
     col ="steelblue4")


#getting the seasonality estimates from the multiplicative model
fitted <- as.data.frame(smooth_4$fitted[,4])

#filtering the date column (day-month and date) from the original dataseet 
#to start in 1997 because the multiplicative model uses the first period 
#(123 days) to estimate the parameters for the following years.
date_2 <- date$Date
day <- date$Day

date_2 <- date_2[124:length(date_2)]
day <- day[124:length(day)]

#adding date column to the fitted values
fitted <- cbind(date_2, fitted, day)

#plotting the seasonality pattern over the years
#adding a confidence interval 
fitted$mino3<-fitted$x-sd(fitted$x, na.rm=T)
fitted$maxo3<-fitted$x+sd(fitted$x, na.rm=T)

ggplot(fitted, aes(x=date_2, y=x)) + 
  geom_ribbon(aes(ymin=mino3, ymax=maxo3), fill="snow3", color="snow3")+
  geom_line(color="steelblue4", lwd=0.8)+
  ggtitle("Smoothed seasonality pattern over the years") +
  ylab("Seasonality pattern") +
  xlab("date") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=18)) 

#removing max and min columns
fitted <- fitted[-c(4,5)]

#creating average for each day
parameters <- fitted %>% group_by(day) %>% summarize(mean = mean(x), stdd = sd(x))


#adding everything to the same database
fitted <- left_join(fitted,  parameters, by ="day")

#initiating an empty matrix to store the CUSUM
CUSUM_matrix= matrix(nrow=length(fitted$day), ncol = 1)

st <- 0


#comptuing the CUSUM function
for (i in (1: length(fitted$day))) {
  CUSUM_matrix[i,1] = max(0, (st + (fitted[[i,4]] - fitted[[i,2]] - fitted[[i,5]])))
  st <- CUSUM_matrix[i,1]
}


#adding the results to a dataframe with the date
CUSUM_matrix_2 <- as.data.frame(CUSUM_matrix)
CUSUM_matrix_2 <- cbind(fitted$date_2, CUSUM_matrix_2, fitted$day) 


#Boxplot of results
ggplot(CUSUM_matrix_2, aes(y=V1)) +
  geom_boxplot() +
  ggtitle("CUSUM - boxplot") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=18)) 


#CUSUM plot  
ggplot(CUSUM_matrix_2, aes(x=fitted$date_2, y=V1)) +
  geom_line(color="steelblue4", lwd=0.8) +
  ggtitle("CUSUM - plot") +
  ylab("Cum sum value") +
  xlab("date") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=18)) 


#adding year and month to the cusum matrix
CUSUM_matrix_2 <- CUSUM_matrix_2 %>% 
  mutate(year = year(fitted$date_2), month = month((fitted$date_2)), day_number = day(fitted$date_2))

#computing the mean and standard deviation
mean <- CUSUM_matrix_2 %>% group_by(fitted$day) %>% summarize(mean = mean(V1), standard_dev = sd(V1))

#adding the mean and standard deviation to the cusum matrix
CUSUM_matrix_2 <- left_join(CUSUM_matrix_2, mean, by = "fitted$day")

#finding the values that are a standard dev away from the mean
results <- CUSUM_matrix_2 %>% filter(month > "8") %>% group_by(year) %>% mutate(diff = V1 - (mean - standard_dev)) %>% arrange(desc(diff)) %>% top_n(1) %>% arrange(desc(year))
colnames(results) <- c("date", "cusum", "day_month", "year", "month", "day_number", "mean", "standard_dev", "diff")

#table with the last day of summer per year

last_day <- results %>% select(year, day_month,day_number)

kable(last_day, caption = "Last day of Summer over the years") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered"))

#plotting results
ggplot(last_day, aes(x = year, y = day_number)) +
  geom_point(color="steelblue4", lwd=2) +
  ggtitle("Last day of Summer over the years") +
  ylab("Day in September") +
  xlab("year") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=18)) 



