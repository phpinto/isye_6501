# ISyE6501
#HW5
#Group: Artur Cabral, Katie Price, Marta Gaia Bras, Pedro Pinto


knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(tidyverse)
library(knitr)
library(kableExtra)
library(colorspace)
library(corrplot)
library(RColorBrewer)
library(car)
library(caret)
library(LEAP)
library(MASS)

#reading the data 
crime_data <- read.table("uscrime.txt", header = TRUE)

# Overall statistics
summary_table <- crime_data %>% 
  summarize(number_states = nrow(crime_data),
            total_crime_rate = sum(Crime),
            average_crime_rate = mean(Crime),
            min_crime_rate = min(Crime),
            max_crime_rate = max(Crime))

kable(summary_table, caption = "Crime Rate - overall statistics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered"))


# Statistics per state
summary_table_state <- crime_data %>% group_by(So) %>% 
  summarize(number_states = nrow(crime_data),
            total_crime_rate = sum(Crime),
            average_crime_rate = mean(Crime),
            min_crime_rate = min(Crime),
            max_crime_rate = max(Crime))

kable(summary_table_state, caption = "Crime Rate per state") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered"))


# Crime rate per state
## adding an index variable
state <- seq(1, length(crime_data$So))
crime_data_2 <- cbind(state, crime_data)
state <- factor(crime_data_2$state)

ggplot(crime_data_2, aes(x=state, y = Crime)) +
  geom_col(color='darkblue') +
  ggtitle("Crime rate per state") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=18)) 

#Correlations between variables

M <-cor(crime_data)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

distribution of the y variable
# Histogram of crime 
ggplot(crime_data, aes(x=Crime)) + 
  geom_histogram(fill = "darkblue") +
  ggtitle("Histogram of crime rates") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=18)) 

##QQplot graph of crime rate overall
qqPlot(crime_data$Crime, main = "Normal Q-Q plot", xlab = "Crime", ylab = "quantiles") 

# Boxplots of crime rate 
## changing to factor variable to use in the histogram
crime_data$So <- factor(crime_data$So)

ggplot(crime_data, aes(y=Crime)) +
  geom_boxplot(color = "darkblue") +
  ggtitle("Boxplot of crime rate - overall") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=18))

#linear model with all explanatory variables
model <- lm(Crime ~ . , data = crime_data)
#summary model
summary(model)

#plots of the results
plot(model)

#predicting on the new data 
newdata = data.frame(M=14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5, LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1 , Prob = 0.04, Time = 39.0)
newdata$So = as.factor(newdata$So)
predict(model, newdata)

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model_1 <- train(Crime ~., data = crime_data,
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:15),
                      trControl = train.control
)

#results from the mddel for different number of variables
step.model_1$results

#summary
summary(step.model_1$finalModel)


#model with selected variables
model_2 <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = crime_data)

#summary
summary(model_2)

#plot of the results
plot(model_2)

#predicting on the new data 
predict(model_2, newdata)

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(Crime ~., data = crime_data,
                    method = 'lmStepAIC', 
                    trControl = train.control
)

#results from the mddel for different number of variables
step.model$results

#summary
summary(step.model$finalModel)


#plot of the results
plot(step.model$finalModel)

#predicting on the new data 
predict(step.model, newdata)

