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
library(glmnet)
library(Metrics)

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

#distribution of the y variable
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

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(Crime ~., data = crime_data,
                    method = "leapForward", 
                    tuneGrid = data.frame(nvmax = 1:15),
                    trControl = train.control
)

#results from the mddel for different number of variables
step.model$results

#summary
summary(step.model$finalModel)


#model with selected variables
model <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = crime_data)
#summary
summary(model)

#plot of the results
plot(model$residuals)
qqPlot(model$residuals, main = "Normal Q-Q plot", xlab = "Residuals", ylab = "quantiles") 

# Set seed for reproducibility
set.seed(123)

# Train the model
step.model_2 <- train(Crime ~., data = crime_data,
                      method = 'lmStepAIC', 
                      trControl = train.control
)


#results from the mddel for different number of variables
step.model_2$results

#summary
summary(step.model_2$finalModel)


#converting data to dataframe and scaling
crime_data <- read.table("uscrime.txt", header = TRUE)

crime_data_matrix <- as.matrix(scale(crime_data))

predictors <- crime_data_matrix[,1:15]
response <-  crime_data_matrix[,16]


#Using cross validation for the Lasso regression
model_lasso <- cv.glmnet(predictors, response, alpha = 1)

#Finding optimal value of lambda that minimizes cross-validation errors
plot(model_lasso)

model_lasso$lambda.min

model_lasso$lambda.1se

coef(model_lasso, model_lasso$lambda.1se)

# Split data into train (70%) and test (30%)
crime_data_matrix_2 <- as.matrix((crime_data))

ind <- sample(1:nrow(crime_data_matrix),floor(nrow(crime_data_matrix)*0.70)) 
crime_data_matrix_train <- crime_data_matrix[ind,]
crime_data_matrix_test <- crime_data_matrix[-ind,]

yhat <- predict(model_lasso, s=model_lasso$lambda.min, newx=crime_data_matrix_test[,1:15])
MSE <- mean((crime_data_matrix_test[,16] - yhat)^2)
MSE

mse_array <- numeric()
find_alpha <- function(num, scaled_data){
  alpha <- num
  elastic_net <- cv.glmnet(predictors,
                           response,
                           alpha = alpha,
                           nfolds=5,
                           type.measure="mse",
                           family="gaussian",
                           standardize=FALSE)
  
  mse_array <<- cbind(mse_array, c(alpha, min(elastic_net$cvm),elastic_net$lambda.min))    
}

# Loop over different values of alpha

for (i in seq(.01,1,by = .01)){find_alpha(i,crime_data_matrix)}

minIdx <- which.min(mse_array[2,])
mse_array[2,minIdx]
mse_array[1, minIdx]

elastic_result <- cv.glmnet(predictors,
                            response,
                            alpha = 0.8,
                            nfolds=5,
                            type.measure="mse",
                            family="gaussian",
                            standardize=FALSE)

coef(elastic_result, s = elastic_result$lambda.min)

#So elastic_net selects 14 variables. Let's rerun lm using those features.

elastic.lm <- lm(Crime ~ ., data = as.data.frame(crime_data_matrix[, -4]))

summary(elastic.lm)


