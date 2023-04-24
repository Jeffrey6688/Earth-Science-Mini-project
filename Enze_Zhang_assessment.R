### Project title: Daily temperature forecasting using the Prophet model
### Name: Enze Zhang

# Load the data
setwd("~/Desktop/temp prediction")  # Set your location
temp <- read.csv("durhamtemp_1901_2019.csv")
temp <- na.omit(temp)

# Data summary
summary(temp)
temp[which(temp$Av.temp == -9), ] # min temp date
temp[which(temp$Av.temp == 24.4), ] # max temp date
hist(temp$Av.temp)
cor(temp$Av.temp, temp$PPT.) # correlation between temp and ppt.

# Time series
temp.ts <- ts(data=temp$Av.temp, start=c(1901,1), frequency=365.25)
plot.ts(temp.ts) # time series plot
temp.decompose <- decompose(temp.ts) # time series decompose
plot(temp.decompose)

### Prophet Model
#install.packages('prophet')
library(prophet)

# Construct data frame for prophet 
ds <- as.Date(temp$Date, format="%d/%m/%Y")
y <- temp$Av.temp
temp.df <- data.frame(ds,y)

### Prophet model fitting

# Daily temperature predictions for 2020
temp.ts.prophet <- prophet(temp.df,
                           yearly.seasonality = TRUE, 
                           weekly.seasonality = FALSE,
                           daily.seasonality = FALSE,
                           seasonality.mode = "additive",
                           changepoint.prior.scale = 0.05,
                           interval.width = 0.95 # 95% confidence interval
)
future <- make_future_dataframe(temp.ts.prophet, periods = 366) # 2020 is a leap year
forecast <- predict(temp.ts.prophet, future) # Make predictions
dyplot.prophet(temp.ts.prophet,forecast) # Interactive forecasting plot
prophet_plot_components(temp.ts.prophet, forecast) # Components plot

##### Back-testing / Model performance evaluation
# The function "fit_prophet" predicts the daily temperature one year forward and evaluates the performance.
# The function parameter "date" is the date separates the training and testing set.

fit_prophet <- function(date){
  temp.df <- temp.df[temp.df$ds <= as.Date(date, format="%d/%m/%Y") + 365, ]
  temp.df.train <- temp.df[temp.df$ds <= as.Date(date, format="%d/%m/%Y"), ] # Training set
  temp.df.test <- temp.df[temp.df$ds > as.Date(date, format="%d/%m/%Y"), ] # Testing set
  temp.train.prophet <- prophet(temp.df.train,
                                yearly.seasonality = TRUE,
                                weekly.seasonality = FALSE,
                                daily.seasonality = FALSE,
                                seasonality.mode = "additive",
                                changepoint.prior.scale = 0.05,
                                interval.width = 0.95 
  )
  future.train <- make_future_dataframe(temp.train.prophet, periods = nrow(temp.df.test)) 
  forecast.train <- predict(temp.train.prophet, future.train) # Make predictions
  rmse.train <- sqrt(mean((tail(forecast.train$yhat, 365) - temp.df.test$y)^2)) # Calculate RMSE
  mae.train <- mean(abs(temp.df.test$y - tail(forecast.train$yhat, 365))) # Calculate MAE
  cat("Performance:", rmse.train, mae.train)
}

# Model performance for 2015 - 2019
# Note: Due to vast amount of historical data, the code below may require some time to execute.
split_dates <- c("31/12/2018", "31/12/2017", "31/12/2016", "31/12/2015", "31/12/2014")
for (date in split_dates){
  fit_prophet(date)
}


