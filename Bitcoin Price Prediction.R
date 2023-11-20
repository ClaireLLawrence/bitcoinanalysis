# Import the tidyverse library:
library(tidyverse)
library(forecast)
library(tseries)
library(plotly)


# Import a CSV file:
bitdat <- read.csv('BTC-USD.csv', header = TRUE)

# Sense-check the data:
bitdat
view(bitdat)
str(bitdat)
dim(bitdat)
typeof(bitdat)
class(bitdat)
# There are 3343 rows and 7 columns. Date is chr, the rest num.

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
bitdat2 <- select(bitdat, -Open, -High, -Low, -Adj.Close, -Volume)
view(bitdat2)

# View the descriptive statistics.
summary(bitdat2)

# Change the format of the Date column from char to date:

?strptime

bitdat2$Date <- as.Date(bitdat2$Date, format = '%Y-%m-%d')
                  
view(bitdat2)             

# Check for any null values:
bitdat2[is.na(bitdat2)]
# There are no null values in the data set.

# Convert the data into a time series.
# Create a new data frame and assign time series value,
# and specify the 'ts' function.
bitdat2_ts <- ts(bitdat2$Close,
              start = c(2015, 1),
              end = c(2023, 11),
              # Monthly frequency without missing values in data.
              frequency = 12)

head(bitdat2_ts)

# Plot the time series.
plot(bitdat2_ts)

# View the data by creating a smaller sample of the visualisation.
plot(window(bitdat2_ts, 2022, 2023))

# Group the data by month.

# Specify the boxplot function and specific operands. 
boxplot(bitdat2_ts~cycle(bitdat2_ts),
      ylab = "Close Value USD", 
      xlab = "Month",
      main = "Bitcoin Value in USD by Month")


# Extract and plot the main components to decompose the time series.
bitdat_components <- decompose(bitdat2_ts)

# What is the object?
class(bitdat_components)


# Determine the structure.
str(bitdat_components)


# Visualise the decomposed time series.
plot(bitdat_components) 


# Look at a single year (seasonal pattern) of the data.
plot(window(bitdat_components$seasonal,
            c(2016, 1), c(2016, 12)))


# Plot the trend component.
plot(window(bitdat_components$trend,
            c(2015, 1), c(2023, 10)))

# Test stationarity with augmented ADF test.
adf.test(bitdat2_ts)
# A p-value of 0.4 suggests that the data is not stationary (because is it is
# >0.05).

# Testing Autocorrelation:
# Review random time series variables.
bitdat_components$random

# 2014 and 2015 contains some null values.
# Plot values removing NA values while doing so.
autoplot(acf(na.remove(bitdat_components$random), plot=FALSE)) + 
  # Add a title.
  labs(title="Randomness value") + 
  # Set the theme.
  theme_classic() 

# Plot random variables to check the distribution.
hist(bitdat_components$random)

# Fit the model to our time series. 
arima_bitdat_ts <- auto.arima(bitdat2_ts)

# Make a forecast for the next three months.
forecast_bitdat_ts <- forecast(arima_bitdat_ts, 3)

# Plot the forecast on a graph.
autoplot(forecast_bitdat_ts) + theme_classic()

# Print the values in the data frame.
forecast_bitdat_ts

# Extend the prediction, set data source, time span, and assign a new object.
forecast2_bitdat_ts <- forecast(arima_bitdat_ts, 24)

# Plot the output and set the theme. 
autoplot(forecast2_bitdat_ts) + theme_classic()

# Print the values in the data frame.
forecast2_bitdat_ts  

# Testing for Accuracy:
# (Training data) Create a new time series object
# and assign the values and parameters.
# Note: 2022, 10 is 12 months before the end of the series.
bitdat_train_ts = window(bitdat2_ts,
                       start = c(2014, 9),
                       end = c(2022, 10),
                       frequency = 12)

# (Test data) Create a new time series object
# and assign the values and parameters.
bitdat_test_ts = window(bitdat2_ts,
                      start = c(2022, 11),
                      end = c(2023, 11),
                      frequency = 12)

# View new data frames.
head(bitdat_train_ts)
head(bitdat_test_ts)

# Fit the training model to the training data:
# Create a new object and
# specify the forecast function and pass the ARIMA model.
forecast_bitdat_train_ts <- forecast(auto.arima(bitdat_train_ts), 12)

# Plot the values and forecast and add a theme:
autoplot(forecast_bitdat_train_ts, main="Bitcoin Value (USD) and Two Year Prediction"
         , xlab="Time", ylab="Value") +
  theme_classic()

# The shaded blue section is the fitted values forecast for the 12 months we removed 
# from the time series.  If our model is accurate, then the test set should 
# correspond to this blue section.

# Add the autolayer(), specify the data set, and series name.
autoplot(forecast_bitdat_train_ts) +
  autolayer(bitdat_train_ts, series='Train') +
  autolayer(bitdat_test_ts, series='Test') +
  theme_classic()

# Check the accuracy of the prediction.
accuracy(forecast_bitdat_train_ts, bitdat_test_ts)

# Let’s focus on our MAPE value for the test set, which sits at 2.0186. This means 
# that the average difference between the predicted value and the actual value is 2%. 
# According to the rule of thumb, a MAPE value of less than 10% is considered excellent. 
# Therefore, we have a strong MAPE value which suggests that the model is relatively 
# accurate.

autoplot(forecast2_bitdat_ts) +
  autolayer(bitdat2_ts, series='2014 to 2023') +
  autolayer(forecast2_bitdat_ts, series='Predicted Next 2 Years')
  theme_classic()