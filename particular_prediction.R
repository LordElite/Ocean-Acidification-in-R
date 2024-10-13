rm(list = ls())

#library initialization
library(ggplot2)
library(tidyverse)
library(lubridate)
library(forecast)
library(tidyr)
library(dplyr)
library(neuralnet)



#ph dataset
ocean <- data.frame(read.csv('directory/seawater-ph.csv')) # oceanic pH database 
ph_ocean <- data.frame(Date= as.Date(ocean$Day),
                       pH = ocean$Monthly.measurement.of.ocean.pH.levels) # oceanic ph monthly

#ph time series
ts_data_ph <- ts(ph_ocean$pH, start = 1, frequency = 12)


#optimal ARIMA model
op_model_ph <- auto.arima(ts_data_ph)
#codeblock to calculate the amount of months to achieve ph = 7
p  <- 1
ph_arima <- 9
while (ph_arima > 7) {
  op_predict_ph <- forecast(op_model_ph, h = p)
  ph_arima <- op_predict_ph[["lower"]][p]
  p  <- p + 1
}

#calculating the date when ph = 7
date_ARIMA<- seq(from = ph_ocean$Date[length(ph_ocean$Date)],
                 by = "month",
                 length.out = p)
date_ARIMA[length(date_ARIMA)]


#linear model to achieve pH = 7
ph_ocean$index <- c(1:length(ph_ocean$Date))
model_ph <- lm( pH ~ index, data = ph_ocean)

coefficients_ph <- coef(model_ph)

# Print the equation
cat("Equation of the line: y =", coefficients_ph[1], "+", 
    coefficients_ph[2], "x\n")

# Create the equation as a string
equation_ph <- paste("pH = ", round(coefficients_ph[1], 10)," ", 
                     round(coefficients_ph[2], 10), " Month", sep = "")

month_linear <- (7 - as.numeric(coefficients_ph[1]))/ as.numeric(coefficients_ph[2])

#calculating the date when ph = 7
date_linear<- seq(from = ph_ocean$Date[length(ph_ocean$Date)],
                 by = "month",
                 length.out = as.integer(month_linear) - length(ph_ocean$Date))
date_linear[length(date_linear)]


#estimation of CO2
# CO2 dataset
co2 <-  data.frame(read.csv('directory/co2_mm_mlo.csv'))

db_co2 <-subset(co2, (year > 1958 | (year == 1958 & month >= 3)) &  #generating a date column
                  (year < 2024 | (year == 2024 & month <= 7)))

db_co2$date <-as.Date(paste(db_co2$year, db_co2$month, "01", sep = "-")) # CO2 column union

sb_co2 <- data.frame(Date = db_co2$date, CO2 = db_co2$monthly_average)


#ph time series
ts_data_co2 <- ts(sb_co2$CO2, start = 1, frequency = 12)

#optimal ARIMA model
op_model_co2 <- auto.arima(ts_data_co2)
co2_time_vector_ARIMA <- seq(from = sb_co2$Date[length(sb_co2$Date)], 
                       to = date_ARIMA[length(date_ARIMA)], 
                       by = "month")

op_predict_co2 <- forecast(op_model_co2, h = length(co2_time_vector_ARIMA))
long_term_co2_ARIMA <- op_predict_co2$mean[length(op_predict_co2$mean)]
autoplot(op_predict_co2)

