rm(list = ls())

#library initialization
library(ggplot2)
library(tidyverse)
library(lubridate)
library(forecast)
library(tidyr)
library(dplyr)
library(neuralnet)
library(robustbase)
library(MASS)
library(L1pack)



#ph dataset
ocean <- data.frame(read.csv('Directory/seawater-ph.csv')) # oceanic pH database 
ph_ocean <- data.frame(Date= as.Date(ocean$Day),
                       pH = ocean$Monthly.measurement.of.ocean.pH.levels) # oceanic ph monthly

#ph time series
ts_data_ph <- ts(ph_ocean$pH, start = 1, frequency = 12)


#optimal ARIMA model
op_model_ph <- auto.arima(ts_data_ph)
#codeblock to calculate the amount of months to achieve ph = 7
p  <- 5100
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

#prediction for 2100
# Define the start and end dates
start_date_ph <- ph_ocean$Date[1]
end_date_ph <- ymd("2100-01-01")

# Calculate the difference in months
months_difference_ph <- interval(start_date_ph, end_date_ph) %/% months(1)

# Display the result
pH_2100 <- coefficients_ph[1]+coefficients_ph[2]*months_difference_ph


#through ARIMA model
op_predict_ph_2100 <- forecast(op_model_ph, h = months_difference_ph)
ph_arima_2100 <- op_predict_ph_2100 [["lower"]][months_difference_ph]

#estimation of CO2
# CO2 dataset
co2 <-  data.frame(read.csv('Directory/co2_mm_mlo.csv'))

db_co2 <-subset(co2, (year > 1958 | (year == 1958 & month >= 3)) &  #generating a date column
                  (year < 2024 | (year == 2024 & month <= 7)))
db_co2$Date <-as.Date(paste(db_co2$year, db_co2$month, "01", sep = "-")) # CO2 column union
sb_co2 <- data.frame(Date = db_co2$date, CO2 = db_co2$monthly_average)
sb_co2$index <- c(1:length(sb_co2$Date))

#1958-2024 model
model_co2 <- lm( CO2 ~ index, data = sb_co2)

coefficients_co2 <- coef(model_co2)

# Print the equation
cat("Equation of the line: y =", coefficients_co2[1], "+", 
    coefficients_co2[2], "x\n")

# Create the equation as a string
equation_co2 <- paste("CO2 = ", round(coefficients_co2[1], 10),"+ ", 
                        round(coefficients_co2[2], 10), " Month", sep = "")

# Define the start and end dates
start_date_co2 <- sb_co2$Date[1]
end_date_co2 <- ymd("2100-01-01")

# Calculate the difference in months
months_difference_co2 <- interval(start_date_co2, end_date_co2) %/% months(1)

# Display the result
co2_2100 <- coefficients_co2[1]+coefficients_co2[2]*months_difference_co2

#2498 forecasting
# Define the start and end dates
start_date_co2_2498 <- sb_co2$Date[1]
end_date_co2_2498 <- ymd("2498-02-01")

# Calculate the difference in months
months_difference_co2_2498 <- interval(start_date_co2_2498, end_date_co2_2498) %/% months(1)

# Display the result
co2__2498 <- coefficients_co2[1]+coefficients_co2[2]*months_difference_co2_2498




#ph time series
ts_data_co2 <- ts(sb_co2$CO2, start = 1, frequency = 12)

#optimal ARIMA model
op_model_co2 <- auto.arima(ts_data_co2)
co2_time_vector_ARIMA <- seq(from = sb_co2$Date[length(sb_co2$Date)], 
                       to = as.Date("2449-06-01"), 
                       by = "month")

op_predict_co2 <- forecast(op_model_co2, h = length(co2_time_vector_ARIMA))
long_term_co2_ARIMA <- op_predict_co2$mean[length(op_predict_co2$mean)]


#2100 prediction

op_model_co2_2100 <- auto.arima(ts_data_co2)
co2_time_vector_ARIMA_2100  <- seq(from = sb_co2$Date[length(sb_co2$Date)], 
                             to = as.Date("2100-01-01"), 
                             by = "month")

op_predict_co2_2100 <- forecast(op_model_co2_2100, h = length(co2_time_vector_ARIMA_2100))
long_term_co2_ARIMA_2100 <- op_predict_co2_2100$mean[length(op_predict_co2_2100$mean)]

long_term_co2_ARIMA_2100



#scatter prediction
ph_ocean$Year <-as.numeric(format(ph_ocean$Date, '%Y'))  #extracting Year from Date column
ph_ocean$Month <-as.numeric(format(ph_ocean$Date, '%m')) #extracting Month from Date column

sb_co2$Year <-as.numeric(format(sb_co2$Date, '%Y'))  #extracting Year from Date column
sb_co2$Month <-as.numeric(format(sb_co2$Date, '%m')) #extracting Month from Date column
# Merge the datasets based on the Date column
merged_data <- merge(ph_ocean, sb_co2, by=c("Year","Month"), all=FALSE)

# Remove rows with any missing values
clean_data <- na.omit(merged_data)
corr_data <- data.frame(pH = clean_data$pH, CO2 = clean_data$CO2)

lad_model <- lad(pH ~ CO2, data =  corr_data)       # LAD model
# Extract the intercept and slope
intercept_lad <- lad_model$coefficients[1]
slope_lad <- lad_model$coefficients[2]

# Create a new data frame for the fitted line
fitted_line_lad <- data.frame(
  x = corr_data$CO2,
  y = intercept_lad + slope_lad * corr_data$CO2
)
# Extract the coefficients   #LAD
coefficients_lad<- coef(lad_model)
# Print the equation
cat("Equation of the line: pH =", coefficients_lad[1], "+", 
    coefficients_lad[2], "CO2\n")
# Create the equation as a string
equation_lad <- paste("pH = ", round(coefficients_lad[1], 3), " + ", 
                      round(coefficients_lad[2], 3), "CO2", sep = "")

#calculating co2 when ph = 7
scatter_co2 <- (7-coefficients_lad[1])/coefficients_lad[2]

