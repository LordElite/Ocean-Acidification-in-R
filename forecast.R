rm(list = ls())

#library initialization
library(ggplot2)
library(tidyverse)
library(lubridate)
library(forecast)
library(tidyr)
library(dplyr)
library(neuralnet)
library(scico)
library(viridis)


#ph dataset
ocean <- data.frame(read.csv('directory/seawater-ph.csv')) # oceanic pH database 
ph_ocean <- data.frame(Date= as.Date(ocean$Day),
                       pH = ocean$Monthly.measurement.of.ocean.pH.levels) # oceanic ph monthly

#ph time series
ts_data_ph <- ts(ph_ocean$pH, start = 1, frequency = 12)

#vector of random combinations of ARIMA inputs
orders <- matrix(c(2,1,0, 1,1,0, 1,2,1,3,2,1,2,3,3,3,1,2), ncol = 3, byrow = TRUE)
BIC <- c()
AIC <- c()
SE <- c()
p <- c()
d <- c()
q <- c()
#codeblock to calculate different ARIMA models
for (i in c(1:6)) {
  model_ph <- Arima(ts_data_ph, orders[i,],include.drift = TRUE) #possible models
  p <- append(p, orders[i,1])
  d <- append(d, orders[i,2])
  q <- append(q, orders[i,3])
  BIC <- append(BIC, BIC(model_ph))
  AIC <- append(AIC, AIC(model_ph))
  SE <- append(SE, sqrt(model_ph$sigma2))
}

#gathering of models
ARIMA_models <- data.frame(p = p, d = d, q = q, BIC = BIC, AIC = AIC, SE = SE)
#optimal ARIMA model
op_model_ph <- auto.arima(ts_data_ph)
op_ph_BIC <- BIC(op_model_ph)
op_ph_AIC <- AIC(op_model_ph)
op_ph_SE <- sqrt(op_model_ph$sigma2)

op_predict_ph <- forecast(op_model_ph)

ph_forecast_plot <- autoplot(op_predict_ph)
ph_forecast_plot

ggsave(plot = ph_forecast_plot, 
       "fig25.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")


predictions_ph  <- data.frame(Date = seq(from = ph_ocean$Date[length(ph_ocean$Date)]+31,
                                         by = "month",
                                         length.out = length(op_predict_ph$mean)), 
                              pH = as.numeric(op_predict_ph$mean)) 

total_data_ph <- rbind( ph_ocean,predictions_ph) 
# plotting predicted results
time_line_ph_forecast_plot <- ggplot() + 
  geom_line(data = total_data_ph, 
            aes(x = Date, y = pH, group = 1, color = "Predicted pH data"), size = 1.05) + 
  geom_line(data = ph_ocean, 
            aes(x = Date, y = pH, color = "Original pH Data"), size = 1.05) + 
  labs(x = "Date (Monthly)", y = "Acidity (pH)", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  theme_minimal()

time_line_ph_forecast_plot
# Save the plot as a JPG file with 300 DPI
ggsave(plot = time_line_ph_forecast_plot, 
       "fig26.jpg", dpi = 300, width = 8, height = 6, units = "in")

#applying prediction on a section of the data
ph_section <- head(ph_ocean,180)
section_ts_ph <- ts(ph_section$pH, start = 1, frequency = 12)
adjust_ph_Section <- auto.arima(section_ts_ph)

section_prediction_ph <- forecast(adjust_ph_Section)

arima_section_ph_plot <- autoplot(section_prediction_ph)

arima_section_ph_plot
# Save the plot as a JPG file with 300 DPI
ggsave(plot = arima_section_ph_plot, 
       "fig27.jpg", dpi = 300, width = 8, height = 6, units = "in")

#generating a dataframe with the new data sectioned
predictions_ph_sec  <- data.frame(Date = seq(from = ph_ocean$Date[181],
                                             to = ph_ocean$Date[199] ,
                                             by = "month"),
                              pH = as.numeric(section_prediction_ph$mean)) 
#percentage error
percent_error_ph <- c()
for (j in c(1:length(predictions_ph_sec$pH))) {
  percent_error_ph <- append(percent_error_ph, 
                             100*(abs(ph_ocean$pH[180+j]-predictions_ph_sec$pH[j])/abs(ph_ocean$pH[180+j]) ))
}
total_data_ph_sec <- data.frame( Date = predictions_ph_sec$Date,
                                 Actual_value = ph_ocean$pH[181:204],
                                 Forecasted_value = predictions_ph_sec$pH,
                                 Percentage_error = percent_error_ph)


section_ph_forecast_plot <- ggplot() + 
  geom_line(data = total_data_ph_sec, 
            aes(x = Date, y = Actual_value, group = 1, color = "Original pH data"), size = 1.05) + 
  geom_line(data = total_data_ph_sec, 
            aes(x = Date, y = Forecasted_value, color = "Predicted pH Data"), 
            size = 1.05) + 
  labs(x = "Date (Monthly)", y = "Acidity (pH)", color = "Legend") +
  scale_color_viridis_d(option = "C") +  # Use viridis palette for colorblind-friendly colors
  theme_minimal()

section_ph_forecast_plot 

ggsave(plot = section_ph_forecast_plot , 
       "fig28.jpg", dpi = 300, width = 8, height = 6, units = "in")


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

op_predict_co2 <- forecast(op_model_co2)
co2_forecast_plot <- autoplot(op_predict_co2)

co2_forecast_plot

ggsave(plot = co2_forecast_plot, 
       "fig29.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")

predictions_co2  <- data.frame(Date = seq(from = sb_co2$Date[length(sb_co2$Date)]+31,
                                         by = "month",
                                         length.out = length(op_predict_co2$mean)), 
                              CO2 = as.numeric(op_predict_co2$mean)) 
#partly data frame to zoom in on the prediction
total_data_co2_part <- rbind( sb_co2[500:length(sb_co2$CO2),],predictions_co2) 

total_co2_part_forecast_plot <- ggplot() + 
  geom_line(data = total_data_co2_part, 
            aes(x = Date, y = CO2, group = 1, color = "Predicted CO2 Data"), size = 1.05) + 
  geom_line(data = sb_co2[500:length(sb_co2$CO2),], 
            aes(x = Date, y = CO2, color = "Original CO2 data"), size = 1.05) + 
  labs(x = "Date (Monthly)", y = "CO2 emissions (ppm)", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  theme_minimal()

total_co2_part_forecast_plot

ggsave(plot = total_co2_part_forecast_plot, 
       "fig30.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")

#applying prediction on a section of the data
co2_section <- head(sb_co2,240)
section_ts_co2 <- ts(co2_section$CO2, start = 1, frequency = 12)
adjust_co2_Section <- auto.arima(section_ts_co2)

section_prediction_co2 <- forecast(adjust_co2_Section)
autoplot(section_prediction_co2)

#generating a dataframe with the new data sectioned
predictions_co2_sec  <- data.frame(Date = seq(from = sb_co2$Date[241],
                                             to = sb_co2$Date[264] ,
                                             by = "month"),
                                   CO2 = as.numeric(section_prediction_co2$mean)) 
#percentage error
percent_error_co2 <- c()
for (K in c(1:length(predictions_co2_sec$CO2))) {
  percent_error_co2 <- append(percent_error_co2, 
                             100*(abs(sb_co2$CO2[240+K]-predictions_co2_sec$CO2[K])/abs(sb_co2$CO2[240+K]) ))
}
total_data_co2_sec <- data.frame( Date = predictions_co2_sec$Date,
                                 Actual_value = sb_co2$CO2[241:264],
                                 Forecasted_value = predictions_co2_sec$CO2,
                                 Percentage_error = percent_error_co2)


co2_forecast_section_plot <- ggplot() + 
  geom_line(data = total_data_co2_sec, 
            aes(x = Date, y = Actual_value, group = 1, color = "Original pH data"), size = 1.05) + 
  geom_line(data = total_data_co2_sec, 
            aes(x = Date, y = Forecasted_value, color = "Predicted CO2 Data"), size = 1.05) + 
  labs(x = "Date (Monthly)", y = "CO2 emissions (ppm)", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  theme_minimal()

co2_forecast_section_plot

ggsave(plot = co2_forecast_section_plot, 
       "fig31.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")
