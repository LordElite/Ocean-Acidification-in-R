rm(list = ls())

library(ggplot2) #for plot detailed datasets
library(gridExtra) #for adding subplots
library(dplyr) #for special operators
library(zoo)  # For rolling functions
library(forecast)
library(lubridate)
library(scico)
library(viridis)



#ph dataset
ocean <- data.frame(read.csv('directory/seawater-ph.csv')) # oceanic pH database 
ph_ocean <- data.frame(Date = as.Date(ocean$Day),
                       pH = ocean$Monthly.measurement.of.ocean.pH.levels) # oceanic ph monthly
ph_ocean$Year <-as.numeric(format(ph_ocean$Date, '%Y'))  #extracting Year from Date column
ph_ocean$Month <-as.numeric(format(ph_ocean$Date, '%m')) #extracting Month from Date column
# Calculate rolling variance with a window of 12 months (adjust as needed)
ph_ocean$rolling_variance <- rollapply(ph_ocean$pH, width = 12, FUN = var, fill = NA, align = 'right')

# Calculate rolling standard deviation with the same window
ph_ocean$rolling_sd <- rollapply(ph_ocean$pH, width = 12, FUN = sd, fill = NA, align = 'right')

# Calculate rolling mean with the same window
ph_ocean$rolling_mean <- rollapply(ph_ocean$pH, width = 12, FUN = mean, fill = NA, align = 'right')

mean_ph <- ph_ocean %>%
  group_by(Year) %>%
  summarize(avg_pH = mean(pH, na.rm = TRUE))

mean_ph$Date <- as.Date(paste( mean_ph$Year, "12", "31", sep = "-"))

# Plot rolling statistics
ph_sd <- ggplot(ph_ocean, aes(x = Date)) +
  geom_line(aes(y = rolling_sd), size = 1) +
  labs(x = "Date (Monthly)", y = "pH Rolling Standard Deviation [pH]") +
  ggtitle("(a)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) +
  theme_minimal() 

ph_rv  <- ggplot(ph_ocean, aes(x = Date)) +
  geom_line(aes(y = rolling_variance), size = 1) +
  labs(x = "Date (Monthly)", y = "pH Rolling Variance") +
  theme_minimal() +
  ggtitle( "(b)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) 

ph_rm <- ggplot(ph_ocean, aes(x = Date)) +
  geom_line(aes(y = rolling_mean), size = 1) +
  labs(x = "Date (Monthly)", y = "pH Rolling Mean [pH]") +
  theme_minimal() +
  ggtitle("(c)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) 


# plotting
ph_sm <- ggplot() + 
  geom_line(data = mean_ph, 
            aes(x = Date, y = avg_pH), color = "black", size = 1) + 
  labs(x = "Date (Monthly)", y = "pH yearly aritmetic mean [pH]") +
  theme_minimal() +
  ggtitle("(d)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) 

# Arrange the plots in a grid
total_disp_ph_plot <- grid.arrange(ph_sd, ph_rv, ph_rm, ph_sm, nrow = 2, ncol = 2)

ggsave(plot = total_disp_ph_plot, 
       "fig14.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")

#dispersion analysis from 2007 to 2010
ph_subset_1 <-subset(ph_ocean, (Year > 2007 | (Year >= 2007 & Month >= 1)) &
                       (Year < 2009 | (Year == 2009 & Month <= 12)))


ph_subset_1_sd <- ggplot() + 
  geom_line(data = ph_subset_1, 
            aes(x = Date, y = rolling_sd), color = "black", size = 1) + 
  labs(x = "Date (Monthly)", y = "pH rolling standard deviation") +
  theme_minimal() +
  ggtitle("(a)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) 

ph_subset_1_rm <- ggplot() + 
  geom_line(data = ph_subset_1, 
            aes(x = Date, y = rolling_mean), color = "black", size = 1) + 
  labs(x = "Date (Monthly)", y = "pH rolling mean") +
  theme_minimal() +
  ggtitle("(b)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) 

#dispersion analysis from 2020 to 2022
ph_subset_2 <-subset(ph_ocean, (Year > 2020 | (Year >= 2020 & Month >= 1)) &
                       (Year < 2022 | (Year == 2022 & Month <= 12)))

ph_subset_2_sd <- ggplot() + 
  geom_line(data = ph_subset_2, 
            aes(x = Date, y = rolling_sd), color = "black", size = 1) + 
  labs(x = "Date (Monthly)", y = "pH rolling standard deviation") +
  theme_minimal() +
  ggtitle("(c)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) 

ph_subset_2_rm <- ggplot() + 
  geom_line(data = ph_subset_2, 
            aes(x = Date, y = rolling_mean), color = "black", size = 1) + 
  labs(x = "Date (Monthly)", y = "pH rolling mean") +
  theme_minimal() +
  ggtitle("(d)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) 

# Arrange the plots in a grid
ph_disp_subset <- grid.arrange(ph_subset_1_sd, 
                               ph_subset_2_sd , 
                               ph_subset_1_rm , 
                               ph_subset_2_rm , 
                               nrow = 2, ncol = 2)

ggsave(plot = ph_disp_subset, 
       "fig18.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")
#CO2 dispersion analysis

# CO2 dataset
co2 <-  data.frame(read.csv('directory/co2_mm_mlo.csv'))
db_co2 <-subset(co2, (year > 1958 | (year >= 1958 & month >= 3)) &
                    (year < 2024 | (year == 2024 & month <= 7)))

db_co2$date <-as.Date(paste(db_co2$year, db_co2$month, "01", sep = "-")) # CO2 column union

sb_co2 <- data.frame(Date = db_co2$date, CO2 = db_co2$monthly_average, year = db_co2$year, month = db_co2$month)

# Calculate rolling variance with a window of 12 months (adjust as needed)
sb_co2$rolling_variance <- rollapply(sb_co2$CO2, width = 12, FUN = var, fill = NA, align = 'right')

# Calculate rolling standard deviation with the same window
sb_co2$rolling_sd <- rollapply(sb_co2$CO2, width = 12, FUN = sd, fill = NA, align = 'right')

# Calculate rolling mean with the same window
sb_co2$rolling_mean <- rollapply(sb_co2$CO2, width = 12, FUN = mean, fill = NA, align = 'right')

mean_co2 <- sb_co2 %>%
  group_by(year) %>%
  summarize(avg_co2 = mean(CO2, na.rm = TRUE))

mean_co2$Date <- as.Date(paste( mean_co2$year, "12", "31", sep = "-"))

# Plot rolling statistics
co2_sd <- ggplot(sb_co2, aes(x = Date)) +
  geom_line(aes(y = rolling_sd), size = 1) +
  labs(x = "Date (Monthly)", y = "CO2 Rolling Standard Deviation") +
  theme_minimal() +
  ggtitle("(a)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) 


co2_rv  <- ggplot(sb_co2, aes(x = Date)) +
  geom_line(aes(y = rolling_variance), size = 1) +
  labs(x = "Date (Monthly)", y = "CO2 Rolling Variance") +
  theme_minimal() +
  ggtitle("(b)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) 


co2_rm <- ggplot(sb_co2, aes(x = Date)) +
  geom_line(aes(y = rolling_mean), size = 1) +
  labs(x = "Date (Monthly)", y = "CO2 Rolling Mean") +
  theme_minimal() +
  ggtitle("(c)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) 



# plotting
co2_sm <- ggplot() + 
  geom_line(data = mean_co2, 
            aes(x = Date, y = avg_co2), color = "black", size = 1) + 
  labs(x = "Date (Monthly)", y = "CO2 yearly aritmetic mean") +
  theme_minimal() +
  ggtitle("(d)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) 


# Arrange the plots in a grid
total_disp_co2 <- grid.arrange(co2_sd, co2_rv, co2_rm, co2_sm, nrow = 2, ncol = 2)

ggsave(plot = total_disp_co2, 
       "fig19.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")
#dispersion analysis from 2007 to 2010
co2_subset_1 <-subset(sb_co2, (year > 2007 | (year >= 2007 & month >= 1)) &
                       (year < 2009 | (year == 2009 & month <= 12)))

co2_subset_1_sd <- ggplot() + 
  geom_line(data = co2_subset_1, 
            aes(x = Date, y = rolling_sd), color = "black", size = 1) + 
  labs(x = "Date (Monthly)", y = "CO2 rolling standard deviation") +
  theme_minimal() +
  ggtitle("(a)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) 

co2_subset_1_rm <- ggplot() + 
  geom_line(data = co2_subset_1, 
            aes(x = Date, y = rolling_mean), color = "black", size = 1) + 
  labs(x = "Date (Monthly)", y = "CO2 rolling mean") +
  theme_minimal() +
  ggtitle("(b)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) 


#dispersion analysis from 2020 to 2022
co2_subset_2 <-subset(sb_co2, (year > 2020 | (year >= 2020 & month >= 1)) &
                       (year < 2022 | (year == 2022 & month <= 12)))

co2_subset_2_sd <- ggplot() + 
  geom_line(data = co2_subset_2, 
            aes(x = Date, y = rolling_sd), color = "black", size = 1) + 
  labs(x = "Date (Monthly)", y = "CO2 rolling standard deviation") +
  theme_minimal() +
  ggtitle("(c)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) 

co2_subset_2_rm <- ggplot() + 
  geom_line(data = co2_subset_2, 
            aes(x = Date, y = rolling_mean), color = "black", size = 1) + 
  labs(x = "Date (Monthly)", y = "CO2 rolling mean") +
  theme_minimal() +
  ggtitle("(d)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10)) 


# Arrange the plots in a grid
total_disp_co2_subset_1 <- grid.arrange(co2_subset_1_sd, 
                                        co2_subset_2_sd, 
                                        co2_subset_1_rm, 
                                        co2_subset_2_rm, nrow = 2, ncol = 2)

ggsave(plot = total_disp_co2_subset_1, 
       "fig21.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")
#pH mean test
am <- rep(mean(ph_ocean$pH), length(ph_ocean$pH))
mean_dataframe <- data.frame(Mean = am, Date = ph_ocean$Date)
# plotting the original data and the mean line
ph_mean_plot <- ggplot() + 
  geom_line(data = ph_ocean, 
            aes(x = Date, y = pH, group = 1, color = "Original pH Data"), size = 1) +
  geom_line(data = mean_dataframe, 
            aes(x = Date, y = Mean, group = 1, color = "Mean"), size = 1) +
  labs(x = "Date (Monthly)", y = "acidity ( pH )", color = "Legend") +# oceanic Ph monthly plotted
  scale_color_viridis_d(option = "D")  +
    theme_minimal()

ph_mean_plot
ggsave(plot = ph_mean_plot, 
       "fig15.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")
# Convert the 'pH' column to a time series object with monthly frequency
# Assuming data is monthly, with a frequency of 12
ocean_ph_ts <- ts(ph_ocean$pH, 
            start = 1, 
            frequency = 12)


# ACF plot using forecast package
autocorr_ph_plot <- ggAcf(ocean_ph_ts)

autocorr_ph_plot

ggsave(plot = autocorr_ph_plot, 
       "fig16.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")
#decomposition of pH series
decomp_ph <- decompose(ocean_ph_ts)

# Save the decomposition plot as a JPG file with 300 DPI
jpeg("fig17.jpg", width = 8, height = 6, units = "in", res = 300)

# Plot the decomposition
plot(decomp_ph)

# Close the graphics device
dev.off()


#pH mean test
co2_am <- rep(mean(sb_co2$CO2), length(sb_co2$CO2))
co2_dataframe <- data.frame(Mean = co2_am, Date = sb_co2$Date)
# plotting the original data and the mean line
co2_mean_plot <- ggplot() + 
  geom_line(data = sb_co2, 
            aes(x = Date, y = CO2, group = 1, color = "Original CO2 Data"), size = 1) +
  geom_line(data = co2_dataframe, 
            aes(x = Date, y = Mean, group = 1, color = "Mean"), size = 1) +
  labs(x = "Date (Monthly)", y = "CO2 Emissions ( ppm )", color = "Legend") +# oceanic Ph monthly plotted
  scale_color_viridis_d(option = "D")  +
  theme_minimal()

co2_mean_plot

ggsave(plot = co2_mean_plot, 
       "fig20.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")

# Convert the 'pH' column to a time series object with monthly frequency
# Assuming data is monthly, with a frequency of 12
co2_ts <- ts(sb_co2$CO2, 
                  start = c(as.numeric(ph_ocean$Year[1]), 
                            as.numeric(ph_ocean$Month[1])), 
                  frequency = 12)


# ACF plot using forecast package
autocorr_co2_plot <- ggAcf(co2_ts)
autocorr_co2_plot

ggsave(plot = autocorr_co2_plot, 
       "fig22.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")
#decomposition of CO2 series
decomp_co2 <- decompose(co2_ts)

# Save the decomposition plot as a JPG file with 300 DPI
jpeg("fig23.jpg", width = 8, height = 6, units = "in", res = 300)

# Plot the decomposition
plot(decomp_co2)

# Close the graphics device
dev.off()


