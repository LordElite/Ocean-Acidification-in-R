rm(list = ls())

#library initialization
library(ggplot2)
library(scico)
library(viridis)


#ph dataset
ocean <- data.frame(read.csv('directory/seawater-ph.csv')) # oceanic pH database 
ph_ocean <- data.frame(Date = as.Date(ocean$Day),
                       pH = ocean$Monthly.measurement.of.ocean.pH.levels) # oceanic ph monthly
ph_ocean$Year <-as.numeric(format(ph_ocean$Date, '%Y'))  #extracting Year from Date column
ph_ocean$Month <-as.numeric(format(ph_ocean$Date, '%m')) #extracting Month from Date column


# CO2 dataset
co2 <-  data.frame(read.csv('directory/co2_mm_mlo.csv'))

db_co2 <-subset(co2, (year > 1958 | (year == 1958 & month >= 3)) &  #generating a date column
                  (year < 2024 | (year == 2024 & month <= 7)))

db_co2$date <-as.Date(paste(db_co2$year, db_co2$month, "01", sep = "-")) # CO2 column union

sb_co2 <- data.frame(Date = db_co2$date, CO2 = db_co2$monthly_average)

# plotting the original ph data
ggplot() + 
  geom_line(data = ph_ocean, 
            aes(x=Date, y=pH, color = 'line curve', group=1), size = 0.8) + 
  geom_point(data = ph_ocean, 
             aes(x=Date, y=pH, colour = 'data points', group=1), size = 0.9)  +
  labs(x = "Date (Monthly)", y = "acidity ( pH )", color = "Legend") +
  scale_color_viridis_d(option = "D")  +
  theme_minimal()

ggsave("fig5.jpg", dpi = 300, width = 8, height = 6, units = "in")# oceanic Ph monthly plotted



#plotting the original CO2 data
ggplot() + 
  geom_line(data = sb_co2, 
            aes(x=Date, y=CO2, color = 'line curve', group=1), size = 1.05) + 
  labs(x = "Date (Monthly)", y = "CO2 (ppm)", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  theme_minimal()

ggsave("fig6.jpg", dpi = 300, width = 8, height = 6, units = "in")# CO2 monthly plotted
# co2 emissions monthly plotted
