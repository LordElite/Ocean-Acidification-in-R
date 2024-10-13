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
ph_ocean$index <- c(1:length(ph_ocean$Date))



model_ph <- lm( pH ~ index, data = ph_ocean)

coefficients_ph <- coef(model_ph)

# Print the equation
cat("Equation of the line: y =", coefficients_ph[1], "+", 
    coefficients_ph[2], "x\n")

# Create the equation as a string
equation_ph <- paste("pH = ", round(coefficients_ph[1], 10)," ", 
                  round(coefficients_ph[2], 10), " Month", sep = "")


# Plot the data with the regression line and the equation
ph_linear_plot <- ggplot() + 
  geom_point(data = ph_ocean, aes(x = Date, y = pH, color = 'data points', group = 1), 
             size = 1) +
  geom_smooth(data = ph_ocean, 
              aes(x = Date, y = pH, color = 'linear regression', group = 1), 
              method = 'lm', se = FALSE) +
  labs(x = "Date(monthly)", y = "Ocean Acidity (pH)", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  # Annotate to show the equation on the plot
  annotate("text", x = max(ph_ocean$Date[length(ph_ocean$Date)/3]), 
           y = max(ph_ocean$pH), 
           label = equation_ph, 
           color = "blue", 
           hjust = 0) +
  theme_minimal()

ph_linear_plot

ggsave(plot = ph_linear_plot, 
       "fig7.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")

# CO2 dataset
co2 <-  data.frame(read.csv('directory/co2_mm_mlo.csv'))

db_co2_1 <-subset(co2, (year > 1958 | (year >= 1958 & month >= 3)) &
                    (year < 2024 | (year == 2024 & month <= 7)))

db_co2_1$date <-as.Date(paste(db_co2_1$year, db_co2_1$month, "01", sep = "-")) # CO2 column union
sb_co2_1 <- data.frame(Date = db_co2_1$date, CO2 = db_co2_1$monthly_average)
sb_co2_1$index <- c(1:length(sb_co2_1$Date))


#1958-2024 model
model_co2_1 <- lm( CO2 ~ index, data = sb_co2_1)

coefficients_co2_1 <- coef(model_co2_1)

# Print the equation
cat("Equation of the line: y =", coefficients_co2_1[1], "+", 
    coefficients_co2_1[2], "x\n")

# Create the equation as a string
equation_co2_1 <- paste("CO2 = ", round(coefficients_co2_1[1], 10),"+ ", 
                  round(coefficients_co2_1[2], 10), " Month", sep = "")


# Plot the data with the regression line and the equation
co2_linear_plot_1 <- ggplot() + 
  geom_point(data = sb_co2_1, aes(x = Date, y = CO2,  color = 'data points', group = 1), size = 1) +
  geom_smooth(data = sb_co2_1, 
              aes(x = Date, y = CO2,  color = 'linear regression', group = 1),
              method = 'lm', se = FALSE) +
  labs(x = "Date(monthly)", y = "CO2 (ppm)") +
  scale_color_viridis_d(option = "C")  +
  # Annotate to show the equation on the plot
  annotate("text", x = max(sb_co2_1$Date[length(sb_co2_1$Date)/3]), 
           y = max(sb_co2_1$CO2), 
           label = equation_co2_1, 
           color = "blue", 
           hjust = 0) +
  theme_minimal()

co2_linear_plot_1

ggsave(plot = co2_linear_plot_1, 
       "fig8.jpg", 
       dpi = 300,
       width = 8, 
       height = 6, units = "in")# CO2 monthly plotted


#1988-2022 model
db_co2_2 <-subset(co2, (year > 1988 | (year >= 1988 & month >= 10)) &
                    (year < 2022 | (year == 2022 & month <= 9)))

db_co2_2$date <-as.Date(paste(db_co2_2$year, db_co2_2$month, "01", sep = "-")) # CO2 column union
sb_co2_2 <- data.frame(Date = db_co2_2$date, CO2 = db_co2_2$monthly_average)
sb_co2_2$index <- c(1:length(sb_co2_2$Date))


model_co2_2 <- lm( CO2 ~ index, data = sb_co2_2)

coefficients_co2_2 <- coef(model_co2_2)

# Print the equation
cat("Equation of the line: y =", coefficients_co2_2[1], "+", 
    coefficients_co2_2[2], "x\n")

# Create the equation as a string
equation_co2_2 <- paste("CO2 = ", round(coefficients_co2_2[1], 10),"+ ", 
                        round(coefficients_co2_2[2], 10), " Month", sep = "")


# Plot the data with the regression line and the equation
co2_linear_plot_2 <- ggplot() + 
  geom_point(data = sb_co2_2, aes(x = Date, y = CO2,  color = 'data points', group = 1), size = 1) +
  geom_smooth(data = sb_co2_2, 
              aes(x = Date, y = CO2,  color = 'linear regression', group = 1),
              method = 'lm', se = FALSE) +
  labs(x = "Date(monthly)", y = "CO2 (ppm)") +
  scale_color_viridis_d(option = "C")  +
  # Annotate to show the equation on the plot
  annotate("text", x = max(sb_co2_2$Date[length(sb_co2_2$Date)/3]), 
           y = max(sb_co2_2$CO2), 
           label = equation_co2_2, 
           color = "blue", 
           hjust = 0) +
  theme_minimal()

co2_linear_plot_2

ggsave(plot = co2_linear_plot_2, 
       "fig9.jpg", 
       dpi = 300,
       width = 8, 
       height = 6, units = "in")# CO2 monthly plotted



