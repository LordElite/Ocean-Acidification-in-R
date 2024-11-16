rm(list = ls())

#library initialization
library(ggplot2)
library(tidyverse)
library(lubridate)
library(tidyr)
library(dplyr)
library(scico)
library(viridis)
library(robustbase)
library(MASS)
library(L1pack)

#ph dataset
ocean <- data.frame(read.csv('directory/seawater-ph.csv')) # oceanic pH database 
ph_ocean <- data.frame(Date = as.Date(ocean$Day),
                       pH = ocean$Monthly.measurement.of.ocean.pH.levels) # oceanic ph monthly
ph_ocean$Year <-as.numeric(format(ph_ocean$Date, '%Y'))  #extracting Year from Date column
ph_ocean$Month <-as.numeric(format(ph_ocean$Date, '%m')) #extracting Month from Date column


# CO2 dataset
co2 <-  data.frame(read.csv('directory/co2_mm_mlo.csv'))

db_co2 <-subset(co2, (year > 1988 | (year == 1988 & month >= 10)) &
                  (year < 2022 | (year == 2022 & month <= 9)))
db_co2$date <-as.Date(paste(db_co2$year, db_co2$month, "01", sep = "-")) # CO2 column union

sb_co2 <- data.frame(Date = db_co2$date, CO2 = db_co2$monthly_average)

sb_co2$Year <-as.numeric(format(sb_co2$Date, '%Y'))  #extracting Year from Date column
sb_co2$Month <-as.numeric(format(sb_co2$Date, '%m')) #extracting Month from Date column
# Merge the datasets based on the Date column
merged_data <- merge(ph_ocean, sb_co2, by=c("Year","Month"), all=FALSE)

# Remove rows with any missing values
clean_data <- na.omit(merged_data)
corr_data <- data.frame(pH = clean_data$pH, CO2 = clean_data$CO2)


#linear regression
ols_model <- lm( pH ~ CO2, data = corr_data)
# Fit the LMS and LTS models
lad_model <- lad(pH ~ CO2, data =  corr_data)       # LAD model
lms_model <- lqs(pH ~ CO2, data = corr_data, method = "lms")  # LMS model
lts_model <- lqs(pH ~ CO2, data = corr_data, method = "lts")  # LTS model

# Extract the intercept and slope
intercept_lad <- lad_model$coefficients[1]
slope_lad <- lad_model$coefficients[2]

# Extract coefficients for LMS and LTS models
intercept_lms <- coef(lms_model)[1]
slope_lms <- coef(lms_model)[2]

intercept_lts <- coef(lts_model)[1]
slope_lts <- coef(lts_model)[2]

# Create a new data frame for the fitted line
fitted_line_lad <- data.frame(
  x = corr_data$CO2,
  y = intercept_lad + slope_lad * corr_data$CO2
)


# Create data frames for the fitted lines
fitted_line_lms <- data.frame(
  x = corr_data$CO2,
  y = intercept_lms + slope_lms * corr_data$CO2
)

fitted_line_lts <- data.frame(
  x = corr_data$CO2,
  y = intercept_lts + slope_lts * corr_data$CO2
)

coefficients_ols <- coef(ols_model)
# Print the equation
cat("Equation of the line: pH =", coefficients_ols[1], "+", 
    coefficients_ols[2], "CO2\n")

# Create the equation as a string
equation_ols<- paste("pH = ", round(coefficients_ols[1], 10)," ", 
                         round(coefficients_ols[2], 10), " CO2", sep = "")

# Extract the coefficients   #LAD
coefficients_lad<- coef(lad_model)
# Print the equation
cat("Equation of the line: pH =", coefficients_lad[1], "+", 
    coefficients_lad[2], "CO2\n")
# Create the equation as a string
equation_lad <- paste("pH = ", round(coefficients_lad[1], 3), " + ", 
                      round(coefficients_lad[2], 3), "CO2", sep = "")


# Extract the coefficients   #LMS
coefficients_lms<- coef(lms_model)
# Print the equation
cat("Equation of the line: pH =", coefficients_lms[1], "+", 
    coefficients_lms[2], "CO2\n")
# Create the equation as a string
equation_lms <- paste("pH = ", round(coefficients_lms[1], 3), " + ", 
                      round(coefficients_lms[2], 3), "CO2", sep = "")


# Extract the coefficients   #LTS
coefficients_lts<- coef(lts_model)
# Print the equation
cat("Equation of the line: pH =", coefficients_lts[1], "+", 
    coefficients_lts[2], "CO2\n")
# Create the equation as a string
equation_lts <- paste("pH = ", round(coefficients_lts[1], 3), " + ", 
                      round(coefficients_lts[2], 3), "CO2", sep = "")     



# Plot the data with the regression line and the equation
co2_ph_linear_plot <- ggplot() + 
  geom_point(data = corr_data, aes(x = CO2, y = pH, color = 'data points', group = 1), 
             size = 1) +
  geom_smooth(data = corr_data, 
              aes(x = CO2, y = pH, color = 'OLS', group = 1), 
              method = 'lm', se = FALSE) +
  geom_line(data =corr_data, aes(x = CO2,
                            y = fitted_line_lad$y, 
                            color = "LAD"), size = 1) +     # LAD line
  geom_line(data =corr_data, aes(x = CO2,
                            y = fitted_line_lms$y, 
                            color = "LMS"), size = 1) +     # LMS line
  geom_line(data =corr_data, aes(x = CO2,
                            y = fitted_line_lts$y, 
                            color = "LTS"), size = 1) +     # LTS line
  labs(x = "CO2 [ppm]", y = "Ocean Acidity (pH)", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  theme_minimal()

co2_ph_linear_plot

ggsave(plot = co2_ph_linear_plot, 
       "fig25.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")

#calculation of MAE and MAD

# Calculate residuals for each model
residuals_ols <- corr_data$pH - predict(ols_model, corr_data)
residuals_lad <- corr_data$pH - predict(lad_model, corr_data)
residuals_lms <- corr_data$pH - predict(lms_model, corr_data)
residuals_lts <- corr_data$pH - predict(lts_model, corr_data)

# Function to calculate MAE
calc_mae <- function(residuals) {
  mean(abs(residuals))
}

# Function to calculate MAD
calc_mad <- function(residuals) {
  median(abs(residuals))
}

# MAE and MAD for each model
mae_ols <- calc_mae(residuals_ols)
mad_ols <- calc_mad(residuals_ols)

mae_lad <- calc_mae(residuals_lad)
mad_lad <- calc_mad(residuals_lad)

mae_lms <- calc_mae(residuals_lms)
mad_lms <- calc_mad(residuals_lms)

mae_lts <- calc_mae(residuals_lts)
mad_lts <- calc_mad(residuals_lts)

# Print results
cat("MAE values:\n",
    "OLS:", mae_ols, "\n",
    "LAD:", mae_lad, "\n",
    "LMS:", mae_lms, "\n",
    "LTS:", mae_lts, "\n\n")

cat("MAD values:\n",
    "OLS:", mad_ols, "\n",
    "LAD:", mad_lad, "\n",
    "LMS:", mad_lms, "\n",
    "LTS:", mad_lts, "\n")


#LOESS MODEL

#function to optimize loess span parameter in CO2 dataset
calcSSE_co2_ph <- function(x){
  loessMod <- try(loess(pH ~ CO2, data=corr_data, span=x), silent=T)
  res <- try(loessMod$residuals, silent=T)
  if(!inherits(res, "try-error")){
    sse <- sum(res^2)  
  }else{
    sse <- 99999
  }
  return(sse)
}

optimal_span_co2_ph <- optimize(calcSSE_co2_ph, c(0.01,1))


# plotting
co2_ph_optimal_loess_plot <- ggplot() + 
  geom_point(data = corr_data, 
             aes(x = CO2, y = pH,, color = "Data Points"), size = 0.7) +
  geom_smooth(data = corr_data, 
              aes(x = CO2, y = pH, 
                  color = "optimal span: 0.0329"), 
              method = 'loess', 
              span = optimal_span_co2_ph$minimum, 
              se = FALSE, 
              size = 1) +
  geom_smooth(data = corr_data, 
              aes(x = CO2, y = pH, 
                  color = "span: 0.1"), 
              method = 'loess', 
              span = 0.1, 
              se = FALSE, 
              size = 1) +
  geom_smooth(data = corr_data, 
              aes(x = CO2, y = pH, 
                  color = "span: 0.5"), 
              method = 'loess', 
              span = 0.5, 
              se = FALSE, 
              size = 1) +
  geom_smooth(data = corr_data, 
              aes(x = CO2, y = pH, 
                  color = "span: 1"), 
              method = 'loess', 
              span = 1, 
              se = FALSE, 
              size = 1) +
  labs(x = "CO2 [ppm]", y = "ocean acidity [pH]", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  theme_minimal()

co2_ph_optimal_loess_plot


ggsave(plot = co2_ph_optimal_loess_plot, 
       "fig26.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")
