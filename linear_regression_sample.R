rm(list = ls())

library(ggplot2)
library(gridExtra)
library(scico)
library(viridis)
library(robustbase)
library(MASS)
library(L1pack)
# generating data
x <- c() #independent variable
y <- c() #dependent variable
for (i in 1:30) {
  a <- sample(2:15,1, replace = T) #random parameter
  x <- append(x, a)  
  y <- append(y, a*2 + sample(-2:2,1, replace = T)) 
}
Data <- data.frame(x , y)

# Fit the linear model
ols_model <- lm(y ~ x, data = Data)

# Extract the coefficients
coefficients_ols <- coef(ols_model)

# Print the equation
cat("Equation of the line: y =", coefficients_ols[1], "+", 
    coefficients_ols[2], "x\n")

# Create the equation as a string
equation_ols <- paste("y = ", round(coefficients_ols[1], 3), " + ", 
                  round(coefficients_ols[2], 3), "x", sep = "")



# Fit the LMS and LTS models
lad_model <- lad(y ~ x, data = Data)       # LAD model
lms_model <- lqs(y ~ x, data = Data, method = "lms")  # LMS model
lts_model <- lqs(y ~ x, data = Data, method = "lts")  # LTS model

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
  x = Data$x,
  y = intercept_lad + slope_lad * Data$x
)


# Create data frames for the fitted lines
fitted_line_lms <- data.frame(
  x = Data$x,
  y = intercept_lms + slope_lms * Data$x
)

fitted_line_lts <- data.frame(
  x = Data$x,
  y = intercept_lts + slope_lts * Data$x
)


# Extract the coefficients   #LAD
coefficients_lad<- coef(lad_model)
# Print the equation
cat("Equation of the line: y =", coefficients_lad[1], "+", 
    coefficients_lad[2], "x\n")
# Create the equation as a string
equation_lad <- paste("y = ", round(coefficients_lad[1], 3), " + ", 
                     round(coefficients_lad[2], 3), "x", sep = "")


# Extract the coefficients   #LMS
coefficients_lms<- coef(lms_model)
# Print the equation
cat("Equation of the line: y =", coefficients_lms[1], "+", 
    coefficients_lms[2], "x\n")
# Create the equation as a string
equation_lms <- paste("y = ", round(coefficients_lms[1], 3), " + ", 
                      round(coefficients_lms[2], 3), "x", sep = "")


# Extract the coefficients   #LTS
coefficients_lts<- coef(lts_model)
# Print the equation
cat("Equation of the line: y =", coefficients_lts[1], "+", 
    coefficients_lts[2], "x\n")
# Create the equation as a string
equation_lts <- paste("y = ", round(coefficients_lts[1], 3), " + ", 
                      round(coefficients_lts[2], 3), "x", sep = "")


# Plot the data with the regression line and the equation
plot_line <- ggplot() + 
  geom_point(data = Data, aes(x = x,
                              y = y, 
                              color = 'data points', 
                              group = 1), size = 0.9) +
  geom_smooth(data = Data, aes(x = x, 
                               y = y, 
                               color = 'OLS', 
                               group = 1),method = 'lm',se = FALSE) +
  geom_line(data =Data, aes(x = x,
                y = fitted_line_lad$y, 
                color = "LAD"), size = 1) +     # LAD line
  geom_line(data =Data, aes(x = x,
                            y = fitted_line_lms$y, 
                            color = "LMS"), size = 1) +     # LMS line
  geom_line(data =Data, aes(x = x,
                            y = fitted_line_lts$y, 
                            color = "LTS"), size = 1) +     # LTS line
  labs(x = "x", y = "y", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  theme_minimal()

plot_line

#regression after adding a random outlier
Data_out <- Data
b <- sample(2:15,1, replace = T) 
Data_out <- rbind(Data_out, c(2 , 3*b))

# Fit the linear models
ols_model_out <- lm(y ~ x, data = Data_out)  #OLQ model
lad_model_out <- lad(y ~ x, data = Data_out)       # LAD model
lms_model_out <- lqs(y ~ x, data = Data_out, method = "lms")  # LMS model
lts_model_out <- lqs(y ~ x, data = Data_out, method = "lts")  # LTS model

# Extract the intercept and slope
intercept_lad_out <- lad_model_out$coefficients[1]
slope_lad_out <- lad_model_out$coefficients[2]

# Extract coefficients for LMS and LTS models
intercept_lms_out <- coef(lms_model_out)[1]
slope_lms_out <- coef(lms_model_out)[2]

intercept_lts_out <- coef(lts_model_out)[1]
slope_lts_out <- coef(lts_model_out)[2]

# Create a new data frame for the fitted line
fitted_line_lad_out <- data.frame(
  x = Data_out$x,
  y = intercept_lad_out + slope_lad_out * Data_out$x
)


# Create data frames for the fitted lines
fitted_line_lms_out <- data.frame(
  x = Data_out$x,
  y = intercept_lms_out + slope_lms_out * Data_out$x
)

fitted_line_lts_out <- data.frame(
  x = Data_out$x,
  y = intercept_lts_out + slope_lts_out * Data_out$x
)


# Extract the coefficients   #LAD
coefficients_lad_out<- coef(lad_model_out)
# Print the equation
cat("Equation of the line: y =", coefficients_lad_out[1], "+", 
    coefficients_lad_out[2], "x\n")
# Create the equation as a string
equation_lad_out <- paste("y = ", round(coefficients_lad_out[1], 3), " + ", 
                      round(coefficients_lad_out[2], 3), "x", sep = "")


# Extract the coefficients   #LMS
coefficients_lms_out<- coef(lms_model_out)
# Print the equation
cat("Equation of the line: y =", coefficients_lms_out[1], "+", 
    coefficients_lms_out[2], "x\n")
# Create the equation as a string
equation_lms_out <- paste("y = ", round(coefficients_lms_out[1], 3), " + ", 
                      round(coefficients_lms_out[2], 3), "x", sep = "")


# Extract the coefficients   #LTS
coefficients_lts_out <- coef(lts_model_out)
# Print the equation
cat("Equation of the line: y =", coefficients_lts_out[1], "+", 
    coefficients_lts_out[2], "x\n")
# Create the equation as a string
equation_lts_out <- paste("y = ", round(coefficients_lts_out[1], 3), " + ", 
                      round(coefficients_lts_out[2], 3), "x", sep = "")

# Extract the coefficients
coefficients_ols_out <- coef(ols_model_out)

# Print the equation
cat("Equation of the line: y =", coefficients_ols_out[1], "+", 
    coefficients_ols_out[2], "x\n")

# Create the equation as a string
equation_ols_out <- paste("y = ", round(coefficients_ols_out[1], 3), " + ", 
                      round(coefficients_ols_out[2], 3), "x", sep = "")

#plot section with subplots 
# Plot the data with the regression line and the equation
plot_out <- ggplot() + 
  geom_point(data = Data_out, aes(x = x, 
                              y = y, 
                              color = 'data points', 
                              group = 1), size = 0.9) +
  geom_smooth(data = Data_out, aes(x = x, 
                               y = y,  
                               color = "OLS", 
                               group = 1),method = 'lm',se = FALSE) +
  geom_line(data =Data_out, aes(x = x,
                            y = fitted_line_lad_out$y, 
                            color = "LAD"), size = 1) +     # LAD line
  geom_line(data =Data_out, aes(x = x,
                            y = fitted_line_lms_out$y, 
                            color = "LMS"), size = 1) +     # LMS line
  geom_line(data =Data_out, aes(x = x,
                            y = fitted_line_lts_out$y, 
                            color = "LTS"), size = 1) +     # LTS line
  labs(x = "x", y = "y", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  theme_minimal()

plot_out

# Arrange the plots in a grid
total_plot <- grid.arrange(plot_line, plot_out, ncol = 2)
# Save the plot as a jpg  file with 300 DPI
ggsave(plot = plot_line, "fig1.jpg", dpi = 300, width = 8, height = 6, units = "in")
ggsave(plot = plot_out, "fig2.jpg", dpi = 300, width = 8, height = 6, units = "in")
 


