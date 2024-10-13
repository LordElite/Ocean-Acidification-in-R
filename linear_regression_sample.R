rm(list = ls())

library(ggplot2)
library(gridExtra)
library(scico)
library(viridis)

# generating data
x <- c() #independent variable
y <- c() #dependent variable
for (i in 1:30) {
  a <- sample(2:15,1, replace = T) #random parameter
  x <- append(x, a)  
  y <- append(y, a*2 + sample(-2:2,1, replace = T)) 
}
Data <- data.frame(x,y)

# Fit the linear model
model <- lm(y ~ x, data = Data)

# Extract the coefficients
coefficients <- coef(model)

# Print the equation
cat("Equation of the line: y =", coefficients[1], "+", 
    coefficients[2], "x\n")

# Create the equation as a string
equation <- paste("y = ", round(coefficients[1], 3), " + ", 
                  round(coefficients[2], 3), "x", sep = "")

# Plot the data with the regression line and the equation
ggplot() + 
  geom_point(data = Data, aes(x = x, y = y, color = 'data points', group = 1), size = 0.9) +
  geom_smooth(data = Data, aes(x = x, y = y,  color = 'linear regression', group = 1),method = 'lm',se = FALSE) +
  labs(x = "x", y = "y", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  # Annotate to show the equation on the plot
  annotate("text", x = max(Data$x) * 0.7, 
           y = max(Data$y), 
           label = equation, 
           color = "black", 
           hjust = 0) +
  theme_minimal()
# Save the plot as a jpg  file with 300 DPI
ggsave("linear_sample.jpg", dpi = 300, width = 8, height = 6, units = "in")

#regression after adding a random outlier
Data_out <- Data
b <- sample(2:15,1, replace = T) 
Data_out <- rbind(Data_out, c(2 , 3*b))

# Fit the linear model
model_out <- lm(y ~ x, data = Data_out)

# Extract the coefficients
coefficients_out <- coef(model_out)

# Print the equation
cat("Equation of the line: y =", coefficients_out[1], "+", 
    coefficients_out[2], "* x\n")

# Create the equation as a string
equation_out <- paste("y = ", round(coefficients_out[1], 3), " + ", 
                      round(coefficients_out[2], 3), " * x", sep = "")

#plot section with subplots 
# Plot the data with the regression line and the equation
plot_line <- ggplot() + 
  geom_point(data = Data, aes(x = x, y = y, color = 'data points', group = 1), size = 0.9) +
  geom_smooth(data = Data, aes(x = x, y = y,  color = 'linear regression', group = 1),method = 'lm',se = FALSE) +
  labs(x = "x", y = "y", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  # Annotate to show the equation on the plot
  annotate("text", x = max(Data$x) * 0.7, 
           y = max(Data$y), 
           label = equation, 
           color = "black", 
           hjust = 0) +
  theme_minimal()

# Plot the data with the regression line and the equation after adding outliers
plot_out <- ggplot() + 
  geom_point(data = Data_out, aes(x = x, y = y, color = 'data points', group = 1), size = 0.9) +
  geom_smooth(data = Data, aes(x = x, y = y,  color = 'linear regression', group = 1),method = 'lm',se = FALSE) +
  labs(x = "x", y = "y", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  # Annotate to show the equation on the plot
  annotate("text", x = max(Data$x) * 0.7, 
           y = max(Data$y), 
           label = equation_out, 
           color = "black", 
           hjust = 0) +
  theme_minimal()


# Arrange the plots in a grid
total_plot <- grid.arrange(plot_line, plot_out, ncol = 2)
# Save the plot as a jpg  file with 300 DPI
ggsave(plot = plot_line, "fig1.jpg", dpi = 300, width = 8, height = 6, units = "in")
ggsave(plot = plot_out, "fig2.jpg", dpi = 300, width = 8, height = 6, units = "in")

