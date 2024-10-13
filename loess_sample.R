rm(list = ls())

library(ggplot2)
library(patchwork)
library(scico)
library(viridis)

# generating data
x <- c() #independent variable
y <- c() #dependent variable
for (i in 1:50) {
  a <- sample(2:15,1, replace = T) #random parameter
  x <- append(x, a)  
  y <- append(y, a*2 + sample(-3:3,1, replace = T)**2) 
}
Data <- data.frame(x,y)



# Plot the data with the regression line and the legend
ggplot() + 
  geom_point(data = Data, aes(x = x, y = y, color = 'data points', group = 1), size = 0.9) +
  
  # Plot first loess curve
  geom_smooth(data = Data, 
              aes(x = x, y = y, color = "span = 0.25"), 
              method = 'loess', span = 0.25, se = FALSE, size = 1) +
  
  # Plot second loess curve
  geom_smooth(data = Data, 
              aes(x = x, y = y, color = "span = 0.5"), 
              method = 'loess', span = 0.5, se = FALSE, size = 1) +
  
  # Plot third loess curve
  geom_smooth(data = Data, 
              aes(x = x, y = y, color = "span = 0.75"), 
              method = 'loess', span = 0.75, se = FALSE, size = 1) +
  
  # Plot fourth loess curve
  geom_smooth(data = Data, 
              aes(x = x, y = y, color = "span = 1"), 
              method = 'loess', span = 1, se = FALSE, size = 1) +
  
  # Add labels
  labs(x = "x", y = "y", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  theme_minimal()


