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

#function to optimize span parameter 
calcSSE <- function(x){
  loessMod <- try(loess(pH ~ index, data=ph_ocean, span=x), silent=T)
  res <- try(loessMod$residuals, silent=T)
  if(!inherits(res, "try-error")){
    sse <- sum(res^2)  
  }else{
    sse <- 99999
  }
  return(sse)
}

# Plot the data with the regression line and the equation
ph_loess_plot <- ggplot() + 
  geom_point(data = ph_ocean, 
             aes(x = Date, y = pH, color = "Data Points"), size = 0.7) +
  geom_smooth(data = ph_ocean, 
              aes(x = Date, y = pH, color = "span: 0.05"), method = 'loess', 
              span = 0.05, se = FALSE, size = 1) +
  geom_smooth(data = ph_ocean, 
              aes(x = Date, y = pH, color = "span: 0.25"), method = 'loess', 
              span = 0.25, se = FALSE, size = 1) +
  geom_smooth(data = ph_ocean, 
              aes(x = Date, y = pH, color = "span: 0.5"), method = 'loess', 
              span = 0.5, se = FALSE, size = 1) +
  geom_smooth(data = ph_ocean, 
              aes(x = Date, y = pH, color = "span: 1"), method = 'loess', 
              span = 1, se = FALSE, size = 1) +
  labs(x = "Date (Monthly)", y = "Acidity (pH)", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  theme_minimal()
ph_loess_plot

ggsave(plot = ph_loess_plot, 
       "fig10.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")

#function to optimize span parameter in ocean dataset
calcSSE_ph <- function(x){
  loessMod <- try(loess(pH ~ index, data=ph_ocean, span=x), silent=T)
  res <- try(loessMod$residuals, silent=T)
  if(!inherits(res, "try-error")){
    sse <- sum(res^2)  
  }else{
    sse <- 99999
  }
  return(sse)
}

optimal_span_ph <- optimize(calcSSE_ph, c(0.01,1))

# plotting
optimal_ph_plot_loess <- ggplot() + 
  geom_line(data = ph_ocean, 
            aes(x = Date, y = pH, group = 1, color = "Original pH Data"), size = 1) + 
  geom_point(data = ph_ocean, 
             aes(x = Date, y = pH, color = "Data Points"), size = 0.7) +
  geom_smooth(data = ph_ocean, 
              aes(x = Date, y = pH, 
                  color = "span: 0.128"), 
              method = 'loess', 
              span = optimal_span_ph$minimum, 
              se = FALSE, 
              size = 1) +
  labs(x = "Date (Monthly)", y = "Acidity (pH)", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  theme_minimal()

optimal_ph_plot_loess

ggsave(plot = optimal_ph_plot_loess, 
       "fig11.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")

#codeblock to get the optimal span
SE_vector <- c()
span_vector = c(0.05, optimal_span_ph$minimum, 0.25, 0.5, 1)
for (i in span_vector) {
  model_loess <- loess(pH ~ index, data = ph_ocean, span = i)
  residuals <- ph_ocean$pH - predict(model_loess)
  SE_vector <- append(SE_vector, sqrt(mean(residuals^2)) )  # Standard Error
}
SE_dataframe <- data.frame( "span"= span_vector,
                            "SE"= SE_vector)



#CO2 LOESS model
# CO2 dataset
co2 <-  data.frame(read.csv('directory/co2_mm_mlo.csv'))

db_co2_1 <-subset(co2, (year > 1958 | (year >= 1958 & month >= 3)) &
                    (year < 2024 | (year == 2024 & month <= 7)))

db_co2_1$date <-as.Date(paste(db_co2_1$year, db_co2_1$month, "01", sep = "-")) # CO2 column union
sb_co2_1 <- data.frame(Date = db_co2_1$date, CO2 = db_co2_1$monthly_average)
sb_co2_1$index <- c(1:length(sb_co2_1$Date))
#function to optimize span parameter in CO2 dataset
calcSSE_co2_1 <- function(x){
  loessMod <- try(loess(CO2 ~ index, data=sb_co2_1, span=x), silent=T)
  res <- try(loessMod$residuals, silent=T)
  if(!inherits(res, "try-error")){
    sse <- sum(res^2)  
  }else{
    sse <- 99999
  }
  return(sse)
}

optimal_span_co2_1 <- optimize(calcSSE_co2_1, c(0.01,1))


# plotting
co2_1_optimal_loess_plot <- ggplot() + 
  geom_point(data = sb_co2_1, 
             aes(x = Date, y = CO2,, color = "Data Points"), size = 0.7) +
  geom_smooth(data = sb_co2_1, 
              aes(x = Date, y = CO2,, 
                  color = "span: 0.067"), 
              method = 'loess', 
              span = optimal_span_co2_1$minimum, 
              se = FALSE, 
              size = 1) +
  labs(x = "Date (Monthly)", y = "CO2 (ppm)", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  theme_minimal()

co2_1_optimal_loess_plot

ggsave(plot = co2_1_optimal_loess_plot, 
       "fig12.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")

#1988-2022 model
db_co2_2 <- subset(co2, (year > 1988 | (year >= 1988 & month >= 10)) &
         (year < 2022 | (year == 2022 & month <= 9)))


db_co2_2$date <-as.Date(paste(db_co2_2$year, db_co2_2$month, "01", sep = "-")) # CO2 column union
sb_co2_2 <- data.frame(Date = db_co2_2$date, CO2 = db_co2_2$monthly_average)
sb_co2_2$index <- c(1:length(sb_co2_2$Date))
#function to optimize span parameter in CO2 dataset
calcSSE_co2_2 <- function(x){
  loessMod <- try(loess(CO2 ~ index, data=sb_co2_2, span=x), silent=T)
  res <- try(loessMod$residuals, silent=T)
  if(!inherits(res, "try-error")){
    sse <- sum(res^2)  
  }else{
    sse <- 99999
  }
  return(sse)
}

optimal_span_co2_2 <- optimize(calcSSE_co2_2, c(0.01,1))


# plotting
co2_2_optimal_loess_plot <- ggplot() + 
  geom_point(data = sb_co2_2, 
             aes(x = Date, y = CO2,, color = "Data Points"), size = 0.7) +
  geom_smooth(data = sb_co2_2, 
              aes(x = Date, y = CO2,, 
                  color = "span: 0.172"), 
              method = 'loess', 
              span = optimal_span_co2_2$minimum, 
              se = FALSE, 
              size = 1) +
  labs(x = "Date (Monthly)", y = "CO2 (ppm)", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  theme_minimal()

co2_2_optimal_loess_plot

ggsave(plot = co2_2_optimal_loess_plot, 
       "fig13.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")
