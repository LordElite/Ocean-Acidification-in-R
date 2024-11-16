rm(list = ls())

#library initialization
library(ggplot2)
library(tidyverse)
library(lubridate)
library(tidyr)
library(dplyr)
library(corrplot)
library(energy)  #pack to perform distance correlation
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

correlation <- corrplot(cor(corr_data), method = "number" )



pearson <- cor(corr_data$CO2, corr_data$pH, method = "pearson") # Apply pearson correlation

spearman <- cor(corr_data$CO2, corr_data$pH, method = "spearman") # Apply Spearman's ro

Kendall <- cor(corr_data$CO2, corr_data$pH, method = "kendall") # Apply Kendall's tau


distance_corr <- dcor(corr_data$CO2, corr_data$pH) # Apply distance correlation

correlation_coeff <- data.frame("pearson" = pearson, 
                                "spearman" = spearman, 
                                "kendall" = Kendall, 
                                "distance corr" = distance_corr )
#ph vs co2 plot
ph_co2_plot <- ggplot() + 
  geom_point(data = corr_data, 
             aes(x = CO2, y = pH, color = "Data Points"), size = 1.1) +
  labs(x = "CO2 [ppm]", y = "Ocean acidification [pH]", color = "Legend") +
  scale_color_viridis_d(option = "C")  +
  theme_minimal()

ph_co2_plot

ggsave(plot = ph_co2_plot, 
       "fig24.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")

