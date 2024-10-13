rm(list = ls())

#library initialization
library(ggplot2)
library(gridExtra) #for adding subplots
library(dplyr) #for special operators
library(corrplot)

v1 <- c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10)
v2 <- c(2,3,4,5,6,7,8,9,10,11,0,1,2,3,4,5,6,7,8,9 )

#positive corelation
positive_corr <- data.frame(v1,v2)
positive_corr_plot <- ggplot() + 
  geom_point(data = positive_corr, 
             aes(x = v1, y = v2), size = 1.1) +
  ggtitle("(a)") +
  labs(x = "v1", y = "v2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10))

#negative corelation
v3 <- v2[length(v2):1]
negative_corr <- data.frame(v1,v3)
negative_corr_plot <- ggplot() + 
  geom_point(data = negative_corr, 
             aes(x = v1, y = v3), size = 1.1) +
  ggtitle("(b)") +
  labs(x = "v1", y = "v2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10))

#complex correlation
v4 <- c(3,4,5,6,7,7,6,5,4,3,4,5,6,7,8,8,7,6,5,4)
complex_corr <- data.frame(v1,v4)
complex_corr_plot <- ggplot() + 
  geom_point(data = complex_corr, 
             aes(x = v1, y = v4), size = 1.1) +
  ggtitle("(c)") +
  labs(x = "v1", y = "v2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10))

#null correelation
v5 <- c(3,2,5,7,4,6,2,3,0,1,1,7,4,9,4,5,5,2,8,0)
null_corr <- data.frame(v1,v5)
null_corr_plot <- ggplot() + 
  geom_point(data = null_corr, 
             aes(x = v1, y = v5), size = 1.1) +
  ggtitle("(d)") +
  labs(x = "v1", y = "v2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=10))

# Arrange the plots in a grid
total_corr_plot <- grid.arrange(positive_corr_plot,
                                negative_corr_plot, 
                                complex_corr_plot, 
                                null_corr_plot, nrow = 2, ncol = 2)


ggsave(plot = total_corr_plot, 
       "fig4.jpg",
       dpi = 300,
       width = 8,
       height = 6, units = "in")
