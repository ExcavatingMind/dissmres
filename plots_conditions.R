library(tidyverse)
library(ggpubr)
library(MASS)


set.seed(1234)

my_sample_size = 128

my_desired_r = 0.6

mean_variable_1 = 0
sd_variable_1 = 1

mean_variable_2 = 0
sd_variable_2 = 1

mu <- c(mean_variable_1, mean_variable_2) 

myr <- my_desired_r * sqrt(sd_variable_1) * sqrt(sd_variable_2)

mysigma <- matrix(c(sd_variable_1, myr, myr, sd_variable_2), 2, 2) 

corr_data = as_tibble(mvrnorm(my_sample_size, mu, mysigma, empirical = TRUE))

corr_model <- lm(V2 ~ V1, data = corr_data)

my_residuals <- abs(residuals(corr_model))

data_with_resid <- round(cbind(corr_data, my_residuals), 2)




slopes <- data_with_resid %>%
  mutate(slope_linear = my_residuals/3.2) %>%
  mutate(slope_0.25 = 1-(0.25)^my_residuals) %>%
  mutate(slope_inverted = (1 + (0.25)^ my_residuals)-1)


plot_example_function <- function (d, x, t) {
  
  set.seed(1234)
  
  ggplot(d, aes(x = V1, y = V2)) +
    scale_alpha_identity() +
    geom_point(aes(alpha = x), shape = 16, size = 1)  +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(axis.text = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm"),
          legend.position = "none",
          plot.title = element_text(size = 15)) +
    labs(title = t)
  
}

ggarrange(plot_example_function(slopes, (1-slopes$slope_0.25), "Nonlinear Decay"),
          plot_example_function(slopes, (1-slopes$slope_linear), "Linear Decay"),
          plot_example_function(slopes, (1-slopes$slope_inverted), "Inverted Nonlinear Decay"),
          plot_example_function(slopes, 1, "Full Contrast"))




























library(ggpubr)



  
  set.seed(1234)
  
  my_sample_size = 128
  
  my_desired_r = 0.6
  
  mean_variable_1 = 0
  sd_variable_1 = 1
  
  mean_variable_2 = 0
  sd_variable_2 = 1
  
  mu <- c(mean_variable_1, mean_variable_2) 
  
  myr <- my_desired_r * sqrt(sd_variable_1) * sqrt(sd_variable_2)
  
  mysigma <- matrix(c(sd_variable_1, myr, myr, sd_variable_2), 2, 2) 
  
  corr_data = as_tibble(mvrnorm(my_sample_size, mu, mysigma, empirical = TRUE))
  
  corr_model <- lm(V2 ~ V1, data = corr_data)
  
  my_residuals <- abs(residuals(corr_model))
  
  data_with_resid <- round(cbind(corr_data, my_residuals), 2)
  
  slopes <- data_with_resid %>%
    mutate(slope_linear = my_residuals/3.2) %>%
    mutate(slope_0.25 = 1-(0.25)^my_residuals) %>%
    mutate(slope_inverted = (1 + (0.25)^ my_residuals)-1)
  
  plot_function <- function (d, x, t) {
    
    set.seed(1234)
    
    ggplot(d, aes(x = V1, y = V2)) +
      scale_size_identity() +
      geom_point(aes(size = 4*(x + 0.2)), shape = 16)  +
      labs(x = "", y = "") +
      theme_classic() +
      theme(axis.text = element_blank(),
            plot.margin = unit(c(0,0,0,0), "cm"),
            legend.position = "none",
            plot.title = element_text(size = 15)) +
      labs(title = t)
    
  }  
  
image <- ggarrange(plot_function(slopes, (1-slopes$slope_0.25), "Nonlinear Decay"),
                     plot_function(slopes, (1-slopes$slope_linear), "Linear Decay"),
                     plot_function(slopes, (1-slopes$slope_inverted), "Inverted Nonlinear Decay"),
                     plot_function(slopes, 0.05, "Standard Size"))

image

ggsave("hoping3.svg", plot = image, height = 4, width = 5, dpi = 300)






