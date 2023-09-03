library(datasets) # for anscombe data
library(tidyverse) # for ggplot2 among others
library(patchwork) # combining plots together

# naming the data and loading it
quartet <- datasets::anscombe

# make plots

p1 <- ggplot(quartet) +
  geom_point(aes(x1, y1), color = "black", size = 2) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  expand_limits(x = 0, y = 0) +
  labs(x = "x1", y = "y1",
       title = "Dataset 1" ) +
  theme_minimal()
p2 <- ggplot(quartet) +
  geom_point(aes(x2, y2), color = "black", size = 2) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  expand_limits(x = 0, y = 0) +
  labs(x = "x2", y = "y2",
       title = "Dataset 2" ) +
  theme_minimal()


p3 <- ggplot(quartet) +
  geom_point(aes(x3, y3), color = "black", size = 2) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  expand_limits(x = 0, y = 0) +
  labs(x = "x3", y = "y3",
       title = "Dataset 3" ) +
  theme_minimal()

p4 <- ggplot(quartet) +
  geom_point(aes(x4, y4), color = "black", size = 2) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  expand_limits(x = 0, y = 0) +
  labs(x = "x4", y = "y4",
       title = "Dataset 4" ) +
  theme_minimal()


(p1 + p2) / (p3 + p4)


lm1 <- lm(y1 ~ x1, data = quartet)
lm1


p1_proper <- p1 + ggplot2::geom_abline(intercept = 3.0001, slope = 0.5001, color = "black")

p1_proper


lm2 <- lm(y2 ~ x2, data = quartet)
lm2

p2_proper <- p2 + ggplot2::geom_abline(intercept = 3.001, slope = 0.500, color = "black")

p2_proper

lm3 <- lm(y3 ~ x3, data = quartet)
lm3

p3_proper <- p3 + ggplot2::geom_abline(intercept = 3.0025, slope = 0.4997, color = "black")

p3_proper

lm4 <- lm(y4 ~ x4, data = quartet)
lm4

p4_proper <- p4 + ggplot2::geom_abline(intercept = 3.0017, slope = 0.4999, color = "black")

p4_proper

(p1_proper + p2_proper) / (p3_proper + p4_proper)




