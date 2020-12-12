# Algebra
# Ser på om kurvene gir noe

library(tidyverse)

datas <- tibble::tibble(x = seq(-9, 9, by = 0.1), 
                y = 1 / x,
                y2 = abs(-2 * x),
                y3 = sqrt(9*9 - x^2),
                y4 = -sqrt(9*9 - x^2),
                #y5 = -3 * abs(sin(x)),
                y6 = asin(-x / 3),
                y7 = asin(x / 3)
                ) %>% 
  dplyr::mutate(y = dplyr::case_when(x <= 0 ~ NaN,
                                     TRUE ~ y),
                y6 = dplyr::case_when(x <= -pi ~ NaN,
                                      x >= pi ~ NaN,
                                      TRUE ~ y6),
                y7 = dplyr::case_when(x <= -pi ~ NaN,
                                      x >= pi ~ NaN,
                                      TRUE ~ y7)
                )

ggplot2::ggplot(datas) +
  geom_line(aes(x, y)) +
  geom_line(aes(x, y2)) +
  geom_line(aes(x, y3)) +
  geom_line(aes(x, y4)) +
  #geom_line(aes(x, y5)) +
  geom_line(aes(x, y6)) +
  geom_line(aes(x, y7))
  theme_minimal()
