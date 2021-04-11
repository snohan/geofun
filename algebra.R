# Algebra
# Ser på om kurvene gir noe

library(tidyverse)

datas <- tibble::tibble(x = seq(-4, 4, by = 0.1), 
                y1 = 1 / x,
                y2 = abs(-2 * x),
                y3 = sqrt(9 - x^2),
                y4 = -sqrt(9 - x^2),
                #y5 = -3 * abs(sin(x)),
                y6 = asin(-x / 3),
                y7 = asin(x / 3)
                ) %>% 
  dplyr::mutate(y1 = dplyr::case_when(x <= 0 ~ NaN,
                                     TRUE ~ y1),
                y6 = dplyr::case_when(x <= -pi ~ NaN,
                                      x >= pi ~ NaN,
                                      TRUE ~ y6),
                y7 = dplyr::case_when(x <= -pi ~ NaN,
                                      x >= pi ~ NaN,
                                      TRUE ~ y7)
                # y5 = dplyr::case_when(x <= -pi ~ NaN,
                #                       x >= pi ~ NaN,
                #                       TRUE ~ y5)
                )

ggplot2::ggplot(datas) +
  # geom_line(aes(x, y1)) +
  # geom_line(aes(x, y2)) +
  # geom_line(aes(x, y3)) +
  # geom_line(aes(x, y4)) +
  # #geom_line(aes(x, y5)) +
  # geom_line(aes(x, y6)) +
  # geom_line(aes(x, y7)) +
  #geom_line(aes(x, y8)) +
  #geom_line(aes(x, 1 / tan(x))) +
  geom_line(aes(x, 1/x)) +
  theme_minimal()
