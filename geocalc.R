# Solving mysteries...
library(tidyverse)
library(sf)
library(geosphere)

# BP25 Lost and found 2 ----
# Denne cachens plassering: A
# Lost and found 1: B
# En liten perle: C
# 
# Avstanden til denne er altså:
#   AC = 471,471 + AB
# Gitt at vi skal ta utgangspunkt i As opprinnelige plassering på kartet, så er
#   AB

points_AB <- tibble::tibble(
  name = c("A", "B"),
  lat = c(63 + 23.064 / 60, 63 + 23.098 / 60),
  lon = c(10 + 16.462 / 60, 10 + 17.041 / 60)
  ) %>% 
  sf::st_as_sf(., coords = c("lon", "lat"), crs = 4326, remove = FALSE)

distance_AB <- sf::st_distance(points_AB$geometry[1], points_AB$geometry[2])
distance_AC <- as.numeric(distance_AB) + 471.471

# Project from point A using geosphere
point_A_final <- geosphere::destPoint(p = c(10 + 16.462 / 60, 63 + 23.064 / 60),
                                      b = 0,
                                      d = distance_AC) %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(lon_min = (lon - trunc(lon)) * 60,
                lat_min = (lat - trunc(lat)) * 60)

#cache_a0 = sf::st_point(c(63 + 23.064 / 60, 10 + 16.462 / 60))
#cache_b  = sf::st_point(c(63 + 23.098 / 60, 10 + 17.041 / 60))

# Comparing distance by UTM
distance_AB_by_UTM <- sqrt((564177-563696)^2 + (7029125-7029052)^2)
