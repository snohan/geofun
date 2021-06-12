# Solving mysteries...
library(tidyverse)
library(sf)

# A Platonic Cache ----
# Must be a regular icosahedron circumscribed by a sphere, 
# thus with its 12 vertices tangent to the sphere's surface.

# Points given
# N 52° 22.412 E 173° 20.792
# N 36° 38.504 W 93° 40.426
# N 26° 27.182 E 93° 17.523
# N 7° 26.881 W 31° 22.508
# N 2° 4.095 E 32° 5.978

# Using spherical coordinates, with 0 degrees at the north pole and the prime meridian.
circumradius <- sqrt(((1 + sqrt(5)) * 0.5 + 2))

# Points in decimal degrees
points <- tibble::tibble(
  lat = c(
    52 + 22.412 / 60,
    - (52 + 22.412 / 60),
    36 + 38.504 / 60,
    - (36 + 38.504 / 60),
    26 + 27.182 / 60,
    - (26 + 27.182 / 60),
    7 + 26.881 / 60,
    - (7 + 26.881 / 60),
    2 + 4.095 / 60,
    - (2 + 4.095 / 60)
    ),
  lon = c(
    173 + 20.792 / 60,
    173 + 20.792 / 60 - 180,
    - (93 + 40.426 / 60),
    - (93 + 40.426 / 60) + 180,
    93 + 17.523 / 60,
    93 + 17.523 / 60 - 180,
    - (31 + 22.508 / 60),
    - (31 + 22.508 / 60) + 180,
    32 + 5.978 / 60,
    32 + 5.978 / 60 - 180
    )
  ) %>% 
  #sf::st_as_sf(., coords = c("lat", "lon"), crs = 4326, remove = FALSE) %>%
  dplyr::mutate(
    latrad = (90 - lat) / 180 * pi,
    lonrad = lon / 180 * pi,
    latadjusted = latrad + (90 - 52.373522) / 180 * pi,
    lonadjusted = lonrad + (180 - 173.346533) / 180 * pi,
    x = circumradius * sin(latrad) * cos(lonrad),
    y = circumradius * sin(latrad) * sin(lonrad),
    z = circumradius * cos(latrad),
    sum_xyz = x + y + z)

points

# Change to cartesian coordinates and use (0, +-1, +-phi)?
phi <- (1 + sqrt(5)) / 2
lat_dev <- atan(0.5) / pi * 180


# The Large Heap ----
calculate_decimal_minutes <- function(decimal_degrees) {
  decimal_minutes <- round((decimal_degrees - floor(decimal_degrees)) * 60,
                           digits = 3)
}

addition_number <- 1.287262935597079127914529663121
multiplication_number <- 0.199912332675918863522787694816

x1 <- (addition_number + sqrt(addition_number^2 - 4 * multiplication_number)) * 0.5
x2 <- (addition_number - sqrt(addition_number^2 - 4 * multiplication_number)) * 0.5
y1 <- addition_number - x1
y2 <- addition_number - x2

# Must be radians - convert to latlon:

lat1 <- x1 * (180 / pi)
lon1 <- y1  * (180 / pi)

nullpunkt <- tibble::tibble(cache_name = c("The Large Heap"),
                          east = c(lon1),
                          north = c(lat1)) %>% 
  #sf::st_as_sf(coords = c("east", "north"), crs = 32632) %>%
  #sf::st_transform("+proj=longlat +datum=WGS84")
  #koordinater <- sf::st_coordinates(nullpunkt) %>% 
  #tibble::as_tibble() %>% 
  dplyr::mutate(min_e = calculate_decimal_minutes(east),
                min_n = calculate_decimal_minutes(north))

