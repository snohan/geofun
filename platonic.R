# A Platonic Cache
library(tidyverse)
#library(sf)

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
