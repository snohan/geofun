# Solving mysteries...
library(tidyverse)
library(sf)
library(geosphere)
library(nvctr)

# A Platonic Cache ----
# Must be a regular icosahedron circumscribed by a sphere, 
# thus with its 12 vertices tangent to the sphere's surface.

# Points given
# N 52° 22.412 E 173° 20.792
# N 36° 38.504 W 93° 40.426
# N 26° 27.182 E 93° 17.523
# N 7° 26.881 W 31° 22.508
# N 2° 4.095 E 32° 5.978

# Points in decimal degrees
# N and E positive
vertices_given <- tibble::tibble(
  lat = c(
    52 + 22.412 / 60,
    36 + 38.504 / 60,
    26 + 27.182 / 60,
    7 + 26.881 / 60,
    2 + 4.095 / 60
    ),
  lon = c(
    173 + 20.792 / 60,
    - (93 + 40.426 / 60),
    93 + 17.523 / 60,
    - (31 + 22.508 / 60),
    32 + 5.978 / 60
    )
  ) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    n_vector = list(
      nvctr::lat_lon2n_E(
        nvctr::rad(lat),
        nvctr::rad(lon)
      )
    )
  ) %>% 
  dplyr::ungroup() #%>% 
  # dplyr::summarise(
  #   #n_positions = n(),
  #   mean_position =
  #     nvctr::unit(n_vector)
  # )

# Alt. 1: nvctr
mean_position <-
  nvctr::unit(
    vertices_given$n_vector[[1]] +
      vertices_given$n_vector[[2]] +
      vertices_given$n_vector[[3]] +
      vertices_given$n_vector[[4]] +
      vertices_given$n_vector[[5]]
  ) %>% 
  nvctr::n_E2lat_lon() %>% 
  nvctr::deg()
# YES!

n_minutes <- (mean_position[1] - base::trunc(mean_position[1])) * 60
e_minutes <- (mean_position[2] - base::trunc(mean_position[2])) * 60

# Alt. 2: geosphere
mean_position_2 <-
  vertices_given %>% 
  dplyr::select(
    lon,
    lat
  ) %>% 
  geosphere::geomean()
# NOPE!

# vertices_opposite_given <- 
#   vertices_given %>% 
#   dplyr::mutate(
#     lat = - lat,
#     lon = 
#       dplyr::case_when(
#         lon >= 0 ~ lon - 180,
#         lon < 0 ~ lon + 180
#       )
#   ) 
# 
# # Edge length is 2
# circumradius <- sqrt(((1 + sqrt(5)) / 2)^2 + 1)
# 
# # http://csharphelper.com/blog/2015/12/platonic-solids-part-6-the-icosahedron/
# 
# vertices <-
#   dplyr::bind_rows(
#     vertices_given,
#     vertices_opposite_given
#   ) %>% 
#   dplyr::mutate(
#     vertex = paste0("v", seq(1:(2*nrow(vertices_given)))),
#     latrad = (90 - lat) / 180 * pi,
#     lonrad = 
#       dplyr::case_when(
#         lon >= 0 ~ lon / 180 * pi,
#         lon < 0 ~ (360 + lon) / 180 * pi
#       ),
#     x = circumradius * sin(latrad) * cos(lonrad),
#     y = circumradius * sin(latrad) * sin(lonrad),
#     z = circumradius * cos(latrad)
#   ) %>% 
#   dplyr::mutate(
#     dplyr::across(
#       .cols = x:z,
#       .fns = ~ round(.x, digits = 5)
#     )
#   ) %>%
#   dplyr::arrange(
#     x
#   )# %>% 
  # dplyr::rowwise() %>% 
  # dplyr::mutate(
  #   c_vector = 
  #     list(
  #       as.vector(c(x, y, z))
  #     )
  # ) %>% 
  # dplyr::ungroup()

# The two missing x values must be plus-minus 1.15344, since all other x values
# come in pairs, which must mean that there is no rotation around the z axis.
# Further, there must be a rotation around the y axis, since we don't have
# x values as 0, 1 and phi.


# Rotating to get nicer coordinates
# c <- 
#   acos(
#     vertices$x[10] / ((1 + sqrt(5)) / 2)
#   ) * (-1)
# 
# rotate_z <-
#   matrix(
#     c(cos(c), sin(c), 0,
#     -sin(c), cos(c), 0,
#     0, 0, 1),
#     nrow = 3,
#     ncol = 3
#   )
# 
# b <- 
#   acos(
#     vertices$x[10] / ((1 + sqrt(5)) / 2)
#   ) * (-1)
# 
# rotate_z <-
#   matrix(
#     c(cos(c), sin(c), 0,
#       -sin(c), cos(c), 0,
#       0, 0, 1),
#     nrow = 3,
#     ncol = 3
#   )
# 
# new_coordinates <-
#   rotate_z %*% vertices$m_vector[10][[1]]







# Choosing the first vertex and moving it to origin
x_adjustment <- 0 - vertices$x[1]
y_adjustment <- 1 - vertices$y[1]
z_adjustment <- (1 + sqrt(5)) / 2 - vertices$z[1]

vertices_translated <-
  vertices %>% 
  dplyr::mutate(
    x_trans = x + x_adjustment,
    y_trans = y + y_adjustment,
    z_trans = z + z_adjustment
  )



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

