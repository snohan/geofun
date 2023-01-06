# Solving mysteries...
{
  library(tidyverse)
  library(sf)
  library(geosphere)
  library(nvctr)
  library(leaflet)
  library(pracma)
}

calculate_initial_direction_of_path <- function(n_vector, bearing) {
  
  # Bearing in radians
  
  k_east = 
    pracma::cross(
      c(0, 0, 1), # coordinate frame along Earth's rotational axis
      n_vector
    ) |> 
    nvctr::unit()
  
  k_north = pracma::cross(
    n_vector,
    k_east
  )
  
  inital_direction_of_path <- 
    k_north * cos(bearing) + k_east * sin(bearing)
  
}


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



# The Large Heap ----
calculate_decimal_minutes <- function(decimal_degrees) {
  
  decimal_minutes <- 
    base::round(
      (decimal_degrees - base::floor(decimal_degrees)) * 60,
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


# Bartejul20-12 - Halvveis V ----
# 4.9 km fra Krokstien 63 23.617 10 24.838
# 6.3 km fra Statens hus 25.716 23.584
# 1.9 km fra 22.145 20.449

measured_points <-
  tibble::tibble(
    lat = c(
      63 + 23.617 / 60, 
      63 + 25.716 / 60,
      63 + 22.145 / 60  
    ),
    lon = c(
      10 + 24.838 / 60,
      10 + 23.584 / 60,
      10 + 20.449 / 60
    ),
    distance = c(
      4.9e3, 
      6.3e3, 
      1.9e3
    )
  ) %>% 
  # dplyr::rowwise() %>% 
  # dplyr::mutate(
  #   n_vector = list(
  #     nvctr::lat_lon2n_E(
  #       nvctr::rad(lat),
  #       nvctr::rad(lon)
  #     )
  #   )
  # ) %>% 
  # dplyr::ungroup() %>% 
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  ) %>% 
  sf::st_transform(32632) 

circles <-
  measured_points %>% 
  sf::st_buffer(
    dist = measured_points$distance
  )

map_circles <- 
  circles %>% 
  sf::st_transform(4326) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolylines(
    weight = 2
  )
# Nøyaktig samme resultat som med verktøyet i G Toolbox
# Midt på vegen
# Antar mellom vegen og elva. YESS!


# GC9NJMH Har du peiling? ----

# 1. Triangulate from two measured points
# 2. Plot in map

measured_points <-
  tibble::tibble(
    lat = c(
      63.34793,
      63.34790
    ),
    lon = c(
      10.36920,
      10.38315
    ),
    bearing = c(
      186,
      203
    )
  ) |> 
  dplyr::mutate(
    lat_rad = nvctr::rad(lat),
    lon_rad = nvctr::rad(lon),
    azimuth = nvctr::rad(bearing)
  ) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    n_vector =
      list(
        nvctr::lat_lon2n_E(
          latitude = lat_rad,
          longitude = lon_rad
        )
      ),
    initial_direction_of_path =
      list(
        calculate_initial_direction_of_path(n_vector, azimuth)
      ),
    great_circle_n_vector =
      list(
        pracma::cross(
          initial_direction_of_path,
          n_vector
        )
      )
  )

gz <-
  pracma::cross(
    measured_points$great_circle_n_vector[[2]],
    measured_points$great_circle_n_vector[[1]]
  ) |> 
  nvctr::unit() |> 
  nvctr::n_E2lat_lon() |> 
  nvctr::deg()

# Ser riktig ut: elskap ved bussholdeplass.

# BP#25 The lost and found cache 2 ----
# Tre punkter i en trekant
# A er den vi skal finne, Lost 2
# B er Lost 1
# C er En liten perle

caches <-
  tibble::tibble(
    name = c(
      "Lost 1",
      "Perle"
    ),
    lat = c(
      63 + 23.098 / 60,
      63 + 23.011 / 60  
    ),
    lon = c(
      10 + 17.041 / 60,
      10 + 17.618 / 60
    ),
    alt = c(
      0,
      0
    )
  ) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(
    # n_vector = list(
    #   nvctr::lat_lon2n_E(
    #     nvctr::rad(lat),
    #     nvctr::rad(lon)
    #   )
    # ),
    # Creating North-East-Up vectors
    neu_vector = list(
      c(lat, lon, alt)
    )
  ) %>%
  dplyr::ungroup() %>%
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

# Finner retning og avstand fra Lost 1 til Perle:
from_L1_to_Perle <-
  nvctr::altitude_azimuth_distance(
    caches$neu_vector[[1]],
    caches$neu_vector[[2]]
  )

# Vinkelen mellom AB og BC
angle_AB_BC <-
  # Oppgitt retning B-A - 180
  # +
  # 180 - retningen B-C
  (261 - 180) + (180 - from_L1_to_Perle[2])

# Oppgitt i oppgaven
given_d <- 471.471

# Bruker cosinusloven
AB = (from_L1_to_Perle[3]^2 - given_d^2) / (2 * given_d + 2 * from_L1_to_Perle[3] * cos(angle_AB_BC * pi / 180))

# Finding A
gz <- 
  geosphere::destPoint(
    p = c(caches$lon[1], caches$lat[1]),
    b = 261,
    d = AB
  )

gz_df <-
  tibble::tibble(
    name = c(
      "Lost 2"
    ),
    lat = c(
      gz[2]
    ),
    lon = c(
      gz[1]
    )
  )


# TODO: calculate A with n-vectors

all_caches <-
  caches |> 
  sf::st_drop_geometry() |> 
  dplyr::select(
    name,
    lat,
    lon
  ) |> 
  dplyr::bind_rows(
    gz_df
  ) |> 
  dplyr::mutate(
    lat_min = (lat - base::trunc(lat)) * 60,
    lon_min = (lon - base::trunc(lon)) * 60
  ) |> 
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

map <- 
  all_caches |> 
  leaflet::leaflet() |> 
  leaflet::addTiles() |> 
  leaflet::addMarkers()
