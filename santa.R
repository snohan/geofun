library(tidyverse)

# Santa 1 ----
santa_1_ciphertext <- 
  readr::read_lines(
    "santa_1.txt"
  )

santa_1 <- 
  tibble::tibble(
    cipher = santa_1_ciphertext
  ) %>% 
  tidyr::separate(
    col = cipher,
    into = paste0("c", c(1:23)),
    sep = c(1:23)
  ) %>% 
  # choosing 8 columns based on the order of the given prime numbers
  dplyr::select(
    d1 = c13,
    d2 = c23,
    d3 = c7,
    d4 = c12,
    d5 = c20,
    d6 = c9,
    d7 = c17,
    d8 = c19
  ) %>% 
  # if "halfdime" is the keyword used to rearrange columns,
  # doing this backwards is using the letters' internal rank
  # halfdime -> adefhilm
  # 51742683 <- 12345678
  dplyr::select(
    d5, d1, d7, d4, d2, d6, d8, d3
  )
