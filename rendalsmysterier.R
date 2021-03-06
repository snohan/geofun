# Omæsjline tal
library(tidyverse)

ns <- c(
  "n1",
  "n2",
  "n3",
  "n4",
  "n5",
  "e1",
  "e2",
  "e3",
  "e4",
  "e5")

numbers <- c(
  "11131221163113112221151113122112111312211811131221101113122117",
  "1321161113122115132112132118132110132117",
  "??????????????????????????",
  "????????????????????????",
  "??????????????",
  "11131221101211132221101113122115111312211011131221121113122113",
  "13211021322110132115132110132112132113",
  "????????????????????????????",
  "??????????????????????????",
  "??????????????")

n_all <- tibble::tibble(ns = ns, numbers = numbers) %>% 
  dplyr::mutate(no_char = stringr::str_length(numbers))

