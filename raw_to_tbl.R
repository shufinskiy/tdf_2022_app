### Преобразование сырых текстовых файлов в табличные данные
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)

### Работа с данными: заполнение пропусков, перевод времени в секунды
hms_or_ms <- function(x){
  x[which(stringr::str_detect(x, "\\d+:\\d+:\\d+"))] <- stringr::str_pad(x, width = 8, side = "left", pad="0")[which(stringr::str_detect(x, "\\d+:\\d+:\\d+"))]
  time <- unlist(lapply(x, function(t) {as.numeric(suppressWarnings(tryCatch(lubridate::hms(t), warning = function(e) lubridate::ms(t))))}))
  return(time)
}

transform_time <- function(file, number_stage){
  f <- readLines(file)
  position <- as.integer(stringr::str_extract(f, "\\d{1,3}"))
  rider <- stringr::str_trim(stringr::str_extract(f, "[A-Z].+(?=\\(\\w+\\))"))
  rider_country <- toupper(stringr::str_extract(f, "(?<=\\()\\w+(?=\\))"))
  time <- stringr::str_extract(f, "(\\d+:\\d+:\\d+)|(\\d+:\\d+)")
  
  df <- dplyr::tibble(POS = position, RIDER = rider, COUNTRY = rider_country, TIME = time)

  df <- df %>% 
    tidyr::fill(TIME) %>% 
    dplyr::mutate(TIME = as.numeric(hms_or_ms(TIME)))
  
  leader_time <- df$TIME[1]
  
  df <- df %>% 
    dplyr::mutate(TIME = dplyr::if_else(TIME == leader_time, TIME, leader_time + TIME),
                  STAGE = paste0("stage", number_stage))
  return(df)
}
list_general <- list.files("./raw_data", pattern = "general.txt$", recursive = TRUE, full.names = TRUE)
test_general <- transform_time(list_general[15], 3)

list_team <- list.files("./raw_data", pattern = "team.txt$", recursive = TRUE, full.names = TRUE)
test_team <- transform_time(list_team[15], 3)

list_white <- list.files("./raw_data", pattern = "white.txt", recursive = TRUE, full.names = TRUE)
test_white <- transform_time(list_white[15], 3)

transform_points <- function(file, number_stage){
  f <- readLines(file)
  position <- as.integer(stringr::str_extract(f, "\\d{1,3}"))
  rider <- stringr::str_trim(stringr::str_extract(f, "[A-Z].+(?=\\(\\w+\\))"))
  rider_country <- toupper(stringr::str_extract(f, "(?<=\\()\\w+(?=\\))"))
  points <- as.numeric(stringr::str_extract(f, "\\d+(?=\t\\s*$)"))
  
  df <- dplyr::tibble(POS = position, RIDER = rider, COUNTRY = rider_country, POINTS = points) %>% 
    dplyr::mutate(STAGE = paste0("stage", number_stage))
  return(df)
}

list_green <- list.files("./raw_data", pattern = "green.txt$", recursive = TRUE, full.names = TRUE)
test_green <- transform_points(list_green[15], 3)

list_peas <- list.files("./raw_data", pattern = "peas.txt$", recursive = TRUE, full.names = TRUE)
test_peas <- transform_points(list_peas[14], 3)