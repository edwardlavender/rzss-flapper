###########################
###########################
#### define_helpers.R

#### Aims
# 1) Define helper functions

#### Prerequisites
# 1) NA


###########################
###########################
#### Data processing 

#' @title Identify strings that contain symbols
contains_qu <- function(x) stringi::stri_detect_fixed(x, "?")
contains_less_than <- function(x) stringi::stri_detect_fixed(x, "<")

#' @title Get capture stages from raw data (e.g., time on hook)
get_time <- function(rates_raw, stage = "Time on hook") {
  rates_raw_is_time <- apply(
    rates_raw,
    2,
    function(x) stringr::str_detect(x, stage)
  )
  anchor <- which(rates_raw_is_time, arr.ind = TRUE)
  stopifnot(nrow(anchor) >= 1L)
  anchor <- anchor[1, , drop = FALSE]
  stopifnot(nrow(anchor) == 1L)
  anchor[, 2] <- anchor[, 2] + 1
  time <- rates_raw[anchor]
  chron::times(as.numeric(time))
}


#### End of code.
###########################
###########################
