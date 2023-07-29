###########################
###########################
#### define_helpers.R

#### Aims
# 1) Define helper functions

#### Prerequisites
# 1) NA


###########################
###########################
#### Utils

#' @title Open a file
.open <- function(file) system(sprintf("open %s", shQuote(file)))
open <- function(file) invisible(sapply(file, .open))


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


###########################
###########################
#### Plotting

#' @title Pretty pairs plots
pretty_pairs <- function(data) {
  psych::pairs.panels(data, 
                      method = "spearman", lwd = 2,
                      ellipse = FALSE, 
                      hist.col = scales::alpha("lightgrey", 0.75), rug = FALSE,
                      cex.cor = 2, cex.axis = 1.6,
                      scale = TRUE, gap = 0.25,
                      las = TRUE, cex.labels = 0.8, font.labels = 2)
}


#### End of code.
###########################
###########################