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

#' @title Strip a string of whitespace
str_strip <- function(x, collapse = "_", ext = "") {
  x <- stringr::str_replace_all(x, " ", collapse) 
  if (ext != "") {
    x <- paste0(x, ".", ext)
  }
  x
}

#' @title Test if an interval overlaps 0
overlaps_zero <- function(x) {
  stopifnot(inherits(x, "matrix"))
  stopifnot(!any(is.na(x)))
  overlap <- rep(NA, nrow(x))
  for (i in seq_len(nrow(x))) {
    z <- x[i, ]
    overlap[i] <- min(z) <= 0 && max(z) >= 0
  }
  overlap
}

#' @examples
if (FALSE) {
  m <- cbind(c(-1, 1, 2, -0.1), c(1, 2, 2.1, -0.01))
  data.frame(m, overlaps_zero(m))
}

  


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

#' @title Blood parameter corrections

tc_pH <- function(m, t) {
  0.011 * (37 - t) + m 
}

tc_PO2 <- function(m, t){
  10^(-0.0058 * (37 - t)) * m
}

tc_PCO2 <- function(m, t) {
  10^(-0.019 * (37 - t)) * m
}

tc_HCO3 <- function(pH, PCO2, t) {
  
  alpha_CO2 <- 
    0.1131 - 
    1.3847 * 10^-2 * t +
    1.4995 * 10^-3 * t^2 - 
    8.8008 * 10^-5 * t^3  + 
    2.4998 * 10^-6 * t^4  - 
    2.7369 * 10^-8 * t^5 
  
  pKa <- 6.4996 + log10(t) * (0.3648 - 0.0521 * pH) - 0.0353 * pH - 0.0074 * t
  
  alpha_CO2 * PCO2 * 10^(pH - pKa)
  
}

tc_valid <- function(a, b) {
  d <- data.frame(a = a, b = b)
  print(d)
  d <- d[complete.cases(d), ]
  stopifnot(isTRUE(all.equal(d$a, d$b)))
  if (!isTRUE(all.equal(a, b))) {
    warning("NA mismatch!", call. = FALSE)
  }
  invisible(NULL)
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