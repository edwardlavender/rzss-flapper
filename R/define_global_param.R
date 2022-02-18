################################
################################
#### define_global_param.R

#### This code: 
# 1) Wipes the workspace, loads essential packages
# ... and defines parameters used by multiple scripts. 
# ... This is designed to be called at the start of every script. 

#### Steps preceding this code: 
# 1) NA 


################################
################################
#### Global set up 

#### Wipe workspace
rm(list = ls())

#### Essential packages
library(magrittr)
library(prettyGraphics)


################################
################################
#### Define global parameters 

#### Data processing functions
contains_qu        <- function(x) stringi::stri_detect_fixed(x, "?")
contains_less_than <- function(x) stringi::stri_detect_fixed(x, "<")
get_time           <- function(rates_raw, stage = "Time on hook"){
  rates_raw_is_time <- apply(rates_raw, 
                             2, 
                             function(x) stringr::str_detect(x, stage))
  anchor <- which(rates_raw_is_time, arr.ind = TRUE)
  stopifnot(nrow(anchor) >= 1L)
  anchor <- anchor[1, , drop = FALSE]
  stopifnot(nrow(anchor) == 1L)
  anchor[, 2] <- anchor[, 2] + 1
  time <- rates_raw[anchor]
  time <- chron::times(as.numeric(time))
}


#### Response variables
## Define core list of response variables 
# Exclude: BE, SO2, Ca, Mg (?)
resps <- c("pH", "PCO2", "PO2", "HCO3", "lac", "glu", "K", "Mg")
## Define pretty axis titles
ylabs <- list(pH = "pH", 
              PCO2 = expression(PCO[2] ~ "[mmHg]"), 
              PO2 = expression(PO[2] ~ "[mmHg]"), 
              HCO3 = expression(HCO[3] ~ "[mmol" * L^-1 * "]"),
              lac = expression("Lactate [mmol" * L^-1 * "]"), 
              glu = expression("Glucose [mmol" * L^-1 * "]"), 
              K = expression("K [mmol" * L^-1 * "]"), 
              Mg = expression("Mg [mmol" * L^-1 * "]")
              )
# Check titles
ylabs_check <- FALSE
if(ylabs_check){
  pp <- par(mfrow = par_mf(length(resps)))
  for(i in 1:length(resps)){
    plot(0, main = ylabs[i])
  }
  par(pp)
}
## Define legend labels
ylabs_legend <- c(expression("pH"), 
                  expression(PCO[2]), 
                  expression(PO[2]), 
                  expression(HCO[3]),
                  expression("Lactate"), 
                  expression("Glucose"), 
                  expression("K"), 
                  expression("Mg")
)

#### Define graphical parameters for error bars
ebars_lwd   <- 2
ebars_param <- list(add_fit = list(pch = 3, bg = "black", cex = 2, lwd = ebars_lwd), 
                    lwd = ebars_lwd)



#### End of code. 
################################
################################