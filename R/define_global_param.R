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



#### End of code. 
################################
################################