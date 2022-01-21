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


#### End of code. 
################################
################################