#########################
#########################
#### process_bloods.R

#### Aims
# 1) Processes the raw physiological blood parameters
# ... for modelling.

#### Prerequisites
# 1) Define global parameters (define_global_param.R)
# 2) Temperature corrections have been applied to physiological parameters,
# ... using specific elasmobranch values where available, in the raw data.


#########################
#########################
#### Set up

#### Wipe workspace
rm(list = ls()) 
try(pacman::p_unload("all"), silent = TRUE) 
dv::clear() 

#### Essential packages
library(dv)
library(prettyGraphics)

#### Load data
source(here_r("001_define_global_param.R"))
source(here_r("002_define_helpers.R"))
physio <- readxl::read_excel("./data-raw/skate/Skate data analysis shared.xlsx",
  sheet = "Data all without formulas"
)


#########################
#########################
#### Data processing

#### Drop column explanations
physio <- physio[-1, ]

#### Tidy column names for initial convenience
colnames(physio) <-
  tolower(colnames(physio)) |>
  stringr::str_replace_all(pattern = " ", replacement = "_")

#### Fix columns with ?
# Some age and gaff records are uncertain
# We fix this here by assuming records are correct, but add an '_uncertain' column
# We can re-run models as necessary to check results
physio$age_uncertain <- 0
physio$age_uncertain[which(contains_qu(physio$age))] <- 1
physio$age <- stringr::str_replace_all(physio$age, "\\?", "")
physio$gaff_uncertain <- 1
physio$gaff_uncertain[which(contains_qu(physio$gaff))] <- 1
physio$gaff <- stringr::str_replace_all(physio$gaff, "\\?", "")

#### Select columns
# (The list of response variables is defined in define_global_param.R)
sort(colnames(physio))
physio <-
  physio |>
  dplyr::select(
    ## capture information
    date = date,
    pit = pit,
    vemco = vemco,
    surgery = surgery,
    sex = sex,
    age = age,
    age_uncertain = age_uncertain,
    size_disc = width,
    size_len = length,
    size_wt = weight,
    ## capture duration
    time_from_capture_to_surface = f_time,
    time_from_surface_to_bs1 = wait_time,
    time_from_bs1_to_bs2 = `bs1-bs2`,
    ## explanatory variables
    temp_water = w_temp_d,
    gaff = gaff,
    gaff_uncertain = gaff_uncertain,
    healthy = healthy,
    ## blood responses (with elasmobranch-specific temperature adjustments ) for BS1 and BS2
    # pH
    pH_1_raw = ph1,
    pH_2_raw = ph_2,
    pH_1 = ph1_tc_b,
    pH_2 = ph2_tc_b,
    # PCO2
    PCO2_1_raw = pc02_1,
    PCO2_2_raw = pc02_2,
    PCO2_1 = pc02_1_tc,
    PCO2_2 = pc02_2_tc,
    # PO2
    PO2_1_raw = p02_1,
    PO2_2_raw = p02_2,
    PO2_1 = p02_1_tc,
    PO2_2 = p02_2_tc,
    # BE
    # be_1 = be_1,
    # be_2 = be_2,
    # HCO3
    HCO3_1 = hco3_1_tc_b, 
    HCO3_2 = hco3_2_tc_b, 
    # SO2
    # SO2_1 = s02_1,
    # SO2_2 = s02_2,
    # Lactate
    lac_1 = lac_1,
    lac_2 = lac_2,
    # glucose
    glu_1 = glu_1,
    glu_2 = glu_2,
    # K
    K_1 = k_1,
    K_2 = k_2,
    K_haem = sample_quality_k2,
    # Ca
    # Ca_1 = ca_1,
    # Ca_2 = ca2,
    # Mg
    Mg_1 = mg_1,
    Mg_2 = mg_2
  ) |>
  data.frame()

#### Fix symbols in variable values
## Some variables are recorded with '<'
# physio$be_1 <- stringr::str_replace_all(physio$be_1, "<", NA_character_)
# physio$be_2 <- stringr::str_replace_all(physio$be_2, "<", NA_character_)
physio$PCO2_2_raw[physio$PCO2_2_raw == "<5"] <- NA_character_
## Check for any ? or < symbols
for (i in 1:ncol(physio)) {
  # print(i)
  stopifnot(length(which(contains_qu(physio[, i]))) == 0L)
  stopifnot(length(which(contains_less_than(physio[, i]))) == 0L)
}

#### Fix variable types
physio <- readr::type_convert(physio)
physio$date <- as.Date(physio$date, origin = "1899-12-30")
physio$pit <- factor(physio$pit)
physio$vemco[physio$vemco == "N"] <- NA
physio$vemco <- factor(physio$vemco)
physio$surgery <- factor(physio$surgery)
physio$surgery <- plyr::mapvalues(physio$surgery, from = c("0", "1"), to = c("N", "Y"))
physio$age <- factor(physio$age)
physio$age_uncertain <- factor(physio$age_uncertain)
physio$sex <- factor(physio$sex)
physio$gaff <- factor(physio$gaff)
physio$gaff_uncertain <- factor(physio$gaff_uncertain)
physio$healthy <- factor(physio$healthy)
physio$K_haem[is.na(physio$K_haem)] <- 0
physio$K_haem[physio$K_haem == "H"] <- 1
physio$K_haem <- factor(physio$K_haem)

#### Fix variable precision
physio$size_disc <- round(physio$size_disc)
physio$size_len <- round(physio$size_len)
physio$size_wt <- round(physio$size_wt)

#### Define additional columns
physio$time_from_capture_to_bs1 <-
  physio$time_from_capture_to_surface + physio$time_from_surface_to_bs1
physio$time_from_surface_to_bs2 <-
  physio$time_from_surface_to_bs1 + physio$time_from_bs1_to_bs2
physio$time_from_capture_to_bs2 <-
  physio$time_from_capture_to_surface + physio$time_from_surface_to_bs1 + physio$time_from_bs1_to_bs2


#########################
#########################
#### Quality checks

#### Check for repeated captures
# 59 captures in total
nrow(physio)
# one individual was caught twice (pit: 29241467)
table(table(physio$pit))

#### For each response variable, check for outliers
# For each response, check a boxplot of the values for BS1 and BS2 to identify outliers
# ... see analyse_bloods_change. R

#### Fix problematic values
## Skate 7093028 has a problematic PH1 value (extreme alkalinosis)
# Other related parameters for this individual (HCO3_1 are NA)
# lac_1 looks reasonable
physio$pH_1[physio$pit == 7093028] <- NA
physio$HCO3_1[physio$pit == 7093028]
## Skate 10991061 has a problematic (raw) PCO2_2 value which
# ... is less than the reading value of the machine
# ... This has already been set to NA in the raw data.
physio$PCO2_2[physio$pit == "10991061"]

#### Validate blood parameter calculations
# pH
tc_valid(physio$pH_1, tc_pH(physio$pH_1_raw, physio$temp_water)) 
tc_valid(physio$pH_2, tc_pH(physio$pH_2_raw, physio$temp_water)) 
# PO2
tc_valid(physio$PO2_1, tc_PO2(physio$PO2_1_raw, physio$temp_water)) 
tc_valid(physio$PO2_2, tc_PO2(physio$PO2_2_raw, physio$temp_water)) 
# PCO2
tc_valid(physio$PCO2_1, tc_PCO2(physio$PCO2_1_raw, physio$temp_water)) 
tc_valid(physio$PCO2_2, tc_PCO2(physio$PCO2_2_raw, physio$temp_water)) 
# HCO3
tc_valid(physio$HCO3_1, tc_HCO3(physio$pH_1, physio$PCO2_1, physio$temp_water)) 
tc_valid(physio$HCO3_2, tc_HCO3(physio$pH_2, physio$PCO2_2, physio$temp_water)) 
# Drop raw columns
raw_cols <- colnames(physio)[stringr::str_detect(colnames(physio), "raw")]
for (col in raw_cols) {
  physio[, col] <- NULL
}
                             
#### Define difference between blood parameters
# ... The 'difference' variable is labelled using sample = '3'
# ... for consistency with BS1 and BS2, so we can use the same code
for (r in resps) {
  r1 <- paste0(r, "_1")
  r2 <- paste0(r, "_2")
  r3 <- paste0(r, "_3")
  physio[, r3] <- physio[, r2] - physio[, r1]
}

#### Check the number of observations for each response
sapply(resps, function(resp) length(which(!is.na(physio[, paste0(resp, "_1")]))))
sapply(resps, function(resp) length(which(!is.na(physio[, paste0(resp, "_2")]))))
sapply(resps, function(resp) length(which(!is.na(physio[, paste0(resp, "_3")]))))


#########################
#########################
#### Save processed data

#### Save data
saveRDS(physio, "./data/skate/physio.rds")

#### Write tidy table to file
physio_tbl <- tidy_numbers(physio, digits = 2)
tidy_write(physio_tbl, "./fig/physio_tbl.txt")


#### End of code.
#########################
#########################