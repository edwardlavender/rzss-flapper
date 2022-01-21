################################
################################
#### process_data_raw.R

#### This code: 
# 1) Processes the 'raw' data (physiological measurements),
# ... for modelling, 

#### Steps preceding this code:
# 1) Temperature corrections have been applied to physiological parameters, 
# ... using specific elasmobranch values where available, in the raw data. 


################################
################################
#### Set up

#### Wipe workspace and source essential packages and variables
source("./R/define_global_param.R")

#### Read data
physio <- readxl::read_excel("./data-raw/Skate data analysis shared.xlsx", 
                             sheet = "Data all without formulas")


################################
################################
#### Data processing 

#### Drop column explanations
physio <- physio[-1, ]

#### Tidy column names for initial convenience 
colnames(physio) <- 
  tolower(colnames(physio)) %>%
  stringr::str_replace_all(pattern = " ", replacement = "_")

#### Fix columns with ? 
# Some age and gaff records are uncertain
# We fix this here by assuming records are correct, but add an '_uncertain' column
# We can re-run models as necessary to check results
physio$age_uncertain <- 0
physio$age_uncertain[which(contains_qu(physio$age))]   <- 1
physio$age <- stringr::str_replace_all(physio$age, "\\?", "")
physio$gaff_uncertain <- 1
physio$gaff_uncertain[which(contains_qu(physio$gaff))] <- 1
physio$gaff <- stringr::str_replace_all(physio$gaff, "\\?", "")

#### Select columns
physio <- 
  physio %>% 
  dplyr::select(
    ## capture information 
    date = date, 
    pit = pit, 
    vemco = vemco, 
    sex = sex, 
    age = age, 
    age_uncertain = age_uncertain,
    size_disc = width, 
    size_len  = length, 
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
    pH_1 = ph1_tc_b,
    pH_2 = ph2_tc_b,
    # pCO2
    pCO2_1 = pc02_1_tc,
    pCO2_2 = pc02_2_tc,
    # pO2
    pO2_1 = p02_1_tc,
    pO2_2 = p02_2_tc,
    # BE
    be_1 = be_1, 
    be_2 = be_2, 
    # HCO3
    HCO3_1 = hc03_1,
    HCO3_2 = hc03_2,
    # SO2
    SO2_1 = s02_1,
    SO2_2 = s02_2, 
    # Lactate
    lac_1 = lac_1, 
    lac_2 = lac_2, 
    # glucose
    glu_1 = glu_1, 
    glu_2 = glu_2, 
    # K
    K_1 = k_1, 
    K_2 = k_2,
    # Ca
    Ca_1 = ca_1,
    ca_2 = ca2,
    # Mg
    Mg_1 = mg_1, 
    Mg_2 = mg_2
  ) %>%
  data.frame()

#### Fix symbols in variable values
# Some variables are recorded with '<'
physio$be_1 <- stringr::str_replace_all(physio$be_1, "<", "")
physio$be_2 <- stringr::str_replace_all(physio$be_2, "<", "")
# Check for any ? or < symbols
for(i in 1:ncol(physio)){
  # print(i)
  stopifnot(length(which(contains_qu(physio[, i]))) == 0L)
  stopifnot(length(which(contains_less_than(physio[, i]))) == 0L)
}

#### Fix variable types
physio                            <- readr::type_convert(physio)
physio$date                       <- as.Date(physio$date, origin = "1899-12-30")
physio$pit                        <- factor(physio$pit)
physio$vemco[physio$vemco == "N"] <- NA
physio$vemco                      <- factor(physio$vemco)
physio$age                        <- factor(physio$age)
physio$age_uncertain              <- factor(physio$age_uncertain)
physio$gaff                       <- factor(physio$gaff)
physio$gaff_uncertain             <- factor(physio$gaff_uncertain)
physio$healthy                    <- factor(physio$healthy)

#### Fix variable precision
physio$size_disc  <- round(physio$size_disc)
physio$size_len   <- round(physio$size_len)
physio$size_wt    <- round(physio$size_wt)


################################
################################
#### Save processed data

#### Save data
saveRDS(physio, "./data/physio.rds")

#### Write tidy table to file
physio_tbl <- tidy_numbers(physio, digits = 2)
tidy_write(physio_tbl, "./fig/physio_tbl.txt")

#### End of code. 
################################
################################
