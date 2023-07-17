#########################
#########################
#### process_vitals.R

#### Aims
# 1) Processes the vital signs (heart and respiration data)
# ... for modelling.

#### Prerequisites
# 1) Define global parameters                      (define_global_param.R)
# 2) Processing of capture events/blood parameters (process_bloods.R)


#########################
#########################
#### Set up

#### Wipe workspace
rm(list = ls()) 
try(pacman::p_unload("all"), silent = TRUE) 
dv::clear() 

#### Essential packages
library(prettyGraphics)
library(ggplot2)

#### Load data
# Load capture event data
physio <- readRDS("./data/skate/physio.rds")
# Load vitals data (below)


#########################
#########################
#### Load vitals data

#### Define connection
con <- "./data-raw/skate/Skate acoustic tagging data .xlsx"

#### Define a function to read data
# The data source is an excel file, with one sheet per individual
# The data on heart and respiratory rates is in a:
# ... 'Physiological Monitoring' section G2 -> I14 (sheets 1:57)
# ... 'Anaesthesia Information' section H2 -> J14 (sheets 58:62)
read_vitals <- function(sheets, anchor = "G2", cl = NULL) {
  out <- pbapply::pblapply(seq_along(sheets), cl = cl, function(i) {
    sheet <- sheets[i]
    rates_raw <- readxl::read_excel(
      path = con,
      sheet = sheet,
      col_types = "text",
      col_names = TRUE
    )
    rates_raw <- as.matrix(rates_raw)
    rates_raw_is_hr <- apply(
      rates_raw,
      2,
      function(x) stringr::str_detect(x, "HR")
    )
    anchor <- which(rates_raw_is_hr, arr.ind = TRUE)
    stopifnot(nrow(anchor) == 1L)
    anchor <- paste0(LETTERS[anchor[2] - 1], anchor[1] + 1)
    rates <- readxl::read_excel(
      path = con,
      sheet = sheet,
      range = readxl::anchored(anchor, dim = c(13, 3)),
      col_names = TRUE,
      col_types = "text"
    )
    # print(head(rates, 3))
    rates <- rates[, c("Time", "HR", "RR")]
    rates$pit <- as.character(rates_raw[9, 2])
    rates$date <- substr(sheet, 1, 8)
    rates$sheet_name <- sheet
    rates$sheet_index <- i
    return(rates)
  })
  return(out)
}

#### Read data
## Define a vector of sheets with data for each individual
sheets <- readxl::excel_sheets(con)
## Load data
rates_by_id <- read_vitals(sheets[1:(length(sheets) - 3)])


#########################
#########################
#### Data processing

#### Manual inspection
inspect <- FALSE
if (inspect) {
  invisible(
    lapply(rates_by_id, function(x) {
      print(x)
      readline("Press [Enter] to continue...")
    })
  )
}

#### Define dataframe across all individuals
rates <-
  do.call(rbind, rates_by_id) |>
  dplyr::select(
    sheet_index = sheet_index,
    sheet_name = sheet_name,
    pit = pit,
    date = date,
    time = Time,
    hr = HR,
    rr = RR
  )

#### Define event IDs
# Note these are defined by sheet index to match the capture fight event IDs
# ... see process_fights.R
rates$event_id <- as.integer(rates$sheet_index)

#### Fix pit names
## Fix .. in PIT names
pos <- substr(rates$pit, 1, 2) == ".."
rates$pit[pos] <- substr(rates$pit[pos], 3, nchar(rates$pit[pos]))
rates$pit[rates$pit == "982..000407092918"] <- 407092918
rates$pit[which(stringi::stri_detect_fixed(rates$pit, ".."))]
## Check for pit IDs not in physio
# The raw data show these individuals are missing from physio due to istat failures.
unique(rates$pit[!(rates$pit %in% physio$pit)])
# "31199256" "31199492" "1972215"
unique(rates$sheet_name[!(rates$pit %in% physio$pit)])
# "19032020 3" "19032020 2" "19032020 1"
data.frame(rates[!(rates$pit %in% physio$pit), ])
## Make pit factor
rates$pit <- factor(rates$pit)
table(rates$pit)

#### Fix HR/HR integers
## Heart rate
# Check class and unique values
class(rates$hr)
unique(rates$hr)
# There is a 'double?' comment for three sheets
# ... Sheets: "22082018 3", "22082018 2", "22082018 1"
# ... IDs:    "10991061",   "7093359",    "7092902"
# ... We will exclude their data for HR.
doubles_pos <- which(rates$hr %in% c("double?", "Double?"))
doubles_evt <- rates$event_id[doubles_pos]
rates$doubles <- rates$event_id %in% doubles_evt
rates$hr[rates$doubles] <- NA
rates$doubles <- NULL
# Force integer specification for HR
rates$hr <- as.integer(rates$hr)
## Respiratory rates
class(rates$rr)
unique(rates$rr)
rates$rr[rates$rr == "Not recorded"] <- NA
rates$rr[rates$rr == "-"] <- NA
unique(rates$rr)
rates$rr <- as.integer(rates$rr)

#### Fix dates/times
range(rates$time, na.rm = TRUE)
rates$hh_mm_ss <- chron::times(as.numeric(rates$time))
unique(rates$pit[is.na(rates$hh_mm_ss)])
rates <- rates[!is.na(rates$hh_mm_ss), ]
rates$yyyy_mm_dd <- lubridate::dmy(rates$date)
rates$time_stamp <- as.POSIXct(paste0(rates$yyyy_mm_dd, rates$hh_mm_ss), tz = "UTC")
range(rates$time_stamp, na.rm = TRUE)
range(Tools4ETS::hour_dbl(rates$time_stamp))

#### Define universal time index
rates <-
  rates |>
  dplyr::arrange(event_id, time_stamp) |>
  dplyr::group_by(event_id) |>
  dplyr::mutate(
    event_index = dplyr::row_number(),
    time_index =
      as.numeric(
        difftime(time_stamp, time_stamp[1], units = "mins")
      )
  ) |>
  dplyr::ungroup()
# Visual check
ggplot() +
  geom_point(aes(time_index, event_index), data = rates) +
  facet_wrap(~event_id)

#### Add predictors
## Add predictors for individuals in physio via matching
rates$key <- paste0(rates$pit, "-", as.Date(rates$time_stamp))
physio$key <- paste0(physio$pit, "-", physio$date)
match_index <- match(rates$key, physio$key)
rates$sex <- physio$sex[match_index]
rates$size_len <- physio$size_len[match_index]
rates$size_disc <- physio$size_disc[match_index]
rates$time_from_capture_to_surface <- physio$time_from_capture_to_surface[match_index]
rates$temp_water <- physio$temp_water[match_index]
rates$gaff <- physio$gaff[match_index]
rates$gaff_uncertain <- physio$gaff_uncertain[match_index]
rates$healthy <- physio$healthy[match_index]
## Add predictors for individuals not in physio manually
# There are three individuals that are missing from physio:
# ... "19032020 3" "19032020 2" "19032020 1"
# ... But we have dropped these already as they are also missing time stamps
# ... for physiological monitoring.

#### Tidy columns
rates <-
  rates |>
  dplyr::select(
    event_id,
    sheet_index,
    sheet_name,
    pit,
    sex,
    size_len,
    size_disc,
    time_from_capture_to_surface,
    temp_water,
    gaff,
    gaff_uncertain,
    healthy,
    time_stamp,
    time_index,
    hr,
    rr
  ) |>
  dplyr::arrange(time_stamp) |>
  data.frame()

#### Quality checks
table(rates$pit)
range(rates$time_stamp)
range(rates$time_index)
pretty_plot(rates$size_len, rates$size_disc)


#########################
#########################
#### Save processed data

#### Save data
saveRDS(rates, "./data/skate/rates.rds")
write.csv(rates, "./data/skate/rates.csv", row.names = FALSE)

#### Write tidy table to file
rates_tbl <- tidy_numbers(rates, digits = 2)
tidy_write(rates_tbl, "./fig/rates_tbl.txt")


#### End of code.
#########################
#########################
