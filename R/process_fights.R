################################
################################
#### process_fights.R

#### This code: 
# 1) Processes fight time data (locations, times and other variables)
# ... for modelling. 

#### Steps preceding this code:
# 1) Define global parameters (define_global_param.R)
# 2) Process vital signs data (process_vitals.R)
# 3) Process spatial data     (process_spatial.R)


################################
################################
#### Set up

#### Wipe workspace and source essential packages and variables
source("./R/define_global_param.R")

#### Essential packages
library(lubridate)

#### Load data
# processed rates/physio data as a source of capture IDs, dates etc.
rates  <- readRDS("./data/skate/rates.rds")
physio <- readRDS("./data/skate/physio.rds")
# coast data for checking location accuracy
coast <- readRDS("./data/spatial/coast/coast.rds")
# WeStCOMS mesh for deriving current speeds
mesh  <- readRDS("./data/spatial/mesh/mesh_around_elements.rds")
# bathymetry data for capture depths
digi <- raster::raster("./data/spatial/bathy/digi.tif")

#### Define local parameters
con   <- "./data-raw/skate/Skate acoustic tagging data .xlsx"
wgs84 <- sp::CRS(SRS_string = "EPSG:4326")


################################
################################
#### Extract raw capture records (location, time)

################################
#### Define skeleton dataframe 

#### Define skeleton capture fights dataframe
run <- FALSE
if(run){
  
  #### Define a list of capture records containing capture locations and time 
  sheets <- readxl::excel_sheets(con)
  sheets <- sheets[1:(length(sheets)- 3)]
  xy_raw_by_sheet <- pbapply::pblapply(seq_along(sheets), function(i){
    
    ## Load sheet for capture event 
    print(i)
    sheet <- sheets[i]
    rates_raw <- readxl::read_excel(path = con, 
                                    sheet = sheet, 
                                    col_types = "text", 
                                    col_names = TRUE)
    rates_raw <- as.matrix(rates_raw)
    
    ## Get IDs
    # Extract raw PIT code
    pit   <- as.character(rates_raw[9, 2])
    # Fix code errors
    if(substr(pit, 1, 2) == "..") pit <- substr(pit, 3, nchar(pit))
    if(pit == "982..000407092918") pit <- "407092918"
      
    ## Get locations 
    rates_raw_is_loc <- apply(rates_raw, 
                              2, 
                              function(x) stringr::str_detect(x, "Location"))
    anchor <- which(rates_raw_is_loc, arr.ind = TRUE)
    stopifnot(nrow(anchor) == 1L)
    anchor[, 2] <- anchor[, 2] + 1
    xy <- rates_raw[anchor]
    
    ## Get date
    date  <- substr(sheet, 1, 8) 
    date  <- lubridate::dmy(date)
    
    ## Get times
    # We will get time at surface/release
    # There are issues with the other times recorded in the raw data
    # time on hook
    time         <- get_time(rates_raw, "Time on hook")
    # time at surface
    time_surface <- get_time(rates_raw, "Time at boat side")
    # time on deck
    time_deck    <- get_time(rates_raw, "Time on deck")
    # time of BS1
    time_bs1     <- get_time(rates_raw, "Time blood sample 1")
    # time of BS2
    time_bs2     <- get_time(rates_raw, "Time blood sample 2")
    # release time 
    time_release <- get_time(rates_raw, "Time back in water")
    
    ## Define timestamps 
    if(!is.na(time)){
      time_stamp   <- as.POSIXct(paste0(date, time), tz = "UTC")
    } else {
      time_stamp <- NA
    }
    if(!is.na(time_surface)){
      time_surface   <- as.POSIXct(paste0(date, time_surface), tz = "UTC")
    } else {
      time_surface <- NA
    }
    if(!is.na(time_deck)){
      time_deck   <- as.POSIXct(paste0(date, time_deck), tz = "UTC")
    } else {
      time_deck <- NA
    }
    if(!is.na(time_bs1)){
      time_bs1   <- as.POSIXct(paste0(date, time_bs1), tz = "UTC")
    } else {
      time_bs1 <- NA
    }
    if(!is.na(time_bs2)){
      time_bs2   <- as.POSIXct(paste0(date, time_bs2), tz = "UTC")
    } else {
      time_bs2 <- NA
    }
    if(!is.na(time_release)){
      time_release <- as.POSIXct(paste0(date, time_release), tz = "UTC")
    } else {
      time_release <- NA
    }

    ## Return extracted information
    out <- data.frame(sheet_name   = sheet, 
                      sheet_index  = i,
                      pit          = pit,
                      time_stamp   = time_stamp, 
                      time_hook    = time_stamp,
                      time_surface = time_surface,
                      time_deck    = time_deck,
                      # time_bs1     = time_bs1, 
                      # time_bs2     = time_bs2,
                      time_release = time_release,
                      xy_raw       = xy)
    return(out)
  })
  
  #### Add covariates 
  fights <- do.call(rbind, xy_raw_by_sheet)
  match_index        <- match(fights$sheet_name, rates$sheet_name)
  fights$sex         <- rates$sex[match_index]
  fights$size_len    <- rates$size_len[match_index]
  fights$size_disc   <- rates$size_disc[match_index]
  fights$time_fight  <- rates$time_from_capture_to_surface[match_index]
  fights$temp_water  <- rates$temp_water[match_index]
  fights$healthy     <- rates$healthy[match_index]
  unique(fights$pit)
  
  #### Write raw captures to file for manual processing
  # Covariates need to be added manually for 
  # ... the first six individuals dropped from rates (due to missing vital signs)
  # Locations need to be corrected manually 
  # ... These have been recorded in different styles
  # ... and need to be converted to decimal degrees
  write.csv(fights, "./data-raw/skate/captures.csv",
            row.names = FALSE, na = "")
}


################################
#### Examine skeleton dataframe 

#### Read skeleton captures dataframe
fights <- readxl::read_excel("./data-raw/skate/captures.xlsx", sheet = "data")
fights$sheet_index <- as.integer(fights$sheet_index)
fights$event_id    <- fights$sheet_index
# fights$event_id    <- 1:nrow(fights)
# identical(fights$event_id, fights$sheet_index) # TRUE
table(is.na(fights$time_fight))
utils.add::basic_stats(fights$time_fight)

#### Process PIT tag IDs
## Compare pit IDs in fights versus 'physio' by matching the last four digits
fights$pit   <- as.character(fights$pit)
physio$pit   <- as.character(physio$pit)
fights$pit_4 <- utils.add::substr_end(fights$pit, 4)
physio$pit_4 <- utils.add::substr_end(physio$pit, 4)
physio$pit_f     <- as.character(fights$pit[pmatch(physio$pit_4, fights$pit_4)])
physio$identical <- physio$pit == physio$pit_f
physio$identical_2 <- identical(utils.add::substr_end(physio$pit, nchar(physio$pit_f)), 
                                physio$pit_f)
## Manual checking
# ... Non matches are due to the absence of a leading '0' or '00' in fights
# ... These result not from differences between the raw data 
# ... (Skate acoustic tagging data .xlsx) and the processed data 
# ... (Skate acoustic tagging data shared.xlsx > Data without all formulas)
# ... but from leading zeros being dropped in Excel when the csv file was written above. 
# ... We will fix this post-hoc below. 
# View(physio[!physio$identical, c("pit", "pit_f", "identical", "identical_2")])
## Add leading zeros in fights where necessary by simply updating pit tags in 
# ... fights to numbers in physio, where available 
fights$pit_2 <- physio$pit[match(fights$pit, physio$pit_f)]
# View(fights[, c("pit", "pit_2")])
fights$pit_2[is.na(fights$pit_2)] <- fights$pit[is.na(fights$pit_2)]
## Rename columns
fights$pit_raw <- fights$pit    # raw pit IDs
fights$pit     <- fights$pit_2  # full pit IDs
fights$pit_2   <- NULL

#### Define column classes
str(fights)
fights$sex <- factor(fights$sex)
fights$pit <- factor(fights$pit)

#### Save full set of capture event IDs
fights$date <- as.Date(fights$time_stamp)
fights$key  <- paste0(fights$pit, "-", fights$date)
saveRDS(fights, 
        "./data/skate/capture_events.rds")

#### Summary statistics
## Number of captures
nrow(fights)
## Number of individuals 
length(levels(fights$pit))
# One individual (29241467) was caught twice: 
names(which(table(fights$pit) == 2))
## Captures occurred from "2018-08-01" to "2020-03-20"
# Capture dates 
range(as.Date(fights$time_stamp))
# The time of day of recorded events is sensible
range(Tools4ETS::hour_dbl(fights$time_hook))
range(Tools4ETS::hour_dbl(fights$time_surface))
range(Tools4ETS::hour_dbl(fights$time_deck))
range(Tools4ETS::hour_dbl(fights$time_release), na.rm = TRUE)
range(difftime(fights$time_release, fights$time_deck, units = "mins"), na.rm = TRUE)
range(difftime(fights$time_deck, fights$time_surface, units = "mins"))
range(difftime(fights$time_surface, fights$time_hook, units = "mins"))
## Sexes and body sizes 
table(fights$sex)
# F  M 
# 31 31 
pretty_plot(fights$size_len, fights$size_disc, 
            xlab = "Total length [cm]", ylab = "Disc width [cm]")
mod_1 <- lm(size_disc ~ size_len, data = fights)
pp <- par(mfrow = c(1, 2))
plot(mod_1, which = 1:2)
par(pp)
utils.add::basic_stats(fights$size_len, na.rm = TRUE)
# min   mean median max    sd IQR   MAD
# 112 177.57    183 229 34.19  51 41.51
tapply(fights$size_len, factor(fights$sex), utils.add::basic_stats, na.rm = TRUE)
## Handling time in relation to temperature
fights$time_handling <- as.integer(difftime(fights$time_release, fights$time_stamp))
pretty_plot(fights$temp_water,fights$time_handling)
mod_1 <- glm(time_handling ~ temp_water, 
             family = gaussian(link = "log"), 
             data = fights)
pretty_predictions_1d(mod_1, x_var = "temp_water")
list_CIs(predict(mod_1, 
                 newdata = data.frame(temp_water = c(min(fights$temp_water), max(fights$temp_water))), 
                 se.fit = TRUE,
                 type = "response"))
summary(mod_1)
pp <- par(mfrow = c(1, 2))
plot(mod_1, which = 1:2)
par(pp)

## Proportion of 'healthy' versus 'unhealthy' individuals
pr_healthy <- table(fights$healthy)
pr_healthy
# 0  1 
# 6 56 
# ~ 10 (9.677) % of individuals were classed as 'unhealthy'
pr_healthy[1]/sum(pr_healthy) * 100 
# 95 % CIS range from 2 % to 17 %:
utils.add::est_prop(pr_healthy[1], pr_healthy[2])
# 0.097 [0.022,0.172]

#### Check capture locations 
# One location appears to be erroneous
pretty_map(add_polys = list(x = coast))
text(x = fights$lon, y = fights$lat, 
     labels = fights$sheet_name, col = "red", cex = 0.5)
fights$lon[fights$sheet_name == "22082018 5"] <- NA

#### Define capture locations in BNG 
bng <- "+init=epsg:27700"
pos <- !is.na(fights$lon) & !is.na(fights$lat)
xy  <- sp::SpatialPointsDataFrame(fights[pos, c("lon", "lat")], 
                                  data = fights[pos, ],
                                  proj4string = wgs84)
xy <- sp::spTransform(xy, bng)
xy <- sp::coordinates(xy)
fights$easting  <- NA
fights$northing <- NA
fights$easting[pos]  <- xy[, 1]
fights$northing[pos] <- xy[, 2]

#### Add surgery and blood parameter values for each event 
fights$key <- paste0(fights$pit, as.Date(fights$time_stamp))
physio$key <- paste0(physio$pit, physio$date)
resps_1 <- paste0(resps, "_1")
resps_2 <- paste0(resps, "_2")
for(i in 1:length(resps)){
  fights[, resps_1[i]] <- physio[, resps_1[i]][match(fights$key, physio$key)]
  fights[, resps_2[i]] <- physio[, resps_2[i]][match(fights$key, physio$key)]
}
fights$K_haem <- physio$K_haem[match(fights$key, physio$key)]
table(fights$K_haem)
which(fights$K_haem == 1)
fights$tag <- physio$surgery[match(fights$key, physio$key)]
fights$tag[is.na(fights$tag)] <- 0
fights$tag <- factor(fights$tag)
table(fights$tag)

#### Add the average heart/respiratory rate for each event 
fights$hr <- NA
fights$rr <- NA
rates$event_id_2 <- fights$event_id[match(rates$sheet_name, fights$sheet_name)]
identical(rates$event_id, rates$event_id_2) # TRUE
for(i in 1:nrow(fights)){
  if(fights$event_id[i] %in% rates$event_id){
    fights$event_id[i] %in% rates$event_id
    rates_for_event <- rates[rates$event_id == fights$event_id[i], ]
    fights$hr[i] <- median(rates_for_event$hr, na.rm = TRUE)
    fights$rr[i] <- median(rates_for_event$rr, na.rm = TRUE)
  }
}

#### Tidy table
## Fix DP
fights_tbl <- data.frame(fights)
dp_1 <- function(x) add_lagging_point_zero(plyr::round_any(x, 0.1), n = 1)
dp_2 <- function(x) add_lagging_point_zero(plyr::round_any(x, 0.01), n = 2)
for(i in 1:length(resps)){
  fights_tbl[, resps_1[i]] <- dp_2(fights_tbl[, resps_1[i]])
  fights_tbl[, resps_2[i]] <- dp_2(fights_tbl[, resps_2[i]])
}
fights_tbl <- 
  fights_tbl %>%
  dplyr::mutate(date = format(as.Date(time_stamp), "%y-%m-%d"), 
                easting = dp_1(easting), 
                northing = dp_1(northing)
                ) 
## Define table with individual characteristics 
fights_tbl_1 <- 
fights_tbl %>%
  dplyr::select(ID         = event_id, 
                Date       = date, 
                Easting    = easting, 
                Northing   = northing,
                PIT        = pit,
                Sex        = sex, 
                TL         = size_len, 
                DW         = size_disc, 
                Tag        = tag
  )
## Define blood parameters 
fights_tbl_2 <- 
  fights_tbl %>%
  dplyr::select(
    ID         = event_id,
    `pH (1)`   = pH_1,
    `PCO2 (1)` = PCO2_1, 
    `PO2 (1)`  = PO2_1, 
    `HCO3 (1)` = HCO3_1, 
    `Lac (1)`  = lac_1, 
    `Glu (1)`  = glu_1, 
    `K (1)`    = K_1, 
    `Mg (1)`   = Mg_1, 
    `pH (2)`   = pH_2,
    `PCO2 (2)` = PCO2_2, 
    `PO2 (2)`  = PO2_2, 
    `HCO3 (2)` = HCO3_2, 
    `Lac (2)`  = lac_2, 
    `Glu (2)`  = glu_2, 
    `K (2)`    = K_2, 
    `Mg (2)`   = Mg_2, 
    HR         = hr, 
    RR         = rr)
tidy_write(fights_tbl_1, "./fig/fights_tbl_1.txt")
tidy_write(fights_tbl_2, "./fig/fights_tbl_2.txt")

#### Select columns
fights$xy_raw <- NULL
fights$notes  <- NULL
fights        <- fights[fights$healthy == 1, ]
fights        <- fights[complete.cases(fights[, c("time_fight", "healthy", "lon", "lat")]), ]
nrow(fights)
# 43

# Recheck locations
pretty_map(coast, 
           add_polys = list(x = coast), 
           add_points = list(x = fights$lon, y = fights$lat, col = "red"))

#### Save locations .csv for mapping in QGIS
write.csv(fights[, c("lon", "lat")], 
          "./data/skate/rzss_capture_fight_locations.csv",
          quote = FALSE,
          row.names = FALSE)

#### Add depths 
fights$depth <- abs(raster::extract(digi, fights[, c("lon", "lat")]))
table(!is.na(fights$depth)) 
# TRUE

#### Add sun angles
data_for_suncalc <- 
  fights %>% 
  dplyr::select(date = time_stamp, lat = lat, lon = lon) %>%
  data.frame()
fights$sun <- suncalc::getSunlightPosition(data = data_for_suncalc)$altitude * 180/pi
pretty_plot(fights$time_stamp, fights$sun, 
            pretty_axis_args = list(axis = list(list(format = "%b-%y"), list()))
            )
table(!is.na(fights$sun)) 
# TRUE

################################
#### Get current velocities from WeStCOMS 

#### Define WeStCOMS dates/names/times
fights$date_name <- fvcom.tbx::date_name(fights$date)
fights$hour      <- round(Tools4ETS::hour_dbl(fights$time_stamp))
fights$year      <- lubridate::year(fights$date)

#### Define WeStCOMS cells 
raster::crs(mesh) <- wgs84
fights$mesh_ID    <- fvcom.tbx::find_cells(lat = fights$lat, long = fights$lon, 
                                           mesh = mesh, 
                                           proj = wgs84, 
                                           return = 4)
fights$mesh_ID    <- as.integer(as.character(fights$mesh_ID))

#### Query FVCOM files from the SAMS thredds server (~5-10 mins)
## Define a list, with one element for each date, where
# ... each element is a dataframe with the capture events 
# ... on that date and the current velocity vector predictions
# ... across all 10 layers 
run <- FALSE
if(run){
  fights_by_date <- split(fights, fights$date_name)
  velocities_by_date <- 
    lapply(fights_by_date, function(fights_on_date){
      
      ## Define data for query 
      message("-------------------------------------------------------------------")
      # fights_on_date <- fights_by_date[[2]]
      dat <- fights_on_date[, c("date_name", "hour", "mesh_ID")]
      dat <- 
        lapply(1:10, function(i) {
          dat$layer <- i
          return(dat)
        }) %>% 
        dplyr::bind_rows() %>%
        dplyr::arrange(date_name, mesh_ID, hour, layer)
      
      ## Define server_catalog for query 
      # ... Data from 2018 are available from:
      # ... ... https://thredds.sams.ac.uk/thredds/catalog/scoats-westcoms1/Archive/
      # ... Data from April 2019-2020 are available from:
      # ... ... https://thredds.sams.ac.uk/thredds/catalog/scoats-westcoms2/Archive/
      if(fights_on_date$year[1] < 2019){
        server_catalog <- paste0("https://thredds.sams.ac.uk/thredds/",
                                 "catalog/scoats-westcoms1/Archive/",
                                 "netcdf_", fights_on_date$year[1], "/catalog.html")
      } else {
        server_catalog <- paste0("https://thredds.sams.ac.uk/thredds/",
                                 "catalog/scoats-westcoms2/Archive/",
                                 "netcdf_", fights_on_date$year[1], "/catalog.html")
      }
      
      ## Extract model predictions 
      dat_with_u <- fvcom.tbx::thredds_extract(dat, var = "u", 
                                               server_catalog = server_catalog)
      dat_with_v <- fvcom.tbx::thredds_extract(dat, var = "v", 
                                               server_catalog = server_catalog)
      dat$u <- dat_with_u[[1]]$wc
      dat$v <- dat_with_v[[1]]$wc
      return(dat)
    })
  saveRDS(velocities_by_date, "./data-raw/skate/skate_capture_velocities.rds")
} else {
  velocities_by_date <- readRDS("./data-raw/skate/skate_capture_velocities.rds")
}

#### Sum current speeds across all layers for each capture event
velocities <- 
  velocities_by_date %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(cs = sqrt(u^2 + v^2)) %>%
  dplyr::group_by(date_name, mesh_ID) %>%
  dplyr::mutate(current_speed = mean(cs)) %>%
  dplyr::slice(1L)

#### Add current speed metric to fights dataframe
fights$key           <- paste0(fights$date_name, "-", fights$mesh_ID)
velocities$key       <- paste0(velocities$date_name, "-", velocities$mesh_ID)
fights$current_speed <- velocities$current_speed[match(fights$key, velocities$key)]
table(is.na(fights$current_speed))

#### Add body (dorsal) surface area
fights$size_area <- 0.5 * (fights$size_disc/100)^2
table(!is.na(fights$size_area))
# TRUE

#### Select rows/columns 
fights <- fights[complete.cases(fights[, c("event_id", 
                                           "time_fight", 
                                           "size_area", 
                                           "current_speed", 
                                           "sun", 
                                           "depth", 
                                           "healthy")]), ]
nrow(fights)

#### Examine handling time 
# utils.add::basic_stats(difftime(fights$time_release, fights$time_stamp), na.rm = TRUE)

#### Drop records of individuals caught during the processing of another individual
## Justification 
# In these circumstances, fight were often held on the bottom, rather than pulling
# ... them immediately to the surface, so fight times may be positively biased
# ... and not reflect responses of skate to capture. 
## Define blank columns and intervals 
fights$check_1  <- fights$check_2 <- fights$check_3 <- NA
fights$interval <- lubridate::interval(fights$time_stamp, fights$time_release)
fights$interval[is.na(fights$time_release)] <- NA
## Loop over each individual and identify whether or not time on hook occurred
# ... during the processing of another individual
for(i in 1:nrow(fights)){
  # Isolate fights of other individuals
  fights_of_others <- fights[-i, ]
  pos <- which(!is.na(fights_of_others$interval))
  # Check whether time on hook occurred during handling intervals
  # ... Note that this includes if the individual was captured at the moment 
  # ... of release, which we will account for below. 
  fights$check_1[i]  <- 
    any(fights$time_stamp[i] %within% fights_of_others$interval[pos])
  # Check that the individual was not captured as the individual was released
  # ... in which case we can include the information from that individual. 
  fights$check_2[i] <- 
    all(fights$time_stamp[i] != fights_of_others$time_release[pos]) 
}
fights$check_3 <- fights$check_1 & fights$check_2
## Visual check 
visual_check <- FALSE
if(visual_check){
  fights %>% 
    dplyr::arrange(time_stamp) %>%
    dplyr::select(pit, time_stamp, time_release, check_1, check_2, check_3) %>%
    View()
}
## The number of excluded records: 
table(fights$check_3)
# FALSE  TRUE 
# 35     8 
## Exclude fight times for these individuals:
fights <- fights[-which(fights$check_3), ]
nrow(fights) # 35

#### Save processed fights dataframe
saveRDS(fights, "./data/skate/fights.rds")


#### End of code. 
################################
################################