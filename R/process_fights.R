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

#### Load data
# processed rates/physio data as a source of capture IDs, dates etc.
rates <- readRDS("./data/skate/rates.rds")
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
    
    ## Get time on hook
    rates_raw_is_time <- apply(rates_raw, 
                               2, 
                               function(x) stringr::str_detect(x, "Time on hook"))
    anchor <- which(rates_raw_is_time, arr.ind = TRUE)
    stopifnot(nrow(anchor) >= 1L)
    anchor <- anchor[1, , drop = FALSE]
    stopifnot(nrow(anchor) == 1L)
    anchor[, 2] <- anchor[, 2] + 1
    time <- rates_raw[anchor]
    time <- chron::times(as.numeric(time))
    
    ## Define timestamps 
    time_stamp <- as.POSIXct(paste0(date, time), tz = "UTC")
    
    ## Return extracted information
    out <- data.frame(sheet_name = sheet, 
                      sheet_index = i,
                      time_stamp = time_stamp, 
                      xy_raw = xy)
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
#### Process skeleton dataframe 

#### Read skeleton captures dataframe
fights <- readxl::read_excel("./data-raw/skate/captures.xlsx", sheet = "data")

#### Define column classes
str(fights)
fights$sex <- factor(fights$sex)

#### Check capture locations 
# One location appears to be erroneous
pretty_map(add_polys = list(x = coast))
text(x = fights$lon, y = fights$lat, 
     labels = fights$sheet_name, col = "red", cex = 0.5)
fights$lon[fights$sheet_name == "22082018 5"] <- NA

#### Select columns
fights$xy_raw <- NULL
fights$notes  <- NULL
fights        <- fights[fights$healthy == 1, ]
fights        <- fights[complete.cases(fights), ]
# Recheck locations
pretty_map(coast, 
           add_polys = list(x = coast), 
           add_points = list(x = fights$lon, y = fights$lat, col = "red"))

#### Add depths 
fights$depth <- abs(raster::extract(digi, fights[, c("lon", "lat")]))

#### Add sun angles
data_for_suncalc <- 
  fights %>% 
  dplyr::select(date = time_stamp, lat = lat, lon = lon) %>%
  data.frame()
fights$sun <- suncalc::getSunlightPosition(data = data_for_suncalc)$altitude * 180/pi
pretty_plot(fights$time_stamp, fights$sun, 
            pretty_axis_args = list(axis = list(list(format = "%b-%y"), list()))
            )


################################
#### Get current velocities from WeStCOMS 

#### Define WeStCOMS dates/names/times
fights$date      <- as.Date(fights$time_stamp)
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
  dplyr::mutate(current_speed = sum(cs)) %>%
  dplyr::slice(1L)

#### Add current speed metric to fights dataframe
fights$key           <- paste0(fights$date_name, "-", fights$mesh_ID)
velocities$key       <- paste0(velocities$date_name, "-", velocities$mesh_ID)
fights$current_speed <- velocities$current_speed[match(fights$key, velocities$key)]

#### Add body (dorsal) surface area
fights$size_area <- 0.5 * (fights$size_disc/100)^2
fights <- fights[complete.cases(fights), ]

#### Save processed fights dataframe
saveRDS(fights, "./data/skate/fights.rds")


#### End of code. 
################################
################################