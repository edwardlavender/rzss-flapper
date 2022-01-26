################################
################################
#### analyse_bloods_change.R

#### This script: 
# 1) Analyses how skate blood parameters change between BS1 and BS2.

#### Steps preceding this script: 
# 1) Define global parameters (define_global_param.R)
# 2) Process raw data         (process_data_raw.R)


################################
################################
#### Set up

#### Wipe workspace and source essential packages and variables
source("./R/define_global_param.R")

#### Load data
physio <- readRDS("./data/physio.rds")

#### Define local parameters
# Define whether or not to save figures
save <- TRUE


################################
################################
#### Plot BS1 and BS2 

#### Loop over each response and make plots
if(save) tiff("./fig/blood_sample_change.tiff", 
              height = 6, width = 8, units = "in", res = 800)
pp <- par(mfrow = par_mf(length(resps)), 
          oma = c(4, 4, 2, 2), mar = c(2, 2, 2, 2))
prompt <- FALSE # TRUE
lapply(1:length(resps), function(i){
  
  #### Define response
  resp <- resps[i]
  ylab <- ylabs[[resp]]
  resp_1 <- paste0(resp, "_1")
  resp_2 <- paste0(resp, "_2")
  y          <- c(physio[, resp_1], physio[, resp_2])
  y_is_valid <- !is.na(y)
  y          <- y[y_is_valid]
  
  #### Define 'x' variable
  x <- rep(c("BS1", "BS2"), each = nrow(physio))
  x <- x[y_is_valid]
  x <- factor(x, levels = unique(x))

  #### Make boxplot
  pretty_boxplot(x, y, 
                 varwidth = TRUE,
                 ylab = "",
                 pretty_axis_args = list(side = 1:2, control_digits = 1)
                 )
  mtext(side = 2, ylab, line = 2)
  mtext(side = 3, LETTERS[i], font = 2, adj = 0)
  if(prompt) readline("Press [Enter] to continue...")
}) %>% invisible()
mtext(side = 1, "Blood sample", line = 1, outer = TRUE)
par(pp)
if(save) dev.off()

#### End of code. 
################################
################################