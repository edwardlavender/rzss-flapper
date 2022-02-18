################################
################################
#### analyse_bloods_synthesis.R

#### This code:
# 1) Synthesises the effects of each variable on 
# ... blood parameters using a simulation approach. 

#### Steps preceding this code:
# 1) Define global parameters   (define_global_param.R)
# 2) Process bloods             (process_bloods.R)
# 3) Develop modelling approach (analyse_bloods.R)


################################
################################
#### Set up

#### Wipe workspace and source essential packages and variables
source("./R/define_global_param.R")

#### Load data
physio <- readRDS("./data/skate/physio.rds")


################################
################################
#### Implement simulations

################################
#### Set up simulations 

#### Define blood sample ("1" or "2") and response variables
sample <- "2"
if(sample == "1"){
  resps_for_bs <- paste0(resps, "_1")
} else if(sample == "2"){
  resps_for_bs <- paste0(resps, "_2")
} else {
  stop("sample not correctly specified.")
}

#### Define model formula (specific to blood sample)
if(sample == "1"){
  form_1 <- resp ~
    sex + 
    size_len + 
    temp_water * 
    time_from_capture_to_surface + time_from_surface_to_bs1 + 
    gaff
} else {
  form_1 <- resp ~
    sex + 
    size_len + 
    temp_water * 
    time_from_capture_to_surface + time_from_surface_to_bs2 + 
    gaff
}

#### Define newdata skeleton
skeleton <- 
  data.frame(sex = factor("F", levels = c("F", "M")),
             size_len = median(physio$size_len, na.rm = TRUE),
             temp_water = median(physio$temp_water), 
             time_from_capture_to_surface = 
               median(physio$time_from_capture_to_surface, na.rm = TRUE), 
             time_from_surface_to_bs1 = 
               median(physio$time_from_surface_to_bs1, na.rm = TRUE), 
             gaff = factor("N", levels = c("N", "Y")))
if(sample == "2"){
  skeleton$time_from_surface_to_bs1 <- NULL
  skeleton$time_from_surface_to_bs2 <- 
    median(physio$time_from_surface_to_bs2, na.rm = TRUE)
}
skeleton <- rbind(skeleton, skeleton)

#### Define simulation comparisons 
## Sex: 
# ... female versus male
## Body size: minimum (female) versus maximum (female) 
## Temperature: 
# ... minimum versus maximum (low fight time)
# ... minimum versus maximum (high fight time)
## Time from capture to surface
# ... minimum versus maximum (low temperature)
# ... minimum versus maximum (high temperature)
## From from surface to BS1
# ... minimum versus maximum 
## Gaff
# ... N versus Y
comparisons <- 
  list(
    sex           = data.frame(sex = factor(c("F", "M"), levels = c("F", "M"))), 
    temp_1        = data.frame(temp_water = range(physio$temp_water), 
                               time_from_capture_to_surface = min(physio$time_from_capture_to_surface)), 
    temp_2        = data.frame(temp_water = range(physio$temp_water), 
                               time_from_capture_to_surface = max(physio$time_from_capture_to_surface)),
    time_fight_1  = data.frame(time_from_capture_to_surface = range(physio$time_from_capture_to_surface), 
                               temp_water = min(physio$temp_water)),
    time_fight_2  = data.frame(time_from_capture_to_surface = range(physio$time_from_capture_to_surface), 
                               temp_water = max(physio$temp_water)), 
    time_surface  = data.frame(time_from_surface_to_bs1 = range(physio$time_from_surface_to_bs1)), 
    gaff          = data.frame(gaff = factor(c("N", "Y"), levels = c("N", "Y")))
  )
if(sample == "2"){
  comparisons$time_surface <- 
    data.frame(time_from_surface_to_bs2 = range(physio$time_from_surface_to_bs2, na.rm = TRUE))
}

#### Define simulation comparison labels
labels <- c(sex          = expression(Sex[M]), 
            temp_1       = expression("T (" * Time["hook" %->% "surface"[min]] * ")"),
            temp_2       = expression("T (" * Time["hook" %->% "surface"[max]] * ")"),
            time_fight_1 = expression(Time["hook" %->% "surface"] * " (" * T[min] *")"), 
            time_fight_2 = expression(Time["hook" %->% "surface"] * " (" * T[max] *")"), 
            time_surface = expression(Time["surface" %->% "BS1"]), 
            gaff         = expression(Gaff[Y])
            )
if(sample == 2){
  labels["time_surface"] <- (Time["surface" %->% "BS2"])
}

################################
#### Implement simulation 

#### Implement simulations 
# [with R = 1e3 simulations, this takes 2 mins one one core]
run <- FALSE
if(run){
  R <- 1e3
  outsims <- 
    pbapply::pblapply(resps_for_bs, cl = 7L, function(resp){
      
      #### Fit model for response
      # resp <- resps_for_bs[7]
      print(paste0(resp, "-----------------------------"))
      physio$resp <- physio[, resp]
      mod <- lm(form_1, data = physio)
      
      #### Define comparisons for selected response 
      comparisons_by_resp <- 
        lapply(1:length(comparisons), function(i){
          print(i)
          
          #### Set simulation specific newdata values 
          # i <- 1
          comparison <- comparisons[[i]]
          newdata <- skeleton
          for(x in colnames(comparison)){
            newdata[, x] <- comparison[, x]
          }
          
          #### Implement bootstrap to estimate ratios
          # Use tryCatch because for some variables (e.g., K_2)
          # ... there are too few observation (e.g., for Gaff 'Y') to 
          # ... implement bootrstrapping. In the sampling proceedure, 
          # ... observations are dropped and the model can't fit
          # ... because gaffing has only one factor level. 
          ratio <- tryCatch({
            finalfit::boot_predict(mod, 
                                   newdata = newdata, 
                                   comparison = "ratio", 
                                   condense = FALSE,
                                   R = R)
            
          }, error = function(e) return(e)
          )
          
          #### Extract relevant information
          if(!inherits(ratio, "error")){
            out <- data.frame(comparison = names(comparisons)[i], 
                              resp = resp, 
                              ratio = ratio$ratio[2], 
                              lowerCI = ratio$ratio_conf.low[2], 
                              upperCI = ratio$ratio_conf.high[2])
          } else {
            out <- data.frame(comparison = names(comparisons)[i], 
                              resp = resp,
                              ratio = NA,
                              lowerCI = NA, 
                              upperCI = NA)
          }
          # print("ok")
          return(out)
        })
      
      out <- dplyr::bind_rows(comparisons_by_resp)
      return(out)
    })
  saveRDS(outsims, paste0("./data/estimates/bloods_sims_", sample, "_.rds"))
} else {
  outsims <- readRDS(paste0("./data/estimates/bloods_sims_", sample, "_.rds"))
}


################################
#### Process simulations 

#### Collate estimates
outsims <- 
  outsims %>% 
  dplyr::bind_rows() %>%
  dplyr::mutate(comparison = factor(comparison, levels = names(comparisons)), 
                resp       = factor(resp, levels = resps_for_bs)) %>%
  dplyr::arrange(comparison, resp)

#### Define simulation IDs (for the x axis)
outsims$id <- 0
gap <- 5
for(i in 2:nrow(outsims)){
  if(outsims$comparison[i-1] == outsims$comparison[i]){
    outsims$id[i] <- outsims$id[i - 1] + 1
  } else {
    outsims$id[i] <- outsims$id[i - 1] + gap
  }
}

#### Define colours to distinguish blood parameters
cols        <- RColorBrewer::brewer.pal(length(resps), "Dark2")
outsims$col <- cols[outsims$resp]


################################
#### Visualise simulations 

#### Set up plot to save
png(paste0("./fig/blood_ratios_", sample, ".png"), 
    height = 6, width = 14, units = "in", res = 600)
pp <- par(oma = c(2, 1, 1, 1), mar = c(2, 1, 1, 1))
xlim <- range(c(-gap/2, outsims$id))

#### Make blank plot 
axis_ls <- 
  pretty_plot(outsims$id, outsims$ratio, 
              pretty_axis_args = 
                list(x = list(x = range(c(-gap/2, outsims$id)), 
                              y = range(c(outsims$lowerCI, outsims$upperCI), na.rm = TRUE)), 
                     add = FALSE),
              xlim = xlim,
              xlab = "", ylab = "",
              type = "n")

#### Add visual aids
# Vertical lines to separate comparisons
outsims_by_comparison <- split(outsims, outsims$comparison)
lapply(outsims_by_comparison[2:length(outsims_by_comparison)], function(d){
  lines(rep(d$id[1], 2) - gap/2, axis_ls[[2]]$lim, lty = 3, lwd = 0.5)
})
# Horizontal line at effect size = 0
lines(axis_ls[[1]]$lim, c(1, 1), lty = 2)
7
#### Add error bars
elwd <- 1.5
add_error_bars(outsims$id, 
               outsims$ratio, 
               lwr = outsims$lowerCI, upr = outsims$upperCI, lwd = elwd,
               add_fit = list(pch = "-", col = outsims$col, lwd = elwd), 
               col = outsims$col)

#### Add axes
axis_ls[[1]]$axis$at <- outsims %>%
  dplyr::group_by(comparison) %>%
  dplyr::summarise(at = mean(id)) %>%
  dplyr::pull(at)
axis_ls[[1]]$axis$labels <- labels
pretty_axis(axis_ls = axis_ls, add = TRUE)

#### Add legend
px <- par(xpd = NA)
if(sample == "1") ypos <- 60 else ypos <- 80
legend(-2.5, ypos,
       lty = rep(1, length(comparisons)), 
       col = cols, 
       legend = ylabs_legend, 
       bty = "n")
par(px)

#### Add titles and save
mtext(side = 1, "Term", line = 2)
mtext(side = 2, "Effect size (ratio)", line = 1)
par(pp)
dev.off()


################################
################################
