################################
################################
#### analyse_vitals.R

#### This script: 
# 1) Analyses skate vital signs. 

#### Steps preceding this script: 
# 1) Define global parameters (define_global_param.R)
# 2) Process vital signs      (process_vitals.R)


################################
################################
#### Set up

#### Wipe workspace and source essential packages and variables
source("./R/define_global_param.R")

#### Load required packages
library(mgcv)
library(ggplot2)

#### Define local parameters
# Define whether or not to save figures
save <- TRUE

#### Load data 
rates <- readRDS("./data/skate/rates.rds")


################################
################################
#### Data processing

#### Order by time_index
rates <- 
  rates %>% 
  dplyr::arrange(event_id, time_index) %>%
  data.frame()


################################
################################
#### Data exploration

#### Key questions: 
# 1) How does temperature affect respiration/heart rates? 
# 2) How does fight time affect respiration/heart rates?
# 3) How do respiration/heart rates change through time on deck? 

#### Summary statistics 
length(unique(rates$pit))
utils.add::basic_stats(rates$hr, na.rm = TRUE)
utils.add::basic_stats(rates$rr, na.rm = TRUE)

#### Correlations 
# Note the relatively high (negative) correlation 
# ... between body size and time from capture to surface
psych::pairs.panels(rates[, c("hr", "rr", 
                              "sex", 
                              "size_len", 
                              "time_from_capture_to_surface", 
                              "temp_water", 
                              "gaff", 
                              "healthy", 
                              "time_index"
                              )])

#### Define response variable
resp <- "rr" # "hr"
rates$resp <- rates[, resp]
rates_for_resp <- rates[, c("event_id", "resp", 
                            "sex", "size_len", 
                            "time_from_capture_to_surface", "temp_water", 
                            "gaff", "time_index")]
rates_for_resp <- rates_for_resp[complete.cases(rates_for_resp), ]
if(resp == "rr"){
  ylab <- "Respiratory rate [bpm]"
} else if(resp == "hr"){
  ylab <- "Heart rate [bpm]"
}

#### Examine the correlation between body size and fight time more closely
rates_for_time_1 <- 
  rates_for_resp %>% 
  dplyr::group_by(event_id) %>%
  dplyr::slice(1L) %>%
  dplyr::filter(!is.na(time_from_capture_to_surface)) %>%
  dplyr::filter(!is.na(size_len))
pretty_plot(rates_for_time_1$time_from_capture_to_surface, rates_for_time_1$size_len)
cor(rates_for_time_1$time_from_capture_to_surface, rates_for_time_1$size_len)
# 0.5374871 (rr), 0.5314036 (hr)

#### Capture events and body size
# Are capture events uniquely defined by size? 
# ... 28-29 individuals are uniquely defined by their size
# ... ~8 pairs of individuals share the same size
# ... ~2 trios of individuals share the same size
rates_for_resp %>%
  dplyr::group_by(event_id) %>% 
  dplyr::slice(1L) %>%
  dplyr::pull(size_len) %>%
  table() %>% 
  table()

#### Period of observations
# There are only few observations after more than 20 minutes
hist(rates$time_index)
# Examine evidence for change during handling 
ggplot() +
  geom_point(aes(x = time_index, y = resp), data = rates_for_resp) + 
  facet_wrap(~event_id)


################################
################################
#### Modelling 

#### Implement model(s)
rates$event_id <- factor(rates$event_id)
mod_1 <- gam(resp ~ 
                sex + size_len + time_from_capture_to_surface + 
                temp_water + 
                gaff + 
                s(event_id, bs = "re") +
                s(event_id, time_index, bs = "re"),
              family = nb(),
              data = rates)
mod_2 <- gam(resp ~ 
                sex + time_from_capture_to_surface + 
                temp_water + 
                gaff + 
               te(size_len, time_index),
              family = nb(),
              data = rates, 
              method = "REML")

#### Model comparison
( ranks <- AIC(mod_1, mod_2) )
message(round(max(ranks$AIC) - min(ranks$AIC), digits = 2))
message(rownames(ranks)[which.min(ranks$AIC)])
mod <- get(rownames(ranks)[which.min(ranks$AIC)])
mod <- mod_1

#### Model summary
summary(mod)

#### Model smooths
plot(mod, all.terms = TRUE, pages = 1, scheme = 1)

#### Model predictions
xlabs <- c("Sex", 
           "Length [cm]", 
           expression("Time (hook" %->% "surface) [mins]"), 
           expression("Temperature [" * degree * "C]"), 
           "Gaff", 
           "Time on deck [mins]"
           )
if(save) tiff(paste0("./fig/", resp, ".tiff"), 
              height = 5.5, width = 9, units = "in", res = 600)
pp <- par(oma = c(2, 2, 2, 2), mar = rep(2.5, 4))
rates_in_mod <- model.frame(mod)
rates_in_mod$cols <- c("royalblue", "black")[rates_in_mod$sex]
pretty_predictions_1d(model = mod, 
                      x_var = c("sex", "size_len", "time_from_capture_to_surface", 
                              "temp_water", "gaff", "time_index"),
                      add_points = list(cex = rates_in_mod$size_len/200, lwd = 0.5, col = rates_in_mod$col),
                      add_error_bars = list(add_fit = list(pch = 21, bg = "black", cex = 2)),
                      add_xlab = list(text = xlabs, line = 2.25),
                      add_ylab = list(text = ylab),
                      add_main = list(text = LETTERS[1:6], adj = 0, font = 2))
par(pp)
if(save) dev.off()

#### Visualise predictions through time for each event
## Set up plot to save
png(paste0("./fig/", resp, "_time_series.png"), 
    height = 12, width = 12, units = "in", res = 800)

## Define graphical parameters 
cex.axis <- 1.4

## Define plotting window 
# Define a list of events to loop over
rates_for_resp_by_event <- split(rates_for_resp, rates_for_resp$event_id)
# Define plots for which x and y axes will be added
n <- length(rates_for_resp_by_event)
dim <- par_mf(n)
mat <- matrix(1:(dim[1] * dim[2]), nrow = dim[1], ncol = dim[2], byrow = TRUE)
mat
ind_for_x <- mat[nrow(mat), ]
ind_for_y <- mat[, 1]
# Define par param 
pp <- par(mfrow = dim, 
          oma = c(6, 6, 2, 2), mar = c(0, 0, 0, 0), 
          xaxs = "i", yaxs = "i", 
          mgp = c(3, 0.5, 0.5))

## Make plots
lapply(1:length(rates_for_resp_by_event), function(i){
  # Isolate data for event
  # i <- 1
  rate <- rates_for_resp_by_event[[i]]
  # Make predictions 
  pred <- predict(mod, newdata = rate, se.fit = TRUE, type = "response")
  pred <- list_CIs(pred)
  # Define axis limits
  use_dynamic_limits <- FALSE
  if(use_dynamic_limits){
    paa <- list(side = 1:4, 
                pretty = list(list(n = 3), list(n = 5)),
                control_axis = list(tck = 0.02, las = TRUE, cex.axis = cex.axis),
                x = list(x = range(rate$time_index),
                         y = range(c(rate$resp, pred$lowerCI, pred$upperCI))))
  } else {
    paa <- list(side = 1:4, 
                pretty = list(list(n = 3), list(n = 5)),
                control_axis = list(tck = 0.02, las = TRUE, cex.axis = cex.axis),
                x = list(x = range(rates_for_resp$time_index), 
                         y = range(rates_for_resp$resp)))
  }
  paa$add <- FALSE
  axis_ls <- do.call(pretty_axis, paa)
  xlim <- axis_ls[[1]]$lim
  ylim <- axis_ls[[2]]$lim
  # Create blank plot
  plot(rate$time_index, rate$resp, 
       xlim = xlim, ylim = ylim,
       axes = FALSE, 
       xlab = "", ylab = "",
       type = "n")
  # Add predictions and observations 
  add_error_envelope(rate$time_index, pred)
  points(rate$time_index, rate$resp)
  # Add axes 
  # rect(xlim[1], ylim[1], xlim[2], ylim[2])
  axis_ls[[1]]$axis$labels[length(axis_ls[[1]]$axis$labels)] <- ""
  if(!(i %in% ind_for_x)) axis_ls[[1]]$axis$labels <- FALSE
  axis_ls[[3]]$axis$labels  <- FALSE
  # axis_ls[[3]]$axis$lwd.tick <- 0
  do.call(graphics::axis, axis_ls[[1]]$axis)
  do.call(graphics::axis, axis_ls[[3]]$axis)
  axis_ls[[2]]$axis$labels[length(axis_ls[[2]]$axis$labels)] <- ""
  if(!(i %in% ind_for_y)) axis_ls[[2]]$axis$labels <- FALSE
  axis_ls[[4]]$axis$labels   <- FALSE
  # axis_ls[[4]]$axis$lwd.tick <- 0
  do.call(graphics::axis, axis_ls[[2]]$axis)
  do.call(graphics::axis, axis_ls[[4]]$axis)
  mtext(side = 3, text = paste0("[", rate$event_id, "]"), line = -2, font = 2)
}) %>% invisible()
mtext(side = 1, "Time on deck [mins]", line = 4, outer = TRUE, cex = 1.5)
mtext(side = 2, ylab, line = 4, outer = TRUE, cex = 1.5)
par(pp)
dev.off()

#### Model diagnostics
if(save) png(paste0("./fig/", resp, "_diag.png"), 
             height = 12, width = 12, units = "in", res = 800)
pp <- par(mfrow = c(2, 2))
gam.check(mod_1, rep = 1000, type = "deviance")
par(pp)
if(save) dev.off()


#### End of code. 
################################
################################