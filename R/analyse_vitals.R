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
rates$event_id_int <- rates$event_id
rates$event_id     <- factor(rates$event_id)
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
length(which(!is.na(rates$hr)))
utils.add::basic_stats(rates$hr, na.rm = TRUE)
length(which(!is.na(rates$rr)))
utils.add::basic_stats(rates$rr, na.rm = TRUE)
ind_cor <- !is.na(rates$hr) & !is.na(rates$rr)
length(which(ind_cor))
h <- cor.test(rates$hr[ind_cor], rates$rr[ind_cor], method = "spearman")
# 0.5280126

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
resp <- "hr" # "hr"
rates$resp <- rates[, resp]
rates_for_resp <- rates[, c("event_id_int", "event_id", 
                            "resp", 
                            "sex", "size_len", 
                            "time_from_capture_to_surface", "temp_water", 
                            "gaff", "time_index")]
rates_for_resp <- rates_for_resp[complete.cases(rates_for_resp), ]
rates_for_resp$event_id <- factor(rates_for_resp$event_id)
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
# ... 28-29 events are uniquely defined by their size
# ... ~8 pairs of events share the same size
# ... ~2 trios of events share the same size
rates_for_resp %>%
  dplyr::group_by(event_id) %>% 
  dplyr::slice(1L) %>%
  dplyr::pull(size_len) %>%
  table() %>% 
  table() %>% 
  as.numeric()/length(unique(rates_for_resp$size_len))
# For both variables, 74 % of events are uniquely defined by their size 

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

################################
#### Model fitting 

#### Implement model(s)
mod_1 <- gam(resp ~ 
               sex + size_len + temp_water * time_from_capture_to_surface + 
               gaff + 
               s(event_id, bs = "re") +
               s(event_id, time_index, bs = "re"),
             family = nb(),
             data = rates_for_resp, 
             method = "REML")
mod_2 <- gam(resp ~ 
               sex + temp_water * time_from_capture_to_surface + 
               gaff + 
               te(size_len, time_index),
             family = nb(),
             data = rates_for_resp, 
             method = "REML")


################################
#### Model comparison and summary   

#### Model comparison
( ranks <- AIC(mod_1, mod_2) )
message(round(max(ranks$AIC) - min(ranks$AIC), digits = 2))
message(rownames(ranks)[which.min(ranks$AIC)])
mod <- get(rownames(ranks)[which.min(ranks$AIC)])
mod <- mod_1
# hr: delta AIC = 17.5
# rr: delta AIC = 113.36

#### Model summary
summary(mod, digits = 3)
con <- paste0("./fig/", resp, "_coef.txt")
sink(con)
summary(mod, digits = 3)
sink()


################################
#### Model predictions 

#### Model smooths 
plot(mod, all.terms = TRUE, pages = 1, scheme = 1)

#### Estimate the difference in vital rates for the smallest versus largest female 
ind_f <- rates_for_resp$sex == "F"
predict(mod, 
        newdata = data.frame(sex = factor("F", levels = c("F", "M")), 
                             size_len = c(min(rates_for_resp$size_len[ind_f]), 
                                          max(rates_for_resp$size_len[ind_f])),
                             time_from_capture_to_surface = median(rates_for_resp$time_from_capture_to_surface), 
                             temp_water = median(rates_for_resp$temp_water), 
                             gaff = factor("N", levels = c("N", "Y")), 
                             event_id = factor(levels(rates_for_resp$event_id)[1], 
                                               levels = levels(rates_for_resp$event_id)),
                             time_index = min(rates_for_resp$time_index)),
        type = "response",
        se.fit = TRUE)

#### Visualise model predictions for each variable 

## Set up figure to save
if(save) tiff(paste0("./fig/", resp, ".tiff"), 
              height = 5.5, width = 9, units = "in", res = 600)
pp <- par(mfrow = c(2, 3), oma = c(2, 2, 2, 2), mar = rep(2.5, 4))
rates_in_mod <- model.frame(mod)

## Define titles
xlabs <- c("Sex", 
           "Length [cm]", 
           expression("Temperature [" * degree * "C]"), 
           expression("Time (hook" %->% "surface) [mins]"), 
           "Gaff", 
           "Time on deck [mins]"
           )
xlab_line = 2.25
main_adj  <- 0
main_font <- 2

## Define point colours, shapes and sizes
col_param <- pretty_cols_brewer(zlim = range(rates_in_mod$temp_water), 
                   scheme = "RdYlBu"
                   )
col_param$col <- scales::alpha(col_param$col, 0.5)
rates_in_mod$pt_col <- col_param$col[findInterval(rates_in_mod$temp_water, col_param$breaks)]
# rates_in_mod$pt_col <- c("royalblue", "black")[rates_in_mod$sex]
rates_in_mod$pt_cex <- rates_in_mod$size_len/225
rates_in_mod$pt_pch <- 21 # c(1, 4)[rates_in_mod$sex]
pt_param <- list(col = rates_in_mod$pt_col, 
                 cex = rates_in_mod$pt_cex, 
                 pch = rates_in_mod$pt_pch)

## Plot predictions for sex
pretty_predictions_1d(model = mod, 
                      x_var = c("sex", "size_len"),
                      add_xlab = list(text = xlabs[1:2], line = xlab_line),
                      add_ylab = NULL,
                      add_main = list(text = c("A", "B"), adj = main_adj, font = main_font),
                      add_error_bars = ebars_param,
                      add_points = pt_param, 
                      one_page = FALSE
                      )

## Plot predictions for temperature (by fight time)
# Define prediction data 
p_n <- 100
p_x <- seq(min(rates_for_resp$temp_water), 
           max(rates_for_resp$temp_water), 
           length.out = p_n)
p_d <- data.frame(temp_water = p_x)
p_d$time_from_capture_to_surface <- min(rates_for_resp$time_from_capture_to_surface)
p_d$sex        <- factor("F", levels = c("F", "M"))
p_d$size_len   <- mean(rates_for_resp$size_len)
p_d$gaff       <- factor("N", levels = c("N", "F"))
p_d$event_id   <- factor(levels(rates_for_resp$event_id)[1], levels = levels(rates_for_resp$event_id))
p_d$time_index <- 1
# Plot predictions for effects of temperature when fight time is low 
pretty_predictions_1d(model = mod, 
                      newdata = p_d[p_d$time_from_capture_to_surface == min(rates_for_resp$time_from_capture_to_surface), ],
                      x_var = "temp_water",
                      add_points = NULL,
                      add_error_envelope = 
                        list(add_fit = list(col = "royalblue"), 
                             add_ci = list(col = scales::alpha("skyblue", 0.2), border = FALSE)),
                      add_xlab = list(text = xlabs[3], line = xlab_line), 
                      add_ylab = NULL, 
                      add_main = list(text = "C", adj = main_adj, font = main_font)
                      )
# Add predictions for effects of temperature when fight time is high 
p_d$time_from_capture_to_surface <- max(rates_for_resp$time_from_capture_to_surface)
p_ci <- list_CIs(predict(mod, p_d, se.fit = TRUE, type = "response"))
add_error_envelope(p_d$temp_water, 
                   p_ci, 
                   add_fit = list(col = "darkred"),
                   add_ci = list(col = scales::alpha("red", 0.2), border = FALSE)
)
add_pt <- pt_param
add_pt$x <- rates_for_resp$temp_water
add_pt$y <- rates_for_resp$resp
do.call(points, add_pt)

## Plot predictions for fight time (by temperature)
# Define prediction data 
p_n <- 100
p_x <- seq(min(rates_for_resp$time_from_capture_to_surface), 
           max(rates_for_resp$time_from_capture_to_surface), 
           length.out = p_n)
p_d            <- data.frame(time_from_capture_to_surface = p_x)
p_d$temp_water <- min(rates_for_resp$temp_water)
p_d$sex        <- factor("F", levels = c("F", "M"))
p_d$size_len   <- mean(rates_for_resp$size_len)
p_d$gaff       <- factor("N", levels = c("N", "F"))
p_d$event_id   <- factor(levels(rates_for_resp$event_id)[1], levels = levels(rates_for_resp$event_id))
p_d$time_index <- 1
# Plot predictions for effects of fight time in low temperatures 
pretty_predictions_1d(model = mod, 
                      newdata = p_d[p_d$temp_water == min(rates_for_resp$temp_water), ],
                      x_var = "time_from_capture_to_surface",
                      add_points = NULL,
                      add_error_envelope = 
                        list(add_fit = list(col = "royalblue"), 
                             add_ci = list(col = scales::alpha("skyblue", 0.2), border = FALSE)),
                      add_xlab = list(text = xlabs[4], line = xlab_line), 
                      add_ylab = NULL, 
                      add_main = list(text = "D", adj = main_adj, font = main_font)
                      )
# Add predictions for effects of fight time in warm temperatures
p_d$temp_water <- max(rates_for_resp$temp_water)
p_ci <- list_CIs(predict(mod, p_d, se.fit = TRUE, type = "response"))
add_error_envelope(p_d$time_from_capture_to_surface, 
                   p_ci, 
                   add_fit = list(col = "darkred"),
                   add_ci = list(col = scales::alpha("red", 0.2), border = FALSE)
                   )
add_pt <- pt_param
add_pt$x <- rates_for_resp$time_from_capture_to_surface
add_pt$y <- rates_for_resp$resp
do.call(points, add_pt)

## Plot predictions for GAFF and handling time 
pretty_predictions_1d(model = mod, 
                      x_var =  c("gaff", "time_index"),
                      add_error_bars = ebars_param,
                      add_points = pt_param,
                      add_xlab = list(text = xlabs[5:6], line = xlab_line),
                      add_ylab = NULL,
                      add_main = list(text = c("E", "F"), adj = main_adj, font = main_font), 
                      one_page = FALSE)
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
  mtext(side = 3, text = paste0("[", rate$event_id_int, "]"), line = -2, font = main_font)
}) %>% invisible()
mtext(side = 1, "Time on deck [mins]", line = 4, outer = TRUE, cex = 1.5)
mtext(side = 2, ylab, line = 4, outer = TRUE, cex = 1.5)
par(pp)
dev.off()


################################
#### Model diagnostics

#### Standard residual diagnostics
if(save) png(paste0("./fig/", resp, "_diag.png"), 
             height = 12, width = 12, units = "in", res = 800)
pp <- par(mfrow = c(2, 2))
gam.check(mod, rep = 1000, type = "deviance")
par(pp)
if(save) dev.off()


#### End of code. 
################################
################################