#########################
#########################
#### analyse_vitals.R

#### Aims
# 1) Analyses skate vital signs.

#### Prerequisites
# 1) Define global parameters (define_global_param.R)
# 2) Process capture fights   (process_fights.R)
# 3) Process vital signs      (process_vitals.R)


#########################
#########################
#### Set up

#### Wipe workspace
rm(list = ls()) 
try(pacman::p_unload("all"), silent = TRUE) 
dv::clear() 

#### Essential packages
library(dv)
library(ggplot2)
library(prettyGraphics)
library(dplyr)
library(mgcv)
# Nominal call to {mgcViz} for DHARMa
if (!requireNamespace("mgcViz", quietly = TRUE)) {
  install.packages("mgcViz")
}

#### Load data
source(here_r("001_define_global_param.R"))
source(here_r("002_define_helpers.R"))
captures <- readRDS("./data/skate/capture_events.rds")
rates    <- readRDS("./data/skate/rates.rds")

#### Define local parameters
# Define whether or not to save figures
save <- TRUE
set.seed(1)


#########################
#########################
#### Data processing

#### Order by time stamp
rates$event_id_int <- rates$event_id
rates$event_id <- factor(rates$event_id)
rates <-
  rates |>
  dplyr::arrange(event_id, time_stamp) |>
  data.frame()

#### Define the times of key events
rates$time_surface <- captures$time_surface[match(rates$event_id, captures$event_id)]
rates$time_deck <- captures$time_deck[match(rates$event_id, captures$event_id)]
rates$time_from_surface_to_deck <- as.integer(difftime(rates$time_deck,
  rates$time_surface,
  units = "mins"
))
rates$time_from_deck_to_obs <- as.integer(difftime(rates$time_stamp,
  rates$time_deck,
  units = "mins"
))
range(rates$time_from_surface_to_deck)
range(rates$time_from_deck_to_obs)
rates <- rates[rates$time_from_deck_to_obs > 0, ]
utils.add::basic_stats(rates$time_from_surface_to_deck)
utils.add::basic_stats(rates$time_from_deck_to_obs)


#########################
#########################
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
# 0.5547736

#### Correlations
# Note the relatively high (negative) correlation
# ... between body size and time from capture to surface
pretty_pairs(rates[, c(
  "hr", "rr",
  "sex",
  "size_len",
  "time_from_capture_to_surface",
  "time_from_surface_to_deck",
  "temp_water",
  "gaff",
  "healthy", 
  "surgery"
)])

#### Define response variable
resp <- "rr" # "hr"
rates$resp <- rates[, resp]
rates_for_resp <- rates[, c(
  "event_id_int", "event_id",
  "resp",
  "sex", "size_len",
  "time_from_capture_to_surface", "temp_water",
  "time_from_surface_to_deck",
  "gaff", "surgery", "time_from_deck_to_obs"
)]
rates_for_resp <- rates_for_resp[complete.cases(rates_for_resp), ]
rates_for_resp$event_id <- factor(rates_for_resp$event_id)
if (resp == "rr") {
  ylab <- "Respiratory rate [bpm]"
} else if (resp == "hr") {
  ylab <- "Heart rate [bpm]"
}

#### Examine the correlation between body size and fight time more closely
rates_for_time_1 <-
  rates_for_resp |>
  dplyr::group_by(event_id) |>
  dplyr::slice(1L) |>
  dplyr::filter(!is.na(time_from_capture_to_surface)) |>
  dplyr::filter(!is.na(size_len))
pretty_plot(rates_for_time_1$time_from_capture_to_surface, rates_for_time_1$size_len)
cor(rates_for_time_1$time_from_capture_to_surface, rates_for_time_1$size_len)
# 0.5374871 (rr), 0.5314036 (hr)

#### Capture events and body size
# Are capture events uniquely defined by size?
# ... 28-29 events are uniquely defined by their size
# ... ~8 pairs of events share the same size
# ... ~2 trios of events share the same size
rates_for_resp |>
  dplyr::group_by(event_id) |>
  dplyr::slice(1L) |>
  dplyr::pull(size_len) |>
  table() |>
  table() |>
  as.numeric() / length(unique(rates_for_resp$size_len))
# For both variables, 74 % of events are uniquely defined by their size

#### Period of observations
# There are only few observations after more than 20 minutes
hist(rates$time_from_deck_to_obs)
# Examine evidence for change during handling
ggplot() +
  geom_point(aes(x = time_from_deck_to_obs, y = resp), data = rates_for_resp) +
  facet_wrap(~event_id)


#########################
#########################
#### Modelling

#########################
#### Model fitting

#### Implement model(s)
fam <- nb()
mod_1 <- gam(
  resp ~
    sex + size_len +
    temp_water * time_from_capture_to_surface +
    time_from_surface_to_deck +
    gaff + surgery + 
    s(event_id, bs = "re") +
    s(event_id, time_from_deck_to_obs, bs = "re"),
  family = fam,
  data = rates_for_resp,
  method = "REML"
)
mod_2 <- gam(
  resp ~
    sex + temp_water * time_from_capture_to_surface +
    time_from_surface_to_deck +
    gaff + surgery + 
    te(size_len, time_from_deck_to_obs),
  family = fam,
  data = rates_for_resp,
  method = "REML"
)

#### Quick diagnostics
if (FALSE) {
  pp <- par(mfrow = c(2, 2))
  gam.check(mod_1, rep = 1000, type = "deviance")
  plot(DHARMa::simulateResiduals(mod_1))
  par(pp)
  stop("Checked!")
}


#########################
#### Model comparison and summary

#### Model comparison
(ranks <- AIC(mod_1, mod_2))
message(round(max(ranks$AIC) - min(ranks$AIC), digits = 2))
message(rownames(ranks)[which.min(ranks$AIC)])
mod <- get(rownames(ranks)[which.min(ranks$AIC)])
mod <- mod_1
# hr: delta AIC = 16.24
# rr: delta AIC = 111.96

#### Model summary
summary(mod, digits = 3)
con <- paste0("./fig/", resp, "_coef.txt")
sink(con)
summary(mod, digits = 3)
sink()


#########################
#### Model predictions

#### Model smooths
plot(mod, all.terms = TRUE, pages = 1, scheme = 1)

#### Estimate the difference in vital rates for the smallest versus largest female
ind_f <- rates_for_resp$sex == "F"
predict(mod,
  newdata = data.frame(
    sex = factor("F", levels = c("F", "M")),
    size_len = c(
      min(rates_for_resp$size_len[ind_f]),
      max(rates_for_resp$size_len[ind_f])
    ),
    time_from_capture_to_surface = median(rates_for_resp$time_from_capture_to_surface),
    temp_water = median(rates_for_resp$temp_water),
    time_from_surface_to_deck = median(rates_for_resp$time_from_surface_to_deck),
    gaff = factor("N", levels = c("N", "Y")),
    surgery = factor("Y", levels = c("N", "Y")),
    event_id = factor(levels(rates_for_resp$event_id)[1],
      levels = levels(rates_for_resp$event_id)
    ),
    time_from_deck_to_obs = min(rates_for_resp$time_from_deck_to_obs)
  ),
  type = "response",
  se.fit = TRUE
) |>
  list_CIs()

#### Visualise model predictions for each variable

## Set up figure to save
if (save) {
  png(paste0("./fig/", resp, ".png"),
    height = 4.5, width = 10, units = "in", res = 600
  )
}
# pp <- par(mfrow = c(2, 4), oma = c(2, 3, 2, 2), mar = rep(2, 4))
pp <- par(mfrow = c(2, 4), oma = c(2, 3, 2, 2), mar = c(2.1, 2, 2.1, 2))

## Define prediction constants
# Where possible, we use the same values as for the physio models
constants <- readRDS(here_data("helper", "constants.rds"))
constants <- constants[, c("sex", "size_len", "temp_water", "time_from_capture_to_surface", 
                           "gaff", "surgery")]
constants$time_from_surface_to_deck <- 1  # median across all IDs
constants$time_from_deck_to_obs     <- 11 # median across all IDs
constants$event_id <- factor(10, levels = levels(rates$event_id))

## Define graphical param
# Define data used for model fitting
rates_in_mod <- model.frame(mod)
# Check the number of observations during time on deck for each event ID
rates_in_mod |> 
  group_by(event_id) |> 
  summarise(n = n()) |> 
  pull(n) |> 
  utils.add::basic_stats()
# Define titles
xlabs <- c(
  "Sex",
  "Size [cm]",
  expression("Temperature [" * degree * "C]"),
  expression("Time (hook" %->% "surface) [mins]"),
  expression("Time (surface" %->% "deck) [mins]"),
  "Gaff",
  "Surgery",
  expression("Time (deck" %->% "observation) [mins]")
)
# Define title param
xlab_line <- 2.25
main_adj <- 0
main_font <- 2
# Define point colours, shapes and sizes
jt <- 0.2
pt_cex_adj <- 0.5
pt_param$cex <- 1
pt_param$col <- pt_cols_id$col[findInterval(as.integer(model.frame(mod)$event_id), pt_cols_id$breaks)]
pt_param$bg  <- pt_param$col
stopifnot(!any(is.na(pt_param$col)))
# Adjust error bar parameters
ebars_param$lwd <- 2.5
ebars_param$add_fit$pch <- "+"
ebars_param$add_fit$lwd <- 1.25
ebars_param$col <- scales::alpha("grey50", 0.95)

## Plot predictions for sex
pretty_predictions_1d(
  model = mod,
  x_var = c("sex", "size_len"),
  constants = constants,
  add_xlab = list(text = xlabs[1:2], line = xlab_line),
  add_ylab = NULL,
  add_main = list(text = c("A", "B"), adj = main_adj, font = main_font),
  add_error_bars = ebars_param,
  add_error_envelope = eenv_param,
  add_points = pt_param,
  add_points_jitter = list(sex = c(jt, 0)),
  add_order = list(sex = c("points", "predictions"), 
                   size_len = c("predictions", "points")),
  one_page = FALSE
)

## Plot predictions for temperature (by fight time)
# Define prediction data
p_n <- 100
p_x <- seq(min(rates_for_resp$temp_water),
  max(rates_for_resp$temp_water),
  length.out = p_n
)
p_d <- lapply(seq_len(length(p_x)), \(i) constants) |> dplyr::bind_rows()
p_d$time_from_capture_to_surface <- min(rates_for_resp$time_from_capture_to_surface)
p_d$temp_water <- p_x
# Plot predictions for effects of temperature when fight time is low
pretty_predictions_1d(
  model = mod,
  newdata = p_d,
  x_var = "temp_water",
  add_points = NULL,
  add_error_envelope =
    list(
      add_fit = rlist::list.merge(eenv_param$add_fit, list(col = "royalblue")),
      add_ci = rlist::list.merge(eenv_param$add_ci, list(col = scales::alpha("skyblue", 0.2))
      )
    ),
  add_xlab = list(text = xlabs[3], line = xlab_line),
  add_ylab = NULL,
  add_main = list(text = "C", adj = main_adj, font = main_font)
)
# Add predictions for effects of temperature when fight time is high
p_d$time_from_capture_to_surface <- max(rates_for_resp$time_from_capture_to_surface)
p_ci <- list_CIs(predict(mod, p_d, se.fit = TRUE, type = "response"))
add_error_envelope(p_d$temp_water,
  p_ci,
  add_fit = rlist::list.merge(eenv_param$add_fit, list(col = "darkred")),
  add_ci = rlist::list.merge(eenv_param$add_ci, 
                             list(col = scales::alpha("red", 0.2)))
)
add_pt <- pt_param
add_pt$x <- rates_for_resp$temp_water
add_pt$y <- rates_for_resp$resp
add_pt$cex <- rates_for_resp$time_from_capture_to_surface / max(rates$time_from_capture_to_surface, na.rm = TRUE) + pt_cex_adj
add_pt$col <- pt_cols_ft$col[findInterval(rates_for_resp$time_from_capture_to_surface, pt_cols_ft$breaks)]
add_pt$bg  <- add_pt$col
stopifnot(!any(is.na(add_pt$col)))
px <- par(xpd = NA)
do.call(points, add_pt)
par(px)
legend_adj <- 0.1
legend("top",
       lty = c(1, 1),
       col = c("royalblue", "darkred"),
       lwd = c(1.5, 1.5),
       legend = c(
         expression(E(T* ":" ~ FT[L])),
         expression(E(T* ":" ~ FT[H]))
       ),
       adj = legend_adj,
       bg = scales::alpha("white", 0.5), box.lwd = 0.5, box.lty = 3,
       y.intersp = 1.2
)

## Plot predictions for fight time (by temperature)
# Define prediction data
p_n <- 100
p_x <- seq(min(rates_for_resp$time_from_capture_to_surface),
           max(rates_for_resp$time_from_capture_to_surface),
           length.out = p_n
)
p_d <- lapply(seq_len(length(p_x)), \(i) constants) |> dplyr::bind_rows()
p_d$time_from_capture_to_surface <- p_x
p_d$temp_water <- min(rates_for_resp$temp_water)
# Plot predictions for effects of fight time in low temperatures
pretty_predictions_1d(
  model = mod,
  newdata = p_d,
  x_var = "time_from_capture_to_surface",
  add_points = NULL,
  add_error_envelope =
    list(
      add_fit = rlist::list.merge(eenv_param$add_fit, list(col = "royalblue")),
      add_ci = rlist::list.merge(eenv_param$add_ci, list(col = scales::alpha("skyblue", 0.2))
      )
    ),
  add_xlab = list(text = xlabs[4], line = xlab_line),
  add_ylab = NULL,
  add_main = list(text = "D", adj = main_adj, font = main_font)
)
# Add predictions for effects of fight time in warm temperatures
p_d$temp_water <- max(rates_for_resp$temp_water)
p_ci <- list_CIs(predict(mod, p_d, se.fit = TRUE, type = "response"))
add_error_envelope(p_d$time_from_capture_to_surface,
  p_ci,
  add_fit = rlist::list.merge(eenv_param$add_fit, list(col = "darkred")),
  add_ci = rlist::list.merge(eenv_param$add_ci, 
                             list(col = scales::alpha("red", 0.2)))
)
add_pt <- pt_param
add_pt$x <- rates_for_resp$time_from_capture_to_surface
add_pt$y <- rates_for_resp$resp
add_pt$cex <- rates_in_mod$temp_water / max(rates$temp_water, na.rm = TRUE) + 0
  
add_pt$col <- pt_cols_temp$col[findInterval(rates_for_resp$temp_water, pt_cols_temp$breaks)]
add_pt$bg  <- add_pt$col
stopifnot(!any(is.na(add_pt$col)))
add_pt$bg <- add_pt$col
px <- par(xpd = NA)
do.call(points, add_pt)
par(px)
do.call(points, add_pt)
legend("topright",
       lty = c(1, 1),
       col = c("royalblue", "darkred"),
       lwd = c(1.5, 1.5),
       legend = c(
         expression(E(T* ":" ~ FT[L])),
         expression(E(T* ":" ~ FT[H]))
       ),
       adj = legend_adj,
       bg = scales::alpha("white", 0.5), box.lwd = 0.5, box.lty = 3,
       y.intersp = 1.2
)

## Plot predictions for surface time, gaff and and handling time
pretty_predictions_1d(
  model = mod,
  x_var = c("time_from_surface_to_deck", "gaff", "surgery", "time_from_deck_to_obs"),
  constants = constants,
  add_error_bars = ebars_param,
  add_error_envelope = eenv_param,
  add_points = pt_param,
  add_points_jitter = list(gaff = c(jt, 0), surgery = c(jt, 0)),
  add_order = list(
    time_from_surface_to_deck = c("predictions", "points"),
    gaff = c("points", "predictions"), 
    surgery = c("points", "predictions"),
    time_from_deck_to_obs = c("predictions", "points")
  ),
  add_xlab = list(text = xlabs[5:8], line = xlab_line),
  add_ylab = NULL,
  add_main = list(text = c("E", "F", "G", "H"), adj = main_adj, font = main_font),
  one_page = FALSE
)

## Save
mtext(side = 2, ylab, line = 1, outer = TRUE, cex = 1)
par(pp)
if (save) dev.off()
# open(paste0("./fig/", resp, ".png"))
# stop()

## Legends
## Add legends
if (TRUE) {
  png("./fig/colour-bars.png", 
      height = 5, width = 5, units = "in", res = 600)
  cex.axis <- 1.2
  # Individual colour scale
  x <- zoo::rollmean(pt_cols_id$breaks, 2)
  plot(0, type = "n", 
       xlim = c(0, 10), ylim = c(0, 10), 
       axes = FALSE, xlab = "", ylab = "")
  TeachingDemos::subplot(
    add_colour_bar(data.frame(x = x, col = pt_cols_id$col), 
                   pretty_axis_args = pretty_axis(side = 2, 
                                                  lim = list(range(x)),
                                                  control_axis = list(pos = 0, las = FALSE, cex.axis = cex.axis),
                                                  add = FALSE)
    ), 
    x = c(0, 0.5), 
    y = c(-0.5, 10)
  )
  #mtext(side = 4, "Event", line = -10)
  # Fight time colour scale
  x <- zoo::rollmean(pt_cols_ft$breaks, 2)
  TeachingDemos::subplot(
    add_colour_bar(data.frame(x = x, col = pt_cols_ft$col), 
                   pretty_axis_args = pretty_axis(side = 2, 
                                                  lim = list(range(x)),
                                                  control_axis = list(pos = 0, las = FALSE, cex.axis = cex.axis),
                                                  add = FALSE)
    ),
    x = c(4.5, 5), 
    y = c(-0.5, 10)
  )
  # mtext(side = 4,  expression(E(FT* ":" ~ T) ~ "[mins]"), line = -3.5)
  # Temperature colour scale 
  x <- zoo::rollmean(pt_cols_temp$breaks, 2)
  TeachingDemos::subplot(
    add_colour_bar(data.frame(x = x, col = pt_cols_temp$col), 
                   pretty_axis_args = pretty_axis(side = 2, 
                                                  lim = list(range(x)),
                                                  control_axis = list(pos = 0, las = FALSE, cex.axis = cex.axis),
                                                  add = FALSE)
    ), 
    x = c(9, 9.5), 
    y = c(-0.5, 10)
  )
  #mtext(side = 4,  expression(E(T* ":" ~ FT)~ "[" * degree * "C]"), line = 2) 
  dev.off()
}


#### Visualise predictions through time for each event
## Set up plot to save
if (save) {
  png(paste0("./fig/", resp, "_time_series.png"),
  height = 12, width = 12, units = "in", res = 800
)
}

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
pp <- par(
  mfrow = dim,
  oma = c(6, 6, 2, 2), mar = c(0, 0, 0, 0),
  xaxs = "i", yaxs = "i",
  mgp = c(3, 0.5, 0.5)
)

## Make plots
lapply(1:length(rates_for_resp_by_event), function(i) {
  # Isolate data for event
  # i <- 1
  rate <- rates_for_resp_by_event[[i]]
  # Make predictions
  pred <- predict(mod, newdata = rate, se.fit = TRUE, type = "response")
  pred <- list_CIs(pred)
  # Define axis limits
  use_dynamic_limits <- FALSE
  if (use_dynamic_limits) {
    paa <- list(
      side = 1:4,
      pretty = list(list(n = 3), list(n = 5)),
      control_axis = list(tck = 0.02, las = TRUE, cex.axis = cex.axis),
      x = list(
        x = range(rate$time_from_deck_to_obs),
        y = range(c(rate$resp, pred$lowerCI, pred$upperCI))
      )
    )
  } else {
    paa <- list(
      side = 1:4,
      pretty = list(list(n = 3), list(n = 5)),
      control_axis = list(tck = 0.02, las = TRUE, cex.axis = cex.axis),
      x = list(
        x = range(rates_for_resp$time_from_deck_to_obs),
        y = range(rates_for_resp$resp)
      )
    )
  }
  paa$add <- FALSE
  axis_ls <- do.call(pretty_axis, paa)
  xlim <- axis_ls[[1]]$lim
  ylim <- axis_ls[[2]]$lim
  # Create blank plot
  plot(rate$time_from_deck_to_obs, rate$resp,
    xlim = xlim, ylim = ylim,
    axes = FALSE,
    xlab = "", ylab = "",
    type = "n"
  )
  # Add predictions and observations
  add_error_envelope(rate$time_from_deck_to_obs, pred)
  points(rate$time_from_deck_to_obs, rate$resp)
  # Add axes
  # rect(xlim[1], ylim[1], xlim[2], ylim[2])
  axis_ls[[1]]$axis$labels[length(axis_ls[[1]]$axis$labels)] <- ""
  if (!(i %in% ind_for_x)) axis_ls[[1]]$axis$labels <- FALSE
  axis_ls[[3]]$axis$labels <- FALSE
  # axis_ls[[3]]$axis$lwd.tick <- 0
  do.call(graphics::axis, axis_ls[[1]]$axis)
  do.call(graphics::axis, axis_ls[[3]]$axis)
  axis_ls[[2]]$axis$labels[length(axis_ls[[2]]$axis$labels)] <- ""
  if (!(i %in% ind_for_y)) axis_ls[[2]]$axis$labels <- FALSE
  axis_ls[[4]]$axis$labels <- FALSE
  # axis_ls[[4]]$axis$lwd.tick <- 0
  do.call(graphics::axis, axis_ls[[2]]$axis)
  do.call(graphics::axis, axis_ls[[4]]$axis)
  mtext(side = 3, text = paste0("[", rate$event_id_int, "]"), line = -2, font = main_font)
}) |> invisible()
mtext(
  side = 1, expression("Time (deck" %->% "observation) [mins]"),
  line = 4, outer = TRUE, cex = 1.5
)
mtext(side = 2, ylab, line = 4, outer = TRUE, cex = 1.5)
par(pp)
if (save) dev.off()


#########################
#### Model diagnostics

#### Standard residual diagnostics
if (save) {
  png(paste0("./fig/", resp, "_diag.png"),
    height = 12, width = 12, units = "in", res = 800
  )
}
pp <- par(mfrow = c(2, 2))
gam.check(mod, rep = 1000, type = "deviance")
par(pp)
if (save) dev.off()

#### DHARMa residuals
plot(DHARMa::simulateResiduals(mod))


#### End of code.
#########################
#########################