#########################
#########################
#### analyse_bloods.R

#### Aims
# 1) Analyses skate blood parameters.

#### Prerequisites
# 1) Define global parameters (define_global_param.R)
# 2) Process bloods           (process_bloods.R)


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
physio   <- readRDS("./data/skate/physio.rds")
captures <- readRDS("./data/skate/capture_events.rds")

#### Define local parameters
# Define whether or not to save figures
save <- TRUE
set.seed(1)


#########################
#########################
#### Data processing

#### Define the times of key events
nrow(physio)
all(physio$pit %in% captures$pit)
physio$key <- paste0(physio$pit, "-", physio$date)
captures$key <- paste0(captures$pit, "-", captures$date)
all(physio$key %in% captures$key)
physio$event_id <- captures$event_id[match(physio$key, captures$key)]
physio$time_surface <- captures$time_surface[match(physio$event_id, captures$event_id)]
physio$time_deck <- captures$time_deck[match(physio$event_id, captures$event_id)]
physio$time_from_surface_to_deck <- as.integer(difftime(physio$time_deck,
                                                        physio$time_surface,
                                                        units = "mins"
))
utils.add::basic_stats(physio$time_from_surface_to_deck)

#### Define dataset
# Focus on healthy or unhealthy individuals
# Choose whether or not to include individuals with uncertain parameters
# ... for the responses (e.g., associated with <) or explanatory variables (e.g., gaffing)
physio <- physio[physio$healthy == 1, ]
# Distinguish tagged/non tagged individuals for BS2 models
physio$vemco[physio$surgery == "N"]
physio$vemco[physio$surgery == "Y"]

#### Define response variable/sample
# "pH"   "PCO2" "PO2"  "HCO3" "lac"  "glu"  "K"    "Mg"
yvar <- "pH"
sample <- "3"
# lapply(resps, function(yvar) {
#   lapply(c("1", "2", "3"), function(sample) {

resp <- paste0(yvar, "_", sample)

#### Focus on specific columns
covars <- c(
  "sex", "age", "size_len",
  "time_from_capture_to_surface",
  "time_from_surface_to_bs1", "time_from_capture_to_bs1",
  "time_from_surface_to_bs2", "time_from_capture_to_bs2",
  "time_from_bs1_to_bs2",
  "temp_water", "gaff", "surgery"
)
physior <- physio[, colnames(physio) %in% c(covars, resp)]
physior$resp <- physio[, resp]


#########################
#########################
#### Data exploration

#### Distribution of response
utils.add::basic_stats(physior[, resp], na.rm = TRUE)

#### Correlations
# Implemented below. 


#########################
#########################
#### Implement modelling

#### Model formulae
if (sample == 1) {
  lf <- "log"
  form_1 <- resp ~
    sex +
    size_len +
    temp_water *
    time_from_capture_to_surface + time_from_surface_to_bs1 +
    gaff
} else if (sample == 2) {
  lf <- "log"
  form_1 <- resp ~
    sex +
    size_len +
    temp_water *
    time_from_capture_to_surface + time_from_surface_to_bs2 +
    gaff + surgery
} else if (sample == 3) {
  # Include time between blood samples
  lf <- "identity"
  form_1 <- resp ~
    sex +
    size_len +
    temp_water *
    time_from_capture_to_surface + time_from_surface_to_bs1 + time_from_bs1_to_bs2 + 
    gaff + surgery
  # Confirm times 'add up' as expected
  stopifnot(all(physio$time_from_surface_to_bs1 + physio$time_from_bs1_to_bs2 == 
                  physio$time_from_surface_to_bs2, 
                na.rm = TRUE))
}

#### Correlations
str(physior[, all.vars(form_1)])
pretty_pairs(physior[, all.vars(form_1)])

#### Model fitting
mod <- glm(form_1, family = gaussian(link = lf), data = physior)
saveRDS(mod, here_data("models", sample, paste0(resp, ".rds")))


#########################
#########################
#### Model summaries

#### Model summary (raw)
# raw number of observations (incl NA) versus number used for model fitting:
nrow(physior)
nrow(model.frame(mod))
# model summary
summary(mod)
# deviance explained
# utils.add::dev_expl(mod)
car::vif(mod)

#### Model summary (tidy)
if (sample == 1) {
  coef_names <- c(
    "Intercept",
    "Sex (M)",
    "Size",
    "Temperature",
    "Time (hook → surface)",
    "Time (surface → BS1)",
    "Gaff (Y)",
    "Temperature: Time (hook → surface)"
  )
} else if (sample == 2) {
  coef_names <- c(
    "Intercept",
    "Sex (M)",
    "Size",
    "Temperature",
    "Time (hook → surface)",
    "Time (surface → BS2)",
    "Gaff (Y)",
    "Surgery (Y)",
    "Temperature: Time (hook → surface)"
  )
} else if (sample == 3) {
  coef_names <- c(
    "Intercept",
    "Sex (M)",
    "Size",
    "Temperature",
    "Time (hook → surface)",
    "Time (surface → BS1)",
    "Time (BS1 → BS2)",
    "Gaff (Y)",
    "Surgery (Y)",
    "Temperature: Time (hook → surface)"
  )
}
coef_tbl <-
  utils.add::tidy_coef(
    coef = coef(summary(mod)),
    coef_names = coef_names,
    col_names = c("Coefficient", "Estimate", "SE", "t-value", "p-value")
  )
tidy_write(coef_tbl, paste0("./fig/", resp, "_coef.txt"))


#########################
#########################
#### Model predictions

#### Quick plot
# jit <- c(0.3, 0)
jit <- c(0, 0)
cowplot::plot_grid(plotlist = 
                     plot(ggeffects::ggpredict(mod), 
                          add.data = TRUE, jitter = jit, dot.size = 0.9, dot.alpha = 2, ci = TRUE)
)

#### Define prediction constants
# This fixes constants across all plots
constants <- 
  data.frame(time_from_capture_to_surface = median(physio$time_from_capture_to_surface, na.rm = TRUE), 
             temp_water                   = median(physio$temp_water, na.rm = TRUE),
             sex                          = factor("F", levels = c("F", "M")), 
             size_len                     = median(physio$size_len, na.rm = TRUE), 
             time_from_surface_to_bs1     = median(physio$time_from_surface_to_bs1, na.rm = TRUE), 
             time_from_surface_to_bs2     = median(physio$time_from_surface_to_bs2, na.rm = TRUE), 
             time_from_bs1_to_bs2         = median(physio$time_from_bs1_to_bs2, na.rm = TRUE), 
             surgery                      = factor("Y", levels = c("N", "Y")), 
             gaff                         = factor("N", levels = c("N", "F")), 
             time_index                   = 1)
saveRDS(constants, here_data("helper", "constants.rds"))

#### Visualise model predictions for each variable
## Set up figure to save
if (sample %in% c(1, 2)) {
  height <- 4.5; width <- 10
  mf <- c(2, 4)
} else {
  height <- 7.5; width <- 9
  mf <- c(3, 3)
}
if (save) {
  png(paste0("./fig/", resp, "_preds.png"),
      height = height, width = width, units = "in", res = 600)
}
# pp <- par(mfrow = mf, oma = c(2, 2, 2, 2), mar = rep(2.5, 4))
pp <- par(mfrow = mf, oma = c(2, 3, 2, 2), mar = c(2.1, 2, 2.1, 2))

## Define graphical param
# Define data used for model fitting
physio_in_mod <- model.frame(mod)
# Get appropriate y limits
if (sample %in% c(1, 2)) {
  ylim <- ylims[[substr(resp, 1, nchar(resp) - 2)]]
} else {
  ylim <- ylims_3[[substr(resp, 1, nchar(resp) - 2)]]
}

# Define titles
if (sample == 1) {
  xlabs <- c(
    "Sex",
    "Size [cm]",
    expression("Temperature [" * degree * "C]"),
    expression("Time (hook" %->% "surface) [mins]"),
    expression("Time (surface" %->% "BS1) [mins]"),
    "Gaff"
  )
} else if (sample == 2) {
  xlabs <- c(
    "Sex",
    "Size [cm]",
    expression("Temperature [" * degree * "C]"),
    expression("Time (hook" %->% "surface) [mins]"),
    expression("Time (surface" %->% "BS2) [mins]"),
    "Gaff", 
    "Surgery"
  )
} else if (sample == 3) {
  xlabs <- c(
    "Sex",
    "Size [cm]",
    expression("Temperature [" * degree * "C]"),
    expression("Time (hook" %->% "surface) [mins]"),
    expression("Time (surface" %->% "BS1) [mins]"),
    expression("Time (BS1" %->% "BS2) [mins]"),
    "Gaff", 
    "Surgery"
  )
}
# Define title param
xlab_line <- 2.25
main_adj <- 0
main_font <- 2
# Define point colours, shapes and sizes
pt_cex_adj <- 0.5
jt <- 0.2
jt_param <-
  list(sex = c(jt, 0), gaff = c(jt, 0), surgery = c(jt, 0))
jt_param <- jt_param[names(jt_param) %in% all.vars(form_1)]
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
  add_order = list(sex = c("points", "predictions"), 
                   size_len = c("predictions", "points")),
  ylim = ylim,
  add_xlab = list(text = xlabs[1:2], line = xlab_line),
  add_ylab = NULL,
  add_main = list(text = c("A", "B"), adj = main_adj, font = main_font),
  add_error_bars = ebars_param,
  add_error_envelope = eenv_param,
  add_points = pt_param,
  add_points_jitter = jt_param,
  one_page = FALSE
)

## Plot predictions for temperature (by fight time)
# Define prediction data
p_n <- 100
p_x <- seq(min(physio_in_mod$temp_water),
           max(physio_in_mod$temp_water),
           length.out = p_n)
p_d <- lapply(seq_len(length(p_x)), \(i) constants) |> dplyr::bind_rows()
p_d$time_from_capture_to_surface <- min(physio$time_from_capture_to_surface)
p_d$temp_water <- p_x
# Plot predictions for effects of temperature when fight time is low
pretty_predictions_1d(
  model = mod,
  newdata = p_d,
  x_var = "temp_water",
  ylim = ylim,
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
p_d$time_from_capture_to_surface <- max(physio$time_from_capture_to_surface, na.rm = TRUE)
p_ci <- list_CIs(predict(mod, p_d, se.fit = TRUE, type = "response"))
add_error_envelope(p_d$temp_water,
                   p_ci,
                   add_fit = rlist::list.merge(eenv_param$add_fit, list(col = "darkred")),
                   add_ci = rlist::list.merge(eenv_param$add_ci, 
                                              list(col = scales::alpha("red", 0.2))
                   )
)
add_pt <- pt_param
add_pt$x <- physio_in_mod$temp_water
add_pt$y <- physio_in_mod$resp
add_pt$cex <- physio_in_mod$time_from_capture_to_surface / max(physio$time_from_capture_to_surface, na.rm = TRUE) + pt_cex_adj
add_pt$col <- pt_cols_ft$col[findInterval(physio_in_mod$time_from_capture_to_surface, pt_cols_ft$breaks)]
stopifnot(!any(is.na(add_pt$col)))
add_pt$bg <- add_pt$col
px <- par(xpd = NA)
do.call(points, add_pt)
par(px)
legend_pos <- "topright"
# legend_pos <- "topleft"
# if (resp %in% c("pH_1", "HCO3_1", "pH_2")) legend_pos <- "bottomleft"
legend_adj <- 0.1
legend(legend_pos,
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
p_x <- seq(min(physio_in_mod$time_from_capture_to_surface),
           max(physio_in_mod$time_from_capture_to_surface),
           length.out = p_n)
p_d <- lapply(seq_len(length(p_x)), \(i) constants) |> dplyr::bind_rows()
p_d$time_from_capture_to_surface <- p_x
p_d$temp_water <- min(physio$temp_water, na.rm = TRUE)
# Plot predictions for effects of fight time in low temperatures
pretty_predictions_1d(
  model = mod,
  newdata = p_d,
  x_var = "time_from_capture_to_surface",
  ylim = ylim,
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
p_d$temp_water <- max(physio$temp_water, na.rm = TRUE)
p_ci <- list_CIs(predict(mod, p_d, se.fit = TRUE, type = "response"))
add_error_envelope(p_d$time_from_capture_to_surface,
                   p_ci,
                   add_fit = rlist::list.merge(eenv_param$add_fit, list(col = "darkred")),
                   add_ci = rlist::list.merge(eenv_param$add_ci, 
                                              list(col = scales::alpha("red", 0.2))
                   )
)
add_pt <- pt_param
add_pt$x <- physio_in_mod$time_from_capture_to_surface
add_pt$y <- physio_in_mod$resp
add_pt$cex <- physio_in_mod$temp_water / max(physio$temp_water, na.rm = TRUE) + pt_cex_adj
add_pt$col <- pt_cols_temp$col[findInterval(physio_in_mod$temp, pt_cols_temp$breaks)]
stopifnot(!any(is.na(add_pt$col)))
add_pt$bg  <- add_pt$col
do.call(points, add_pt)
legend_adj <- 0.2
# if (resp %in% c("PCO2_2")) legend_pos <- "topright"
legend(legend_pos,
       lty = c(1, 1),
       col = c("royalblue", "darkred"),
       legend = c(expression(E(FT* ":" ~ T[L])), 
                  expression(E(FT* ":" ~ T[H]))),
       adj = legend_adj,
       bg = scales::alpha("white", 0.5), box.lwd = 0.5, box.lty = 3,
       y.intersp = 1.2
)

## Plot predictions for handling time, gaff and surgery
x_var_time <- colnames(physio_in_mod)[
  stringr::str_detect(colnames(physio_in_mod), "time_from_capture")]
add_order <- 
  setNames(list(c("predictions", "points"), 
                c("predictions", "points"),
                c("points", "predictions"), 
                c("points", "predictions")), 
           c(x_var_time, "time_from_bs1_to_bs2", "gaff", "surgery"))
x_var <- c(x_var_time, "time_from_bs1_to_bs2", "gaff", "surgery")
x_var <- x_var[x_var %in% all.vars(form_1)]
pos <- 5:(5 + length(x_var) - 1)
x_var <- setNames(x_var, LETTERS[pos])
pretty_predictions_1d(
  model = mod,
  x_var = x_var,
  constants = constants,
  add_order = add_order,
  ylim = ylim,
  add_error_bars = ebars_param,
  add_error_envelope = eenv_param,
  add_points = pt_param,
  add_points_jitter = jt_param,
  add_xlab = list(text = xlabs[pos], line = xlab_line),
  add_ylab = NULL,
  add_main = list(text = names(x_var), adj = main_adj, font = main_font),
  one_page = FALSE
)

## Add legends 
# Fight time colour scale
x <- zoo::rollmean(pt_cols_ft$breaks, 2)
plot(0, type = "n", 
     xlim = c(0, 10), ylim = c(0, 10), 
     axes = FALSE, xlab = "", ylab = "")
TeachingDemos::subplot(
  add_colour_bar(data.frame(x = x, col = pt_cols_ft$col), 
                 pretty_axis_args = pretty_axis(side = 4, 
                                                lim = list(range(x)),
                                                control_axis = list(pos = 1, las = TRUE),
                                                add = FALSE)
  ), 
  x = c(0, 1), 
  y = c(-0.5, 10)
)
mtext(side = 4,  expression(E(FT* ":" ~ T) ~ "[mins]"), line = -8.75)
# Temperature colour scale 
x <- zoo::rollmean(pt_cols_temp$breaks, 2)
TeachingDemos::subplot(
  add_colour_bar(data.frame(x = x, col = pt_cols_temp$col), 
                 pretty_axis_args = pretty_axis(side = 4, 
                                                lim = list(range(x)),
                                                control_axis = list(pos = 1, las = TRUE),
                                                add = FALSE)
  ), 
  x = c(6, 7), 
  y = c(-0.5, 10)
)
mtext(side = 4,  expression(E(T* ":" ~ FT)~ "[" * degree * "C]"), line = -1)

## Global titles
if (sample %in% c(1, 2)) {
  ylab <- ylabs[[substr(resp, 1, nchar(resp) - 2)]]
} else if (sample == 3) {
  ylab <- ylabs_3[[substr(resp, 1, nchar(resp) - 2)]]
}
mtext(side = 2, ylab, line = 1, outer = TRUE)
par(pp)
if (save) dev.off()
# open(paste0("./fig/", resp, "_preds.png"))
# stop("Done!")


#########################
#########################
#### Model diagnostics

#### Model residuals
# Standard residuals 
if (save) {
  png(paste0("./fig/", resp, "_diagnostics.png"),
      height = 5.5, width = 9, units = "in", res = 600
  )
}
pp <- par(mfrow = c(1, 2))
# car::qqPlot(mod, line = "none", rep = 1e3)
plot(mod, 1:2)
par(pp)
if (save) dev.off()
# DHARMA
if (save) {
  png(paste0("./fig/", resp, "_diagnostics_dharma.png"),
      height = 5.5, width = 9, units = "in", res = 600
  )
}
plot(DHARMa::simulateResiduals(mod))
if (save) dev.off()

#### Predictive accuracy
physio_in_mod$x <- factor(1:nrow(physio_in_mod))
ps <- list_CIs(predict(mod, type = "response", se.fit = TRUE))
pretty_plot(physio_in_mod$x, physio_in_mod$resp,
            pretty_axis_args =
              list(
                side = 1:2,
                x = list(
                  x = range_factor(physio_in_mod$x),
                  y = range(
                    c(
                      physio_in_mod$resp,
                      ps$lowerCI,
                      ps$upperCI
                    ),
                    na.rm = TRUE
                  )
                )
              ),
            type = "n",
            main = resp
)
add_error_bars(physio_in_mod$x, fit = ps$fit, lwr = ps$lowerCI, upr = ps$upperCI)
points(physio_in_mod$x, physio_in_mod$resp, col = "red")


#########################
#########################
#### Collective model tables

#### Define a summary table for blood parameters (for BS1 and BS2)
## This contains the number of observations, alongside summary
# ... statistics of each distribution
summaries <-
  lapply(c(paste0(resps, "_1"), paste0(resps, "_2"), paste0(resps, "_3")), function(resp) {
    physio$resp <- physio[, resp]
    pos <- which(!is.na(physio$resp))
    n_obs <- length(pos)
    stat_min <- min(physio$resp, na.rm = TRUE)
    stat_med <- median(physio$resp, na.rm = TRUE)
    stat_max <- max(physio$resp, na.rm = TRUE)
    # mod <- lm(form_1, data = physio)
    # n_mod <- nrow(model.frame(mod))
    data.frame(
      Parameter = substr(resp, 1, nchar(resp) - 2),
      Sample = substr(resp, nchar(resp), nchar(resp)),
      Nobs = n_obs,
      # Nmod = n_mod,
      Min = stat_min,
      Med = stat_med,
      Max = stat_max
    )
  }) |> dplyr::bind_rows()
## Tidy summaries
summaries <-
  summaries |>
  tidyr::pivot_wider(
    names_from = Sample,
    values_from = c(Nobs, Min, Med, Max)
  )
colnames(summaries) <-
  stringr::str_replace_all(colnames(summaries), "_1", " [1]")
colnames(summaries) <-
  stringr::str_replace_all(colnames(summaries), "_2", " [2]")
colnames(summaries) <-
  stringr::str_replace_all(colnames(summaries), "_3", " [D]")
summaries$Parameter <-
  resps_names$name[match(summaries$Parameter, resps_names$resp)]
## Examine the range in the number of observations across parameters for BS1/2
utils.add::basic_stats(summaries$`Nobs [1]`)
utils.add::basic_stats(summaries$`Nobs [2]`)
## Write tidy table to file
summaries <- tidy_numbers(summaries, digits = 2)
tidy_write(summaries, "./fig/blood_summaries.txt")

#### Define tidy coefficient tables derived from mod_1 for each parameter
coefs <-
  lapply(paste0(resps, "_", sample), function(resp) {
    # Fit model and get the number of observations used for model fitting
    # (... excluding K_2 and Mg_2 due to a lack of data)
    if (!(resp %in% resps_exclude)) {
      print(resp)
      physio$resp <- physio[, resp]
      mod <- glm(form_1, family = gaussian(link = lf), data = physio)
      # mod <- readRDS(here_data("models", sample, paste0(resp, ".rds")))
      n_mod <- nrow(model.frame(mod))
      # Extract tidy summary table
      coef_tbl <-
        utils.add::tidy_coef(
          coef = coef(summary(mod)),
          coef_names = coef_names,
          col_names = c(
            "Coefficient", "Estimate",
            "SE", "t-value", "p-value"
          )
        )
      # Add parameter names and deviance explained
      coef_tbl <-
        cbind(
          data.frame(
            Parameter =
              resps_names$name[match(
                substr(resp, 1, nchar(resp) - 2),
                resps_names$resp
              )],
            Nmod = n_mod
          ),
          coef_tbl,
          data.frame(D = utils.add::dev_expl(mod))
        )
      coef_tbl$D <- add_lagging_point_zero(round(coef_tbl$D, 3), 3)
      coef_tbl[2:nrow(coef_tbl), c("Parameter", "Nmod", "D")] <- NA
      return(coef_tbl)
    } else {
      return(NULL)
    }
  }) |> dplyr::bind_rows()
# Write tidy table of coefficients to file
tidy_write(coefs,
           paste0("./fig/blood_coefs_", sample, ".txt"),
           na = ""
)

#### Summary statistics for deviance explained
# ( -> comment out `coef_tbl$D <- add_lagging_point_zero(...)` above)
if (!inherits(coefs$D, "character")) {
  utils.add::basic_stats(coefs$D, na.rm = TRUE)
}
## BS1
# min  mean median   max    sd  IQR  MAD
# 10.06 41.79  42.03 66.33 17.85 20.7 21.8
## BS2
# min  mean median   max    sd   IQR   MAD
# 25.84 37.55  35.57 51.39 9.66 17.2 11.44

### Visual examination of coefs
# Add p-value stars to facilitate visual checking
coefs$star <- ""
coefs$star[as.numeric(coefs$`p-value`) <= 0.05] <- "*"
# View(coefs)


#  })
#})




#### End of code.
#########################
#########################