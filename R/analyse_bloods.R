################################
################################
#### analyse_bloods_synthesis.R

#### This script: 
# 1) Analyses skate blood parameters. 

#### Steps preceding this script: 
# 1) Define global parameters (define_global_param.R)
# 2) Process bloods           (process_bloods.R)


################################
################################
#### Set up

#### Wipe workspace and source essential packages and variables
source("./R/define_global_param.R")

#### Load data
physio <- readRDS("./data/skate/physio.rds")


#### Define local parameters
# Define whether or not to save figures
save <- TRUE


################################
################################
#### Data processing 

#### Define dataset
# Focus on healthy or unhealthy individuals
# Choose whether or not to include individuals with uncertain parameters 
# ... for the responses (e.g., associated with <) or explanatory variables (e.g., gaffing)
physio <- physio[physio$healthy == 1, ]

#### Define response variable/sample
# "pH"   "PCO2" "PO2"  "HCO3" "lac"  "glu"  "K"    "Mg"  
yvar   <- "lac"
sample <- "2"
resp   <- paste0(yvar, "_", sample)

#### Focus on specific columns
covars <- c("sex", "age", "size_len", 
            "time_from_capture_to_surface", 
            "time_from_surface_to_bs1", "time_from_capture_to_bs1",
            "time_from_surface_to_bs2", "time_from_capture_to_bs2",
            "time_from_bs1_to_bs2", 
            "temp_water", "gaff")
physior <- physio[, colnames(physio) %in% c(covars, resp)]
physior$resp <- physio[, resp]


################################
################################
#### Data exploration

#### Distribution of response 
utils.add::basic_stats(physior[, resp], na.rm = TRUE)

#### Variable correlations 
psych::pairs.panels(physior)


################################
################################
#### Implement modelling  

#### Model formulae 
if(sample == 1){
  form_1 <- resp ~
    sex + 
    size_len + 
    temp_water * 
    time_from_capture_to_surface + time_from_surface_to_bs1 + 
    gaff
  form_2 <- resp ~ 
    sex + 
    size_len + 
    temp_water +
    time_from_capture_to_surface + time_from_surface_to_bs1 + 
    gaff
  form_3 <- resp ~ 
    sex + 
    size_len + 
    temp_water +
    time_from_capture_to_bs1 + 
    gaff
} else if(sample == 2){
  form_1 <- resp ~
    sex + 
    size_len + 
    temp_water * 
    time_from_capture_to_surface + time_from_surface_to_bs2 + 
    gaff
  form_2 <- resp ~ 
    sex + 
    size_len + 
    temp_water +
    time_from_capture_to_surface + time_from_surface_to_bs2 + 
    gaff
  form_3 <- resp ~ 
    sex + 
    size_len + 
    temp_water +
    time_from_capture_to_bs2 + 
    gaff
}

#### Model fitting 
mod_1 <- glm(form_1, family = gaussian(link = "log"), data = physior)
mod_2 <- glm(form_2, family = gaussian(link = "log"), data = physior)
mod_3 <- glm(form_3, family = gaussian(link = "log"), data = physior)
summary(mod_1)
summary(mod_2)
summary(mod_3)


################################
################################
#### Model comparison/summaries

#### Model comparison
( ranks <- AIC(mod_1, mod_2) ) # ( ranks <- AIC(mod_1, mod_2, mod_3) )
message(round(max(ranks$AIC) - min(ranks$AIC), digits = 2))
message(rownames(ranks)[which.min(ranks$AIC)])
mod <- get(rownames(ranks)[which.min(ranks$AIC)])
mod <- mod_1

#### Model summary
# raw number of observations (incl NA) versus number used for model fitting:
nrow(physior); nrow(model.frame(mod))
# summary
summary(mod)
# deviance explained
# utils.add::dev_expl(mod)

#### Model summary (tidy)
coef_names <- c("Intercept", 
                "Sex (M)", 
                "Size", 
                "Temperature", 
                "Time (capture → surface)", 
                "Time (surface → sample)", 
                "Gaff (Y)", 
                "Temperature: Time (capture → surface)")
coef_tbl <- utils.add::tidy_coef(coef = coef(summary(mod)), 
                                 coef_names = coef_names, 
                                 col_names = c("Coefficient", "Estimate", "SE", "t-value", "p-value"))
tidy_write(coef_tbl, paste0("./fig/", resp, "_coef.txt"))


################################
################################
#### Model predictions 

#### Visualise model predictions for each variable 
## Set up figure to save
save <- TRUE
if(save) png(paste0("./fig/", resp, "_preds.png"), 
             height = 5.5, width = 9, units = "in", res = 600)
pp <- par(mfrow = c(2, 3), oma = c(2, 2, 2, 2), mar = rep(2.5, 4))

## Define graphical param
# Define data used for model fitting
physio_in_mod <- model.frame(mod)
# Get appropriate y limits
ylim <- ylims[[substr(resp, 1, nchar(resp) - 2)]]
# Define titles
xlabs <- c("Sex", 
           "Length [cm]", 
           expression("Temperature [" * degree * "C]"), 
           expression("Time (hook" %->% "surface) [mins]"), 
           expression("Time (surface" %->% "BS1) [mins]"),
           "Gaff"
           )
if(sample == 2){
  xlabs[3] <- expression("Time (surface" %->% "BS2) [mins]")
}
# Define title param
xlab_line <- 2.25
main_adj  <- 0
main_font <- 2
# Define point colours, shapes and sizes
physio_in_mod$pt_pch <- 21 
physio_in_mod$pt_cex <- 0.75
pt_param <- list(col = scales::alpha("black", 0.75),
                 cex = physio_in_mod$pt_cex, 
                 pch = physio_in_mod$pt_pch, 
                 lwd = 0.75)
# Adjust error bar parameters
ebars_param$lwd          <- 1.5
ebars_param$add_fit$lwd <- 1.5

## Plot predictions for sex
pretty_predictions_1d(model = mod, 
                      x_var = c("sex", "size_len"),
                      ylim = ylim,
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
p_x <- seq(min(physio_in_mod$temp_water), 
           max(physio_in_mod$temp_water), 
           length.out = p_n)
p_d <- data.frame(temp_water = p_x)
p_d$time_from_capture_to_surface <- min(physio_in_mod$time_from_capture_to_surface)
p_d$sex        <- factor("F", levels = c("F", "M"))
p_d$size_len   <- mean(physio_in_mod$size_len)
if(sample == "1"){
  p_d$time_from_surface_to_bs1 <- mean(physio_in_mod$time_from_surface_to_bs1) # added (not in rates models)
} else {
  p_d$time_from_surface_to_bs2 <- mean(physio_in_mod$time_from_surface_to_bs2) # added (not in rates models)
}
p_d$gaff       <- factor("N", levels = c("N", "F"))
p_d$time_index <- 1
# Plot predictions for effects of temperature when fight time is low 
pretty_predictions_1d(model = mod, 
                      newdata = p_d[p_d$time_from_capture_to_surface == min(physio_in_mod$time_from_capture_to_surface), ],
                      x_var = "temp_water",
                      ylim = ylim,
                      add_points = NULL,
                      add_error_envelope = 
                        list(add_fit = list(col = "royalblue"), 
                             add_ci = list(col = scales::alpha("skyblue", 0.2), border = FALSE)),
                      add_xlab = list(text = xlabs[3], line = xlab_line), 
                      add_ylab = NULL, 
                      add_main = list(text = "C", adj = main_adj, font = main_font)
)
# Add predictions for effects of temperature when fight time is high 
p_d$time_from_capture_to_surface <- max(physio_in_mod$time_from_capture_to_surface)
p_ci <- list_CIs(predict(mod, p_d, se.fit = TRUE, type = "response"))
add_error_envelope(p_d$temp_water, 
                   p_ci, 
                   add_fit = list(col = "darkred"),
                   add_ci = list(col = scales::alpha("red", 0.2), border = FALSE)
)
add_pt <- pt_param
add_pt$x   <- physio_in_mod$temp_water
add_pt$y   <- physio_in_mod$resp
add_pt$cex <- physio_in_mod$time_from_capture_to_surface/20
# add_pt$col <- physio_in_mod$pt_col_ft
px <- par(xpd = NA)
do.call(points, add_pt)
par(px)
legend_pos <- "topleft"
legend_adj <- 0.1
if(resp %in% c("pH_1", "HCO3_1", "pH_2")) legend_pos <- "bottomleft"
legend(legend_pos,
       lty = c(1, 1),
       col = c("royalblue", "darkred"), 
       lwd = c(1.5, 1.5),
       legend = c(expression("Time (" * H %->% S * ")" * ""[min]), 
                  expression("Time (" * H %->% S * ")" * ""[max])
                  ),
       adj = legend_adj,
       bty = "n")

## Plot predictions for fight time (by temperature)
# Define prediction data 
p_n <- 100
p_x <- seq(min(physio_in_mod$time_from_capture_to_surface), 
           max(physio_in_mod$time_from_capture_to_surface), 
           length.out = p_n)
p_d            <- data.frame(time_from_capture_to_surface = p_x)
p_d$temp_water <- min(physio_in_mod$temp_water)
p_d$sex        <- factor("F", levels = c("F", "M"))
p_d$size_len   <- mean(physio_in_mod$size_len)
if(sample == "1"){
  p_d$time_from_surface_to_bs1 <- mean(physio_in_mod$time_from_surface_to_bs1) # added
} else {
  p_d$time_from_surface_to_bs2 <- mean(physio_in_mod$time_from_surface_to_bs2) # added
}
p_d$gaff       <- factor("N", levels = c("N", "F"))
p_d$time_index <- 1
# Plot predictions for effects of fight time in low temperatures 
pretty_predictions_1d(model = mod, 
                      newdata = p_d[p_d$temp_water == min(physio_in_mod$temp_water), ],
                      x_var = "time_from_capture_to_surface",
                      ylim = ylim,
                      add_points = NULL,
                      add_error_envelope = 
                        list(add_fit = list(col = "royalblue"), 
                             add_ci = list(col = scales::alpha("skyblue", 0.2), border = FALSE)),
                      add_xlab = list(text = xlabs[4], line = xlab_line), 
                      add_ylab = NULL, 
                      add_main = list(text = "D", adj = main_adj, font = main_font)
)
# Add predictions for effects of fight time in warm temperatures
p_d$temp_water <- max(physio_in_mod$temp_water)
p_ci <- list_CIs(predict(mod, p_d, se.fit = TRUE, type = "response"))
add_error_envelope(p_d$time_from_capture_to_surface, 
                   p_ci, 
                   add_fit = list(col = "darkred"),
                   add_ci = list(col = scales::alpha("red", 0.2), border = FALSE)
)
add_pt <- pt_param
add_pt$x <- physio_in_mod$time_from_capture_to_surface
add_pt$y <- physio_in_mod$resp
add_pt$cex <- physio_in_mod$temp_water/10
do.call(points, add_pt)
legend_adj <- 0.2
if(resp %in% c("PCO2_2")) legend_pos <- "topright"
legend(legend_pos, 
       lty = c(1, 1),
       col = c("royalblue", "darkred"), 
       legend = c(expression(T[min]), expression(T[max])), 
       adj = legend_adj,
       bty = "n")

## Plot predictions for handling time and gaff 
pretty_predictions_1d(model = mod, 
                      x_var =  c(
                        colnames(physio_in_mod)[
                          stringr::str_detect(colnames(physio_in_mod), "time_from_capture")],
                        "gaff"),
                      ylim = ylim,
                      add_error_bars = ebars_param,
                      add_points = pt_param,
                      add_xlab = list(text = xlabs[5:6], line = xlab_line),
                      add_ylab = NULL,
                      add_main = list(text = c("E", "F"), adj = main_adj, font = main_font), 
                      one_page = FALSE)
par(pp)
if(save) dev.off()


################################
################################
#### Model diagnostics 

#### Model residuals
if(save) png(paste0("./fig/", resp, "_diagnostics.png"), 
             height = 5.5, width = 9, units = "in", res = 600)
pp <- par(mfrow = c(1, 2))
# car::qqPlot(mod, line = "none", rep = 1e3)
plot(mod, 1:2)
par(pp)
if(save) dev.off()

#### Predictive accuracy
physio_in_mod$x <- factor(1:nrow(physio_in_mod))
ps              <- list_CIs(predict(mod, type = "response", se.fit = TRUE))
pretty_plot(physio_in_mod$x, physio_in_mod$resp, 
            pretty_axis_args = 
              list(side = 1:2, 
                   x = list(x = range_factor(physio_in_mod$x),
                            y = range(c(physio_in_mod$resp, 
                                        ps$lowerCI, 
                                        ps$upperCI),
                                      na.rm = TRUE))), 
            type = "n",
            main = resp)
add_error_bars(physio_in_mod$x, fit = ps$fit, lwr = ps$lowerCI, upr = ps$upperCI)
points(physio_in_mod$x, physio_in_mod$resp, col = "red")


################################
################################
#### Collective model tables

#### Define a summary table for blood parameters (for BS1 and BS2)
## This contains the number of observations, alongside summary
# ... statistics of each distribution 
summaries <- 
  lapply(c(paste0(resps, "_1"), paste0(resps, "_2")), function(resp){
    physio$resp <- physio[, resp]
    pos <- which(!is.na(physio$resp))
    n_obs <- length(pos)
    stat_min <- min(physio$resp, na.rm = TRUE)
    stat_med <- median(physio$resp, na.rm = TRUE)
    stat_max <- max(physio$resp, na.rm = TRUE)
    # mod <- lm(form_1, data = physio)
    # n_mod <- nrow(model.frame(mod))
    data.frame(Parameter = substr(resp, 1, nchar(resp) - 2),
               Sample = substr(resp, nchar(resp), nchar(resp)),
               Nobs = n_obs, 
               # Nmod = n_mod, 
               Min = stat_min, 
               Med = stat_med, 
               Max = stat_max)
  }) %>% dplyr::bind_rows()
## Tidy summaries 
summaries <- 
  summaries %>% 
  tidyr::pivot_wider(names_from = Sample, 
                     values_from = c(Nobs, Min, Med, Max))
colnames(summaries) <- 
  stringr::str_replace_all(colnames(summaries), "_1", " [1]")
colnames(summaries) <- 
  stringr::str_replace_all(colnames(summaries), "_2", " [2]")
summaries$Parameter <- 
  resps_names$name[match(summaries$Parameter, resps_names$resp)]
## Examine the range in the number of obserations across parameters for BS1/2
utils.add::basic_stats(summaries$`Nobs [1]`)
utils.add::basic_stats(summaries$`Nobs [2]`)
## Write tidy table to file 
summaries <- tidy_numbers(summaries, digits = 2)
tidy_write(summaries, "./fig/blood_summaries.txt")

#### Define tidy coefficient tables derived from mod_1 for each parameter
# Define sample ("1" for BS1 and "2" for BS2)
sample_for_coefs <- "2" # "2"
coefs <- 
  lapply(paste0(resps, "_", sample_for_coefs), function(resp){
    # Fit model and get the number of observations used for model fitting
    # (... excluding K_2 and Mg_2 due to a lack of data)
    if(!(resp %in% c("K_2", "Mg_2"))){
      print(resp)
      physio$resp <- physio[, resp]
      mod         <- glm(form_1, family = gaussian(link = "log"), data = physio)
      n_mod       <- nrow(model.frame(mod))
      # Extract tidy summary table 
      coef_tbl <- 
        utils.add::tidy_coef(coef = coef(summary(mod)), 
                             coef_names = coef_names, 
                             col_names = c("Coefficient", "Estimate", 
                                           "SE", "t-value", "p-value")
        )
      # Add parameter names and deviance explained
      coef_tbl <-
        cbind(data.frame(Parameter = 
                           resps_names$name[match(substr(resp, 1, nchar(resp) - 2), 
                                                  resps_names$resp)], 
                         Nmod = nrow(model.frame(mod))), 
              coef_tbl, 
              data.frame(D = utils.add::dev_expl(mod)))
      coef_tbl$D <- add_lagging_point_zero(round(coef_tbl$D, 3), 3)
      # coef_tbl[2:nrow(coef_tbl), c("Parameter", "Nmod", "D")] <- NA
      return(coef_tbl)
    } else return(NULL)
  }) %>% dplyr::bind_rows()
# Write tidy table of coefficients to file 
tidy_write(coefs, 
           paste0("./fig/blood_coefs_", sample, ".txt"),
           na = "")

#### Summary statistics for deviance explained
# ( -> comment out `coef_tbl$D <- add_lagging_point_zero(...)` above)
if(!inherits(coefs$D, "character")) 
  utils.add::basic_stats(coefs$D, na.rm = TRUE)
## BS1
# min  mean median   max    sd  IQR  MAD
# 10.06 41.79  42.03 66.33 18.93 20.7 21.8
## BS2
# min  mean median   max    sd   IQR   MAD
# 25.84 37.55  35.57 51.39 10.47 15.69 11.44

### Visual examination of coefs
# Add p-value stars to facilitate visual checking
coefs$star <- ""
coefs$star[as.numeric(coefs$`p-value`) <= 0.05] <- "*"
# View(coefs)


#### End of code. 
################################
################################