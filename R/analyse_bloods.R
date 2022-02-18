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


################################
################################
#### Modelling workflow 

################################
#### Define response(s)

#### Define response variable/sample
# "pH"   "PCO2" "PO2"  "HCO3" "lac"  "glu"  "K"    "Mg"  
yvar   <- "lac"
sample <- "1"
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
#### Data exploration

#### Distribution of response 
utils.add::basic_stats(physior[, resp], na.rm = TRUE)

#### Variable correlations 
psych::pairs.panels(physior)


################################
#### Modelling 

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

#### Model residuals
if(save) png(paste0("./fig/", resp, "_diagnostics.png"), 
             height = 5.5, width = 9, units = "in", res = 600)
pp <- par(mfrow = c(1, 2))
# car::qqPlot(mod, line = "none", rep = 1e3)
plot(mod, 1:2)
par(pp)
if(save) dev.off()

#### Model predictions 
## Pretty x axis labels 
# xlabs <- colnames(model.frame(mod))[2:ncol(model.frame(mod))]
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
## Make plot 
if(save) png(paste0("./fig/", resp, ".png"), 
             height = 5.5, width = 9, units = "in", res = 600)
pp <- par(oma = c(2, 2, 2, 2), mar = rep(2.5, 4))
physio_in_mod <- model.frame(mod)
## Define point colours, shapes and sizes
col_param <- pretty_cols_brewer(zlim = range(physio_in_mod$temp_water), 
                                scheme = "RdYlBu"
)
col_param$col <- scales::alpha(col_param$col, 0.5)
physio_in_mod$pt_col <- col_param$col[findInterval(physio_in_mod$temp_water, col_param$breaks)]
physio_in_mod$pt_cex <- physio_in_mod$size_len/225
physio_in_mod$pt_pch <- 21 # c(1, 4)[rates_in_mod$sex]
pt_param <- list(col = physio_in_mod$pt_col, 
                 cex = physio_in_mod$pt_cex, 
                 pch = physio_in_mod$pt_pch)
pretty_predictions_1d(model = mod, 
                      pretty_axis_args = list(control_digits = 2),
                      add_points = pt_param,
                      add_error_bars = list(add_fit = list(pch = 21, bg = "black", cex = 2)),
                      add_xlab = list(text = xlabs, line = 2.25),
                      add_ylab = list(text = ylabs[[yvar]]),
                      add_main = list(text = LETTERS[1:6], adj = 0, font = 2))
par(pp)
if(save) dev.off()


################################
#### Summaries

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
sample_for_coefs <- "1" # "2"
coefs <- 
  lapply(paste0(resps, "_", sample_for_coefs), function(resp){
    # Fit model and get the number of observations used for model fitting
    print(resp)
    physio$resp <- physio[, resp]
    mod         <- lm(form_1, data = physio)
    n_mod       <- nrow(model.frame(mod))
    # Extract tidy summary table 
    coef_tbl <- 
      utils.add::tidy_coef(coef = coef(summary(mod)), 
                           coef_names = coef_names, 
                           col_names = c("Coefficient", "Estimate", 
                                         "SE", "t-value", "p-value")
      )
    # Add parameter names and R2
    coef_tbl <-
      cbind(data.frame(Parameter = 
                         resps_names$name[match(substr(resp, 1, nchar(resp) - 2), 
                                                resps_names$resp)], 
                       Nmod = nrow(model.frame(mod))), 
            coef_tbl, 
            data.frame(R2 = summary(mod)$r.squared))
    coef_tbl$R2 <- add_lagging_point_zero(round(coef_tbl$R2, 3), 3)
    coef_tbl[2:nrow(coef_tbl), c("Parameter", "Nmod", "R2")] <- NA
    return(coef_tbl)
  }) %>% dplyr::bind_rows()
# Write tidy table of coefficients to file 
tidy_write(coefs, 
           paste0("./fig/blood_coefs_", sample, ".txt"),
           na = "")


#### End of code. 
################################
################################