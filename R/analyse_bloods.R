################################
################################
#### analyse_bloods.R

#### This script: 
# 1) Analyses skate blood parameters. 

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
sample <- "1"
resp   <- paste0("glu_", sample)

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
    sex * 
    time_from_capture_to_surface + time_from_surface_to_bs1 + 
    size_len + temp_water + gaff
  form_2 <- resp ~ 
    sex + 
    time_from_capture_to_surface + time_from_surface_to_bs1 + 
    size_len + temp_water + gaff
  form_3 <- resp ~ 
    sex + 
    time_from_capture_to_bs1 + 
    size_len + temp_water + gaff
} else if(sample == 2){
  form_1 <- resp ~ 
    sex * 
    time_from_capture_to_surface + time_from_surface_to_bs2 + 
    size_len + temp_water + gaff
  form_2 <- resp ~ 
    sex + time_from_capture_to_surface + time_from_surface_to_bs2 + 
    size_len + temp_water + gaff
  form_3 <- resp ~ 
    sex + time_from_capture_to_bs2 + 
    size_len + temp_water + gaff
}

#### Model fitting 
mod_1 <- glm(form_1, data = physior)
mod_2 <- glm(form_2, data = physior)
mod_3 <- glm(form_3, data = physior)

#### Model comparison
( ranks <- AIC(mod_1, mod_2, mod_3) )
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
utils.add::dev_expl(mod)

#### Model residuals
pp <- par(mfrow = c(1, 2))
plot(mod, 1:2)
par(pp)

#### Model predictions 
xlabs <- colnames(model.frame(mod))[2:ncol(model.frame(mod))]
pretty_predictions_1d(mod, 
                      pretty_axis_args = list(control_digits = 2),
                      add_points = list(cex = 0.5, lwd = 0.5, col = "grey20"),
                      add_error_bars = list(add_fit = list(pch = 21, bg = "black", cex = 2)),
                      add_xlab = list(text = xlabs, line = 2),
                      add_main = list(text = LETTERS[1:6], adj = 0, font = 2))


#### End of code. 
################################
################################