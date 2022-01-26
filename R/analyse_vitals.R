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

#### Define local parameters
# Define whether or not to save figures
save <- TRUE

#### Load data 
rates <- readRDS("./data/rates.rds")


################################
################################
#### Data exploration

#### Key questions: 
# 1) How does temperature affect respiration/heart rates? 
# 2) How does fight time affect respiration/heart rates?
# 3) How do respiration/heart rates change through time on deck? 

#### Summary statistics 
utils.add::basic_stats(rates$hr, na.rm = TRUE)
utils.add::basic_stats(rates$rr, na.rm = TRUE)

#### Correlations 
psych::pairs.panels(rates[, c("hr", "rr", 
                              "sex", 
                              "size_len", 
                              "time_from_capture_to_surface", 
                              "temp_water", 
                              "gaff", 
                              "healthy", 
                              "time_index"
                              )])

#### Individuals and body size
# Are individuals uniquely defined by size? Mainly. 
rates %>%
  dplyr::group_by(pit) %>% 
  dplyr::slice(1L) %>%
  dplyr::pull(size_len) %>%
  table()


################################
################################
#### Modelling 

#### Define response variable
resp <- "hr" # "hr"
rates$resp <- rates[, resp]
if(resp == "rr"){
  ylab <- "Respiratory rate [units]"
} else if(resp == "hr"){
  ylab <- "Heart rate [units]"
}

#### Implement model 
# Below, the default (correlated intercepts and slopes) formulation is used
mod_1 <- gamm(resp ~ 
                sex * time_from_capture_to_surface + 
                size_len + 
                temp_water + 
                gaff + 
                s(time_index),
              random = list(pit = ~1 + time_index), 
              family = poisson,
              data = rates, 
              method = "REML")

#### Model comparison
mod <- mod_1

#### Model summary
summary(mod$gam)

#### Model smooths
plot(mod$gam, all.terms = TRUE, pages = 1, scheme = 1)

#### Model predictions
xlabs <- c("Sex", 
           expression("Time (hook" %->% "surface) [mins]"), 
           "Length [cm]", 
           expression("Temperature [" * degree * "C]"), 
           "Gaff", 
           "Time on deck [mins]"
           )
if(save) tiff(paste0("./fig/", resp, ".tiff"), 
              height = 5.5, width = 9, units = "in", res = 600)
pp <- par(oma = c(2, 2, 2, 2), mar = rep(2.5, 4))
rates$col <- c("royalblue", "black")[rates$sex]
pretty_predictions_1d(model = mod$gam, 
                      add_points = list(cex = 0.5, lwd = 0.5, col = rates$col),
                      add_error_bars = list(add_fit = list(pch = 21, bg = "black", cex = 2)),
                      add_xlab = list(text = xlabs, line = 2.25),
                      add_ylab = list(text = ylab),
                      add_main = list(text = LETTERS[1:6], adj = 0, font = 2))
par(pp)
if(save) dev.off()

#### Model diagnostics
resids <- resid(mod$lme, type = "normalized")
fitted <- fitted(mod$lme)
pp <- par(mfrow = c(1, 2))
pretty_plot(fitted, resids)
qqnorm(resids); qqline(resids)
par(pp)


#### End of code. 
################################
################################