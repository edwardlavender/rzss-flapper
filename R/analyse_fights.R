################################
################################
#### analyse_fights.R

#### This code: 
# 1) Analyses fight time. 

#### Steps preceding this code:
# 1) Define global parameters (define_global_param.R)
# 2) Process capture fights   (process_fights.R)


################################
################################
#### Set up

#### Wipe workspace and source essential packages and variables
source("./R/define_global_param.R")

#### Load data
fights <- readRDS("./data/skate/fights.rds")

#### Define local parameters
# Define whether or not to save figures
save <- TRUE


################################
################################
#### Data exploration

#### Indices for healthy/non healthy individuals
ind_health_1 <- fights$healthy == 1
# ind_health_0 <- fights$healthy == 0

#### Summary statistics 
# table(fights$pit)
pretty_boxplot(fights$sex, fights$size_area)
utils.add::basic_stats(fights$time_fight, na.rm = TRUE)
pretty_hist(fights$time_fight)

#### Correlations 
psych::pairs.panels(fights[, c("time_fight", 
                               "size_area", "current_speed", 
                               "sun", 
                               "depth", 
                               "healthy"
                               )])


################################
################################
#### Modelling

#### Focus on healthy individuals 
fights <- fights[ind_health_1, ]

#### Fit model(s)
mod_1 <- 
  glm(time_fight ~ sex + size_area * current_speed + sun + temp_water + depth,
      data = fights, 
      family = gaussian(link = "log"))
mod <- mod_1

#### Model summary
# Number of observations used for model fitting:
nrow(model.frame(mod))
# summary
summary(mod)
# deviance explained
utils.add::dev_expl(mod)

#### Model summary (tidy)
coef_names <- c("Intercept", 
                "Sex (M)", 
                "Size", 
                "Current speed", 
                "Sun angle", 
                "Temperature", 
                "Depth", 
                "Size: Current speed")
coef_tbl <- utils.add::tidy_coef(coef = coef(summary(mod)), 
                                 coef_names = coef_names)
tidy_write(coef_tbl, "./fig/fight_time_coef.txt")

#### Model residuals
if(save) tiff("./fig/fight_time_diagnostics.tiff", 
              height = 5.5, width = 9, units = "in", res = 600)
pp <- par(mfrow = c(1, 2))
# car::qqPlot(mod, line = "none", rep = 1e3)
plot(mod, 1:2)
par(pp)
if(save) dev.off()

#### Model predictions 
## Pretty x axis labels 
xlabs <- c("Sex", 
           expression("Surface area [" * m^2 * "]"), 
           expression("Current speed [" * ms^-1 * "]"), 
           expression("Sun angle [" * degree * "]"), 
           expression("Temperature [" * degree * "C]"), 
           "Depth [m]"
           )
## Make plot 
if(save) tiff("./fig/fight_time.tiff", 
              height = 5.5, width = 9, units = "in", res = 600)
pp <- par(oma = c(2, 2, 2, 2), mar = rep(2.5, 4))
pretty_predictions_1d(model = mod, 
                      add_points = list(cex = fights$size_area, lwd = 0.5, col = "grey20"),
                      add_error_bars = list(add_fit = list(pch = 3, lwd = 2), lwd = 2),
                      add_xlab = list(text = xlabs, line = 2.75),
                      add_ylab = list(text = "Fight time [minutes]"),
                      add_main = list(text = LETTERS[1:6], adj = 0, font = 2))
par(pp)
if(save) dev.off()



#### End of code. 
################################
################################