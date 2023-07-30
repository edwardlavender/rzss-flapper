#########################
#########################
#### analyse_fights.R

#### Aims
# 1) Analyses fight time.

#### Prerequisites
# 1) Define global parameters (define_global_param.R)
# 2) Process capture fights   (process_fights.R)


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
fights <- readRDS("./data/skate/fights.rds")

#### Define local parameters
# Define whether or not to save figures
save <- TRUE
set.seed(1)


#########################
#########################
#### Data exploration

#### Indices for healthy/non healthy individuals
ind_health_1 <- fights$healthy == 1
# ind_health_0 <- fights$healthy == 0

#### Summary statistics
# table(fights$pit)
utils.add::basic_stats(fights$time_fight)
pretty_boxplot(fights$sex, fights$size_area)
utils.add::basic_stats(fights$time_fight, na.rm = TRUE)
pretty_hist(fights$time_fight)

#### Correlations
psych::pairs.panels(fights[, c(
  "time_fight",
  "size_area", "current_speed",
  "sun",
  "depth",
  "healthy"
)])


#########################
#########################
#### Modelling

#### Focus on healthy individuals
fights <- fights[ind_health_1, ]

#### Fit model(s)
mod_1 <-
  glm(time_fight ~ sex + size_area * current_speed + sun + temp_water + depth,
    data = fights,
    family = gaussian(link = "log")
  )
mod <- mod_1

#### Model summary
# Number of observations used for model fitting:
nrow(model.frame(mod))
# summary
summary(mod)
# deviance explained
utils.add::dev_expl(mod)

#### Model summary (tidy)
coef_names <- c(
  "Intercept",
  "SexM",
  "Size",
  "Current speed",
  "Sun angle",
  "Temperature",
  "Depth",
  "Size: Current speed"
)
coef_tbl <- utils.add::tidy_coef(
  coef = coef(summary(mod)),
  coef_names = coef_names,
  col_names = c("Coefficient", "Estimate", "SE", "t-value", "p-value")
)
tidy_write(coef_tbl, "./fig/fight_time_coef.txt")

#### Model residuals
if (save) {
  png("./fig/fight_time_diagnostics.png",
    height = 5.5, width = 9, units = "in", res = 600
  )
}
pp <- par(mfrow = c(1, 2))
# car::qqPlot(mod, line = "none", rep = 1e3)
plot(mod, 1:2)
par(pp)
if (save) dev.off()
# plot(DHARMa::simulateResiduals(mod))

#### Model predictions
## Graphical param
pt_param$cex <- fights$size_area
jt <- 0.2
jt_param <-
  list(sex = c(jt, 0))
ebars_param$lwd <- 2.5
ebars_param$add_fit$pch <- "+"
ebars_param$add_fit$lwd <- 1.25
ebars_param$col <- scales::alpha("grey50", 0.95)
## Pretty x axis labels
xlabs <- c(
  "Sex",
  expression("Size [" * m^2 * "]"),
  expression("Current speed [" * ms^-1 * "]"),
  expression("Sun angle [" * degree * "]"),
  expression("Temperature [" * degree * "C]"),
  "Depth [m]"
)
## Make plot
if (save) {
  png("./fig/fight_time.png",
    height = 5.5, width = 9, units = "in", res = 600
  )
}
pp <- par(oma = c(2, 2, 2, 2), mar = rep(2.5, 4))
pretty_predictions_1d(
  model = mod,
  average = median,
  add_points = pt_param,
  add_points_jitter = jt_param,
  add_error_bars = ebars_param,
  add_error_envelope = eenv_param,
  add_order = list(sex = c("points", "predictions"), 
                   size_area = c("predictions", "points"), 
                   current_speed = c("predictions", "points"), 
                   sun = c("predictions", "points"), 
                   temp_water = c("predictions", "points"), 
                   depth = c("predictions", "points")),
  add_xlab = list(text = xlabs, line = 2.75),
  add_ylab = list(text = "Fight time [minutes]"),
  add_main = list(text = LETTERS[1:6], adj = 0, font = 2)
)
range(fights$size_area)
px <- par(xpd = NA)
legend(0.36, 1.1,
       legend = c("0.5", "1.0", "1.5"), 
       pch = pt_param$pch, 
       col = pt_param$col,
       cex = 0.8,
       pt.bg = pt_param$bg,
       pt.cex = c(0.5, 1, 1.5), 
       horiz = FALSE, bty = "n")
par(px)
# axis(side = 1, col = "red"); axis(side = 2, col = "red")
par(pp)
if (save) dev.off()
open("./fig/fight_time.png")


#### End of code.
#########################
#########################