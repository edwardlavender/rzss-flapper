#########################
#########################
#### define_global_param.R

#### Aims
# 1) Defines global parameters

#### Prerequisites
# 1) NA


#########################
#########################
#### Define global parameters

#### Response variables
## Define core list of response variables
# Exclude: BE, SO2, Ca, Mg (?)
resps <- c("pH", "PCO2", "PO2", "HCO3", "lac", "glu", "K", "Mg")
## Define names (e.g., for tables)
resps_names <-
  data.frame(
    resp = c("pH", "PCO2", "PO2", "HCO3", "lac", "glu", "K", "Mg"),
    name = c("pH", "PCO2", "PO2", "HCO3", "Lac", "Glu", "K", "Mg")
  )
## Define plot titles
titles <- list(
  pH = expression(bold("A") ~ "(" * "pH" * ")"),
  PCO2 = expression(bold("B") ~ "(" * PCO[2] * ")"),
  PO2 = expression(bold("C") ~ "(" * PO[2] * ")"),
  HCO3 = expression(bold("D") ~ "(" * HCO[3]^" -" * ")"),
  lac = expression(bold("E") ~ "(" * "Lac" * ")"),
  glu = expression(bold("F") ~ "(" * "Glu" * ")"),
  K = expression(bold("G") ~ "(" * K^"+" * ")"),
  Mg = expression(bold("H") ~ "(" * Mg^"2+" * ")")
)
## Define pretty axis expressions
ylabs <- list(
  pH = "pH",
  PCO2 = expression(PCO[2] ~ "[mmHg]"),
  PO2 = expression(PO[2] ~ "[mmHg]"),
  HCO3 = expression(HCO[3]^" -" ~ "[mmol" * L^-1 * "]"),
  lac = expression("Lac [mmol" * L^-1 * "]"),
  glu = expression("Glu [mmol" * L^-1 * "]"),
  K = expression(K^"+" ~ "[mmol" * L^-1 * "]"),
  Mg = expression(Mg^"2+" ~ "[mmol" * L^-1 * "]")
)
## Define legend labels
ylabs_legend <- c(
  expression("pH"),
  expression(PCO[2]),
  expression(PO[2]),
  expression(HCO[3]^" -"),
  expression("Lac"),
  expression("Glu"),
  expression(K^"+"),
  expression(Mg^"2+")
)
## Check all labels
# Check ylabs
labs_check <- FALSE
if (labs_check) {
  plot_labs <- function(labs) {
    pp <- par(mfrow = prettyGraphics::par_mf(length(resps)))
    for (i in 1:length(resps)) {
      if (inherits(labs, "list")) {
        plot(0, main = labs[[i]])
      } else {
        plot(0, main = labs[i])
      }
    }
    par(pp)
  }
  plot_labs(titles)
  plot_labs(ylabs)
  plot_labs(ylabs_legend)
}
## Blood parameter limits
ylims <- list(
  pH = c(6.9, 7.8),
  PCO2 = c(1, 10),
  PO2 = c(0, 300),
  HCO3 = c(2, 10),
  lac = c(0, 5),
  glu = c(0.5, 3),
  K = c(0, 12),
  Mg = c(0.7, 1.7)
)

#### Effect size limits
ylims_ratios <- list(
  pH = c(0.90, 1.05),
  PCO2 = c(0, 4),
  PO2 = c(0, 4),
  HCO3 = c(0, 2),
  lac = c(0, 10),
  glu = c(0, 4),
  K = c(0, 6),
  Mg = c(0.5, 2)
)

#### Define graphical parameters for error bars
ebars_lwd <- 2
ebars_param <- list(
  add_fit = list(pch = 3, bg = "black", cex = 2, lwd = ebars_lwd),
  lwd = ebars_lwd
)



#### End of code.
#########################
#########################