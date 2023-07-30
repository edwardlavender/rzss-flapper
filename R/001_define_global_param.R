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
# Titles for simulation-based analysis of changes 
titles_3 <- list(
  pH = expression(bold("A") ~ "(" * "pH" * ")"),
  HCO3 = expression(bold("B") ~ "(" * HCO[3]^" -" * ")"),
  lac = expression(bold("C") ~ "(" * "Lac" * ")"),
  glu = expression(bold("D") ~ "(" * "Glu" * ")")
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
## Excludes
resps_exclude <- c("K_2", "Mg_2", "PCO2_3", "PO2_3", "K_3", "Mg_3")

#### Effect size limits
ylims_ratios <- list(
  pH = c(0.90, 1.1),
  PCO2 = c(0, 8),
  PO2 = c(0, 8),
  HCO3 = c(0, 2.5),
  lac = c(0, 11),
  glu = c(0, 8),
  K = c(0, 6),
  Mg = c(0.5, 2)
)

#### Define graphical parameters for error bars and envelopes
ebars_lwd <- 2
ebars_param <- list(
  add_fit = list(pch = 3, bg = "black", cex = 2, lwd = ebars_lwd),
  lwd = ebars_lwd
)
eenv_param <- 
  list(add_ci = list(col = scales::alpha("lightgrey", 0.8), border = FALSE),
       add_fit = list(col = "black", lwd = 1.25))

#### Point parameters
pt_param <- list(
  pch = 21,
  col = scales::alpha("green4", 0.75),
  bg = scales::alpha("green4", 0.75),
  cex = 1,
  lwd = 0
)

#### Colour bars
# Fight time 
pt_cols_ft    <- pretty_cols_brewer(c(9, 55), buffer = TRUE,
                                    scheme = "RdBu", 
                                    select = 1:11, # c(1:4, 7:11), 
                                    rev = TRUE)
# Temperature
pt_cols_temp    <- pretty_cols_brewer(c(7.20, 13.96), buffer = TRUE, 
                                      scheme = "RdBu", 
                                      select = 1:11, # c(1:4, 7:11), 
                                      rev = TRUE)
# Individual
pt_cols_id <- pretty_cols_brewer(c(1, 55), buffer = TRUE, 
                                 n_breaks = 55,
                                 pal = function(n) viridis::viridis(n, alpha = 0.7),
                                 rev = TRUE)

#### End of code.
#########################
#########################