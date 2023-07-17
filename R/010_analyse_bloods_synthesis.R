#########################
#########################
#### analyse_bloods_synthesis.R

#### Aims
# 1) Synthesises the effects of each variable on
# ... blood parameters using a simulation approach.

#### Prerequisites
# 1) Define global parameters   (define_global_param.R)
# 2) Process bloods             (process_bloods.R)
# 3) Develop modelling approach (analyse_bloods.R)


#########################
#########################
#### Set up

#### Wipe workspace
rm(list = ls()) 
try(pacman::p_unload("all"), silent = TRUE) 
dv::clear() 

#### Essential packages
library(prettyGraphics)
source(here_r("002_define_helpers.R"))

#### Load data
physio <- readRDS("./data/skate/physio.rds")

#### Define param
set.seed(1)


#########################
#########################
#### Implement simulations

################################
#### Set up simulations

#### Define blood sample ("1" or "2") and response variables
sample <- "2"
if (sample == "1") {
  resps_for_bs <- paste0(resps, "_1")
} else if (sample == "2") {
  resps_for_bs <- paste0(resps, "_2")
} else {
  stop("sample not correctly specified.")
}

#### Define model formula (specific to blood sample)
if (sample == "1") {
  form_1 <- resp ~
    sex +
    size_len +
    temp_water *
      time_from_capture_to_surface + time_from_surface_to_bs1 +
    gaff
} else {
  form_1 <- resp ~
    sex +
    size_len +
    temp_water *
      time_from_capture_to_surface + time_from_surface_to_bs2 +
    gaff
}

#### Define newdata skeleton
skeleton <-
  data.frame(
    sex = factor("F", levels = c("F", "M")),
    size_len = median(physio$size_len, na.rm = TRUE),
    temp_water = median(physio$temp_water),
    time_from_capture_to_surface =
      median(physio$time_from_capture_to_surface, na.rm = TRUE),
    time_from_surface_to_bs1 =
      median(physio$time_from_surface_to_bs1, na.rm = TRUE),
    gaff = factor("N", levels = c("N", "Y"))
  )
if (sample == "2") {
  skeleton$time_from_surface_to_bs1 <- NULL
  skeleton$time_from_surface_to_bs2 <-
    median(physio$time_from_surface_to_bs2, na.rm = TRUE)
}
skeleton <- rbind(skeleton, skeleton)

#### Define simulation comparisons
## Sex:
# ... female versus male
## Body size:
# ... minimum versus maximum
## Temperature:
# ... minimum versus maximum (low fight time)
# ... minimum versus maximum (high fight time)
## Time from capture to surface
# ... minimum versus maximum (low temperature)
# ... minimum versus maximum (high temperature)
## From from surface to BS1
# ... minimum versus maximum
## Gaff
# ... N versus Y
comparisons <-
  list(
    sex = data.frame(sex = factor(c("F", "M"), levels = c("F", "M"))),
    size = data.frame(size_len = range(physio$size_len, na.rm = TRUE)),
    temp_1 = data.frame(
      temp_water = range(physio$temp_water),
      time_from_capture_to_surface = min(physio$time_from_capture_to_surface)
    ),
    temp_2 = data.frame(
      temp_water = range(physio$temp_water),
      time_from_capture_to_surface = max(physio$time_from_capture_to_surface)
    ),
    time_fight_1 = data.frame(
      time_from_capture_to_surface = range(physio$time_from_capture_to_surface),
      temp_water = min(physio$temp_water)
    ),
    time_fight_2 = data.frame(
      time_from_capture_to_surface = range(physio$time_from_capture_to_surface),
      temp_water = max(physio$temp_water)
    ),
    time_surface = data.frame(time_from_surface_to_bs1 = range(physio$time_from_surface_to_bs1)),
    gaff = data.frame(gaff = factor(c("N", "Y"), levels = c("N", "Y")))
  )
if (sample == "2") {
  comparisons$time_surface <-
    data.frame(time_from_surface_to_bs2 = range(physio$time_from_surface_to_bs2, na.rm = TRUE))
}
median(physio$size_len, na.rm = TRUE)
median(physio$temp_water, na.rm = TRUE)
median(physio$time_from_capture_to_surface, na.rm = TRUE)
median(physio$time_from_surface_to_bs1, na.rm = TRUE)
median(physio$time_from_surface_to_bs2, na.rm = TRUE)

#### Define simulation comparison labels
labels <- c(
  sex = expression(Sex[M]),
  size = expression(Size),
  temp_1 = expression(T[L]),
  temp_2 = expression(T[H]),
  time_fight_1 = expression(FT[L]),
  time_fight_2 = expression(FT[H]),
  time_surface = expression(ST[]),
  gaff = expression(Gaff[Y])
)


################################
#### Implement simulation

#### Implement simulations
# [with R = 1e3 simulations, this takes 2 mins one one core]
run <- FALSE
if (run) {
  R <- 1e3
  outsims <-
    pbapply::pblapply(resps_for_bs, cl = 7L, function(resp) {
      #### Fit model for response
      # resp <- resps_for_bs[5]
      print(paste0(resp, "-----------------------------"))
      physio$resp <- physio[, resp]
      mod <- glm(form_1, data = physio, family = gaussian(link = "log"))

      #### Define comparisons for selected response
      comparisons_by_resp <-
        lapply(1:length(comparisons), function(i) {
          print(i)

          #### Set simulation specific newdata values
          # i <- 1
          comparison <- comparisons[[i]]
          newdata <- skeleton
          for (x in colnames(comparison)) {
            newdata[, x] <- comparison[, x]
          }

          #### Implement bootstrap to estimate ratios
          # Use tryCatch because for some variables (e.g., K_2)
          # ... there are too few observation (e.g., for Gaff 'Y') to
          # ... implement bootstrapping. In the sampling proceedure,
          # ... observations are dropped and the model can't fit
          # ... because gaffing has only one factor level.
          ratio <- tryCatch(
            {
              finalfit::boot_predict(mod,
                newdata = newdata,
                comparison = "ratio",
                condense = FALSE,
                R = R
              )
            },
            error = function(e) {
              return(e)
            }
          )

          #### Extract relevant information
          if (!inherits(ratio, "error")) {
            out <- data.frame(
              comparison = names(comparisons)[i],
              resp = resp,
              ratio = ratio$ratio[2],
              lowerCI = ratio$ratio_conf.low[2],
              upperCI = ratio$ratio_conf.high[2]
            )
          } else {
            out <- data.frame(
              comparison = names(comparisons)[i],
              resp = resp,
              ratio = NA,
              lowerCI = NA,
              upperCI = NA
            )
          }
          # print("ok")
          return(out)
        })

      out <- dplyr::bind_rows(comparisons_by_resp)
      return(out)
    })
  saveRDS(outsims, paste0("./data/estimates/bloods_sims_", sample, "_.rds"))
} else {
  outsims <- readRDS(paste0("./data/estimates/bloods_sims_", sample, "_.rds"))
}


################################
#### Process simulations

#### Collate estimates
outsims <-
  outsims |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    comparison = factor(comparison, levels = names(comparisons)),
    resp = factor(resp, levels = resps_for_bs)
  ) |>
  dplyr::arrange(comparison, resp) |>
  dplyr::filter(!(resp %in% c("K_2", "Mg_2")))

#### Define simulation IDs (for the x axis)
outsims <-
  outsims |>
  dplyr::group_by(resp) |>
  dplyr::mutate(id = dplyr::row_number())

#### Define colours to distinguish explanatory variables
cols <- RColorBrewer::brewer.pal(length(comparisons), "Dark2")
outsims$col <- cols[outsims$comparison]


################################
#### Visualise simulations

#### Set up plot to save
png(paste0("./fig/blood_ratios_", sample, ".png"),
  height = 6, width = 6, units = "in", res = 600
)
pp <- par(
  mfrow = c(length(unique(outsims$resp)) / 2, 2),
  oma = c(2, 2, 2, 2), mar = c(2, 2, 2, 0)
)

#### Generate plots
outsims_by_resps <- split(outsims, outsims$resp)
lapply(1:length(outsims_by_resps), function(i) {
  outsim <- outsims_by_resps[[i]]
  resp <- as.character(outsim$resp[1])
  resp <- substr(resp, 1, nchar(resp) - 2)
  if (!all(is.na(outsim$ratio))) {
    axis_ls <-
      pretty_plot(outsim$comparison, outsim$ratio,
        ylim = ylims_ratios[[resp]],
        pretty_axis_args =
          list(
            x = list(
              x = outsim$comparison[1:2],
              y = range(c(outsim$lowerCI, outsim$upperCI), na.rm = TRUE)
            ),
            pretty = list(list(n = 10), list(n = 4)),
            add = FALSE
          ),
        xlab = "", ylab = "",
        type = "n"
      )
    lines(axis_ls[[1]]$lim, c(1, 1), lty = 2)
    elwd <- 1
    add_error_bars(outsim$id,
      outsim$ratio,
      lwr = outsim$lowerCI, upr = outsim$upperCI, lwd = elwd,
      add_fit = list(pch = "-", col = outsim$col, lwd = elwd),
      col = outsim$col
    )

    axis_ls[[1]]$axis$labels <- labels
    pretty_axis(axis_ls = axis_ls, add = TRUE)
    title <- titles[[resp]]
    mtext(side = 3, title, font = 2, adj = 0.03, line = 0.25)
  }
}) |> invisible()

#### Add titles
mtext(side = 1, "Term", outer = TRUE, line = 0.5)
mtext(side = 2, "Effect size (ratio)", outer = TRUE, line = 0.5)
par(pp)
dev.off()


#### End of code. 
#########################
#########################