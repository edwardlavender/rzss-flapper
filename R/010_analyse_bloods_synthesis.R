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
library(dv)
library(prettyGraphics)
library(tictoc)

#### Load data
source(here_r("001_define_global_param.R"))
physio <- readRDS("./data/skate/physio.rds")

#### Define param
save <- FALSE
set.seed(1)


#########################
#########################
#### Implement simulations

#########################
#### Set up simulations

#### Define blood sample ("1", "2", "3") and response variables
sample <- "3"
if (sample == "1") {
  resps_for_bs <- paste0(resps, "_1")
} else if (sample == "2") {
  resps_for_bs <- paste0(resps, "_2")
} else if (sample == "3") {
  resps_for_bs <- paste0(resps, "_3")
  } else {
  stop("sample not correctly specified.")
  }
resps_for_bs <- resps_for_bs[!(resps_for_bs %in% resps_exclude)]

#### Define newdata skeleton
skeleton <- readRDS(here_data("helper", "constants.rds"))

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
    time_to_sample = data.frame(time_from_surface_to_bs1 = range(physio$time_from_surface_to_bs1)),
    gaff = data.frame(gaff = factor(c("N", "Y"), levels = c("N", "Y")))
  )
if (sample %in% c("2", "3")) {
  if (sample == "2") {
    # For BS2, time to sample is time to BS2
    comparisons$time_to_sample <- 
      data.frame(time_from_surface_to_bs2 = range(physio$time_from_surface_to_bs2, na.rm = TRUE))
  }
  if (sample == "3") {
    # For differences, time_to_sample is time to BS1
    # time_btw_sample is time from BS1 to BS2
    comparisons$time_btw_sample <- 
      data.frame(time_from_bs1_to_bs2 = range(physio$time_from_bs1_to_bs2, na.rm = TRUE))
  }
  # Add surgery 
  comparisons$surgery <- data.frame(surgery = factor(c("N", "Y"), levels = c("N", "Y")))
}
# Fix order
nms <- c(
  "sex", "size", "temp_1", "temp_2", 
  "time_fight_1", "time_fight_2", 
  "time_to_sample", "time_btw_sample", 
  "gaff", "surgery")
comparisons <- comparisons[nms[nms %in% names(comparisons)]]

#### Define simulation comparison labels
if (sample == 1) {
  labels <- c(
    sex = expression(Sex[M]),
    size = expression(Size),
    temp_1 = expression("T:" ~ FT[L]),
    temp_2 = expression("T:" ~ FT[H]),
    time_fight_1 = expression("FT:" ~ T[L]),
    time_fight_2 = expression("FT:" ~ T[H]),
    time_to_sample = expression(ST[]),
    gaff = expression(Gaff[Y])
  )
} else if (sample == 2) {
  labels <- c(
    sex = expression(Sex[M]),
    size = expression(Size),
    temp_1 = expression("T:" ~ FT[L]),
    temp_2 = expression("T:" ~ FT[H]),
    time_fight_1 = expression("FT:" ~ T[L]),
    time_fight_2 = expression("FT:" ~ T[H]),
    time_to_sample = expression(ST[]),
    gaff = expression(Gaff[Y]), 
    surgery = expression(Tag[Y])
  )
} else if (sample == 3) {
  labels <- c(
    sex = expression(Sex[M]),
    size = expression(Size),
    temp_1 = expression("T:" ~ FT[L]),
    temp_2 = expression("T:" ~ FT[H]),
    time_fight_1 = expression("FT:" ~ T[L]),
    time_fight_2 = expression("FT:" ~ T[H]),
    time_to_sample = expression(ST[1]),
    time_btw_sample = expression(ST[2]),
    gaff = expression(Gaff[Y]), 
    surgery = expression(Tag[Y])
  )
}
labels <- labels[names(comparisons)]


#########################
#### Implement simulation

#### Implement simulations
# [with R = 1e3 simulations, this takes 1 min one one core]
tic()
overwrite <- FALSE
file_sim <- paste0("./data/estimates/bloods_sims_", sample, ".rds")
if (overwrite | !file.exists(file_sim)) {
  R <- 1e3
  outsims <-
    pbapply::pblapply(resps_for_bs, cl = NULL, function(resp) {
      
      #### Fit model for response
      # resp <- resps_for_bs[1]
      print(paste0(resp, "-----------------------------"))
      physio$resp <- physio[, resp]
      mod <- readRDS(here_data("models", sample, paste0(resp, ".rds")))

      #### Define comparisons for selected response
      comparisons_by_resp <-
        lapply(1:length(comparisons), function(i) {
          print(i)

          #### Set simulation specific newdata values
          # i <- 1
          comparison <- comparisons[[i]]
          newdata <- lapply(seq_len(nrow(comparison)), \(i) skeleton) |> dplyr::bind_rows()
          for (x in colnames(comparison)) {
            newdata[, x] <- comparison[, x]
          }

          #### Implement bootstrap to estimate ratios
          # Use which() and tryCatch because for some variables (e.g., K_2)
          # ... there are too few observation (e.g., for Gaff 'Y') to
          # ... implement bootstrapping. In the sampling procedure,
          # ... observations are dropped and the model can't fit
          # ... because gaffing has only one factor level.
          success <- FALSE
          count   <- 0
          while (!success & count < 500) {
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
            if (!inherits(ratio, "error")) {
              success <- TRUE
            }
            count <- count + 1
          }

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
  saveRDS(outsims, file_sim)
} else {
  outsims <- readRDS(file_sim)
}
toc()


#########################
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
  dplyr::filter(!(resp %in% resps_exclude))

#### Define simulation IDs (for the x axis)
outsims <-
  outsims |>
  dplyr::group_by(resp) |>
  dplyr::mutate(id = dplyr::row_number())

#### Define colours to distinguish explanatory variables
# We always pick 10 colours (the max number of comparisons)
# ... so colours are consistent across all plots
cols <- colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(10)
outsims$col <- cols[outsims$comparison]
stopifnot(!any(is.na(outsims$col)))


#########################
#### Visualise simulations

#### Set up plot to save
if (save) {
  png(paste0("./fig/blood_ratios_", sample, ".png"),
  height = 6, width = 6, units = "in", res = 600
)
}
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
  if (sample %in% c(1, 2)) {
    ylim <- ylims_ratios[[resp]]
  } else {
    ylim <- NULL
  }
  if (!all(is.na(outsim$ratio))) {
    axis_ls <-
      pretty_plot(outsim$comparison, outsim$ratio,
        ylim = ylim,
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
    if (sample %in% c("1", "2")) { 
      title <- titles[[resp]]
    } else {
      title <- titles_3[[resp]]
    }
    mtext(side = 3, title, font = 2, adj = 0.03, line = 0.25)
  }
}) |> invisible()

#### Add titles
mtext(side = 1, "Term", outer = TRUE, line = 0.5)
mtext(side = 2, "Effect size (ratio)", outer = TRUE, line = 0.5)
par(pp)
if (save) dev.off()


#### End of code. 
#########################
#########################