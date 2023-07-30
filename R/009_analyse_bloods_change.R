#########################
#########################
#### analyse_bloods_change.R

#### Aims
# 1) Analyses how skate blood parameters change between BS1 and BS2.

#### Prerequisites
# 1) Define global parameters (define_global_param.R)
# 2) Process raw bloods       (process_bloods.R)


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
# source(here_r("002_define_helpers.R"))

#### Load data
source(here_r("001_define_global_param.R"))
source(here_r("002_define_helpers.R"))
physio <- readRDS("./data/skate/physio.rds")

#### Define local parameters
# Define whether or not to save figures
save <- TRUE
set.seed(1)


#########################
#########################
#### Plot BS1 and BS2

#### Loop over each response and make plots
if (save) {
  png("./fig/blood_sample_change.png",
    height = 6, width = 10, units = "in", res = 800
  )
}
pp <- par(
  mfrow = par_mf(length(resps)),
  oma = c(4, 4, 2, 2), mar = c(2, 2, 2, 2)
)
prompt <- FALSE # TRUE
obs_by_param <-
  lapply(1:length(resps), function(i) {
    
    #### Define response
    # i <- 1
    resp <- resps[i]
    ylab <- ylabs[[resp]]
    resp_1 <- paste0(resp, "_1")
    resp_2 <- paste0(resp, "_2")
    
    #### Define data 
    dat <- 
      rbind(
        data.frame(x = "BS1", y = physio[, resp_1]),
        data.frame(x = "BS2", y = physio[, resp_2]),
        data.frame(x = "BS2,surgery:N", y = physio[physio$surgery == "N", resp_2]),
        data.frame(x = "BS2,surgery:Y", y = physio[physio$surgery == "Y", resp_2])
      )
    dat <- dat[complete.cases(dat), ]
    dat$resp <- resp
    xlabs <- c("BS1", "BS2", 
               expression("BS2:" ~ S[N]),
               expression("BS2:" ~ S[Y]))
    dat$x <- factor(dat$x, 
                    levels = c("BS1", "BS2", "BS2,surgery:N",  "BS2,surgery:Y"), 
                    labels = xlabs)
    # Define boxplot widths (based on the number of data)
    w <- 
      dat |> 
      dplyr::group_by(x) |>
      dplyr::summarise(n = dplyr::n())
    
    #### Make boxplot
    pretty_boxplot(dat$x, dat$y,
      width = w$n,
      ylim = ylims[[i]], ylab = "",
      pretty_axis_args = list(side = 1:2, axis = list(list(labels = FALSE), list(NULL)), control_digits = 1)
    )
    # Add observed data (jittered horizontally)
    dat$j <- as.integer(dat$x)
    dat$j <- runif(nrow(dat), dat$j - 0.1, dat$j + 0.1)
    pt_param <- list(
      pch = 21,
      col = scales::alpha("green4", 0.5),
      bg = scales::alpha("green4", 0.5),
      cex = 0.5,
      lwd = 0.75
    )
    pt_param$x <- dat$j
    pt_param$y <- dat$y
    do.call(points, pt_param)
    # Add mean values
    points(1:4,
      dat |>
        dplyr::group_by(x) |>
        dplyr::summarise(z = mean(y)) |>
        dplyr::pull(z),
      pch = 3, lwd = 2
    )
    # Add a line at BS1
    lines(c(0.5, 4.5), rep(median(dat$y[dat$x == "BS1"]), 2), col = "red2", lty = 3, lwd = 1.5)
    # Add axes/labels
    axis(side = 1, at = 1:4, labels = xlabs, tick = 0)
    mtext(side = 2, ylab, line = 2)
    mtext(side = 3, LETTERS[i], font = 2, adj = 0)
    
    #### Continue
    if (prompt) readline("Press [Enter] to continue...")
    return(dat)
    
    
  }) |> invisible()
mtext(side = 1, "Blood sample", line = 1, outer = TRUE)
par(pp)
if (save) dev.off()


#########################
#########################
#### Summarise % change during handling

#### Calculate % change in median values of each parameter during handling
obs_by_param |>
  dplyr::bind_rows() |>
  dplyr::group_by(resp, x) |>
  dplyr::summarise(avg = median(y)) |>
  dplyr::mutate(resp = factor(resp, levels = resps)) |>
  dplyr::arrange(resp, x) |>
  tidyr::pivot_wider(
    names_from = x,
    values_from = avg
  ) |>
  dplyr::mutate(
    diff_a = BS2 - BS1,
    diff_n = `"BS2:" ~ S[N]` - BS1,
    diff_y = `"BS2:" ~ S[Y]` - BS1
  ) |>
  dplyr::arrange(diff_a) |>
  dplyr::select(
    Par. = resp,
    BS1,
    BS2,
    `BS2: S[N]` = `"BS2:" ~ S[N]`, 
   ` BS2: S[Y]` = `"BS2:" ~ S[Y]`, 
   D =  diff_a,
   `D: S[N]` = diff_n,
   `D: S[Y]` = diff_y
  ) |>
  tidy_numbers(digits = 2) |>
  tidy_write(file = "./fig/blood_change_tbl.txt")


#########################
#########################
#### Paired statistical tests

#### Summary
# We  can used paired statistical tests to test for differences between
# ... BS1 and BS2 for each blood parameter.
# The paired t test can be used to test for a difference in means if the
# ... differences are normally distributed and the variances equal.
# If the variances are not equal,
# ... If the sample size is bigger than 30, we can assume the results are
# ... ... approximately correct.
# ... Or we the unequal variance t-test can be used instead, but
# ... ... this still assumes normality.
# If normality is violated, Ruxton (2006) recommends taking ranks and then
# ... running the unequal variance t test. But this results in a loss of statistical power.
# An alternative test is the Wilcoxon test/Mann Whitney U test, but this test
# ... assumes equal variance.
# A final option is a bootstrapping approach
# ... This makes no assumptions regarding the underlying population
# ... and can be applied to a variety of situations (Ekik & Ekik, 2012)

#### Define comparison types
types <- c("BS1 vs. BS2", 
          "BS1 vs. BS2: S[N]", 
          "BS1 vs. BS2: S[Y]")

#### Define a dataframe of paired test statistics for each variable
expectations <- list(
  pH = "less",
  PCO2 = "greater",
  PO2 = "two.sided",
  HCO3 = "less",
  lac = "greater",
  glu = "greater",
  K = "greater",
  Mg = "two.sided"
)

#### Generate statistics for each comparison type 
lapply(types, function(type) {
  
  p_by_param <-
    mapply(
      function(resp, alt) {

        ## Define response variables
        # resp = "pH"; alt = "less"
        print(paste(type, ";", resp, "----"))
        r_1 <- paste0(resp, "_1")
        r_2 <- paste0(resp, "_2")
        
        ## Isolate data from individuals with observations at both BS1 and BS2
        # (as needed for a paired test)
        physio_for_resp <- physio[, c("pit", "surgery", r_1, r_2)]
        physio_for_resp <- physio_for_resp[complete.cases(physio_for_resp), ]
        if (type == "BS1 vs. BS2: S [N]") {
          physio_for_resp[physio_for_resp$surgery == "N", ]
        }
        if (type == "BS1 vs. BS2: S[N]") {
          physio_for_resp <- physio_for_resp[physio_for_resp$surgery == "N", ]
        } else if (type == "BS1 vs. BS2: S[Y]") {
          physio_for_resp <- physio_for_resp[physio_for_resp$surgery == "Y", ]
        } else {
          if (type != "BS1 vs. BS2") {
            stop("Unrecognised `type`!", call. = FALSE)
          }
        }
        bs1 <- physio_for_resp[, r_1]
        bs2 <- physio_for_resp[, r_2]
        
        run <- FALSE
        if (run) {
          ## Run paired t test and check shapiro.test for normality
          # Some variables have Shapiro p <= 0.05,
          # ... implying the assumption of normality is not met
          tt <- t.test(bs2, bs1, alternative = alt, paired = TRUE)
          sh <- shapiro.test(bs1 - bs2)
          message("Shapiro: ", round(sh$p.value, 2))
          
          ## Run wilcox.test
          # This compares whether the distribution of 'ranks' differs
          # ... i.e., whether the distributions of values for BS1/BS2 differ
          # ... but we assume equal variance
          wi <- wilcox.test(bs2, bs1, alternative = alt, paired = TRUE, exact = FALSE)
          
          ## Run an F test to compare the variance of the two groups
          # For some variables, the p value is less than 0.05,
          # ... implying a significant difference in variance.
          ft <- var.test(x = bs1, y = bs2)
          
          ## Run unequal variance t test using ranks (following Ruxton, 2006)
          we <- t.test(rank(bs2), rank(bs1), alt, paired = TRUE, var.equal = FALSE)
        }
        
        ## Bootstrapping approach
        bt <- wBoot::boot.paired.per(bs2, bs1, alternative = alt, null.hyp = 0, R = 1e4)
        
        # Return outputs
        out <- dplyr::tibble(
          type = type,
          resp = resp,
          n = length(bs1),
          MD_obs = bt$Observed,
          MD_sim = bt$Mean,
          SE = bt$SE,
          Bias = bt$Bias,
          `Bias (%)` = bt$Percent.bias,
          H0 = 0,
          H1 = bt$Alternative,
          `p-value` = bt$p.value
        )
        rownames(out) <- NULL
        # print(out)
        return(out)
        
      },
      names(expectations), expectations,
      SIMPLIFY = FALSE
    ) |>
    dplyr::bind_rows()
  
  ## Adjust p-values using the Bonferroni correction
  p_by_param$`p-value (BC)` <- p.adjust(p_by_param$`p-value`, method = "bonferroni")
  
  #### Write tidy table to file
  tidy_numbers(p_by_param, 3) |>
    tidy_write(paste0("./fig/blood_change_tbl_bootstrap_", str_strip(type), ".txt"))
  
}) |> invisible()


#### End of code.
#########################
#########################