################################
################################
#### analyse_bloods_change.R

#### This script:
# 1) Analyses how skate blood parameters change between BS1 and BS2.

#### Steps preceding this script:
# 1) Define global parameters (define_global_param.R)
# 2) Process raw bloods       (process_bloods.R)


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
#### Plot BS1 and BS2

#### Loop over each response and make plots
if (save) {
  png("./fig/blood_sample_change.png",
    height = 6, width = 8, units = "in", res = 800
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
    y <- c(physio[, resp_1], physio[, resp_2])
    y_is_valid <- !is.na(y)
    y <- y[y_is_valid]

    #### Define 'x' variable
    x <- rep(c("BS1", "BS2"), each = nrow(physio))
    x <- x[y_is_valid]
    x <- factor(x, levels = unique(x))
    dat <- data.frame(resp = resp, x = x, y = y)

    #### Make boxplot
    pretty_boxplot(x, y,
      width = c(
        length(which(!is.na(physio[, resp_1]))),
        length(which(!is.na(physio[, resp_2])))
      ),
      ylim = ylims[[i]], ylab = "",
      pretty_axis_args = list(side = 1:2, control_digits = 1)
    )
    points(c(1, 2),
      dat %>%
        dplyr::group_by(x) %>%
        dplyr::summarise(z = mean(y)) %>%
        dplyr::pull(z),
      pch = 3
    )
    mtext(side = 2, ylab, line = 2)
    mtext(side = 3, LETTERS[i], font = 2, adj = 0)
    if (prompt) readline("Press [Enter] to continue...")
    return(dat)
  }) %>% invisible()
mtext(side = 1, "Blood sample", line = 1, outer = TRUE)
par(pp)
if (save) dev.off()


################################
################################
#### Summarise % change during handling

#### Calculate % change in median values of each parameter during handling
obs_by_param %>%
  dplyr::bind_rows() %>%
  dplyr::group_by(resp, x) %>%
  dplyr::summarise(avg = median(y)) %>%
  dplyr::mutate(resp = factor(resp, levels = resps)) %>%
  dplyr::arrange(resp, x) %>%
  tidyr::pivot_wider(
    names_from = x,
    values_from = avg
  ) %>%
  dplyr::mutate(
    difference = BS2 - BS1,
    percentage = (difference / BS1) * 100
  ) %>%
  dplyr::arrange(percentage) %>%
  dplyr::select(
    Par. = resp,
    BS1 = BS1,
    BS2 = BS2,
    `Absolute difference` = difference,
    `Percentage difference` = percentage
  ) %>%
  tidy_numbers(digits = 2) %>%
  tidy_write(file = "./fig/blood_change_tbl.txt")


################################
################################
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
p_by_param <-
  mapply(
    function(resp, alt) {
      ## Define response variables
      # resp = "pH"; alt = "less"
      r_1 <- paste0(resp, "_1")
      r_2 <- paste0(resp, "_2")

      ## Isolate data from individuals with observations at both BS1 and BS2
      # (as needed for a paired test)
      physio_for_resp <- physio[, c("pit", r_1, r_2)]
      physio_for_resp <- physio_for_resp[complete.cases(physio_for_resp), ]
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
        resp = resp,
        n = nrow(physio_for_resp),
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
  ) %>%
  dplyr::bind_rows()
## Adjust p-values using the Bonferroni correction
p_by_param$`p-value (BC)` <- p.adjust(p_by_param$`p-value`, method = "bonferroni")

#### Write tidy table to file
tidy_numbers(p_by_param, 3) %>%
  tidy_write("./fig/blood_change_tbl_bootstrap.txt")


#### End of code.
################################
################################
