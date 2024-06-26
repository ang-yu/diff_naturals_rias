
library(foreign)
library(medoutcon)
library(sl3)

data <- read.dta("/Users/Ang/Desktop/Research/Cross-world_estimands/ICPSR_34563/DS0001/34563-0001-Zipped_package/mto_sci_puf_pseudo_20130206.dta")

# The variable names follow the notation of Diaz et al. (2021)
Wnames <- c( "ps_x_f_ad_edgradhs",  "ps_x_f_ad_nevmarr","ps_x_f_ad_parentu18",
          "ps_x_f_ad_working", "ps_x_f_hh_afdc" , "ps_x_f_hh_disabl", "ps_x_f_hh_size2", "ps_x_f_hh_size3", "ps_x_f_hh_size4",
          "ps_x_f_hood_unsafenit",  "ps_x_f_hood_verydissat", "ps_x_f_hous_mov3tm","ps_x_f_hous_movschl", "ps_x_f_hous_sec8bef")

Mnames <- c("ps_f_c9010t_perpov_dw", "ps_f_spl_moves_n")

Aname <- "ra_poolgrp_exps8"
Zname <- "ps_f_svy_cmove"
Yname <- "ps_f_mh_idx_z_ad"

alldat <- data[data$ra_site!=1, c(Wnames,Aname, Zname, Mnames, Yname) ]

# drop 3 rows with missing values
sum(is.na(alldat))
alldat <- alldat[complete.cases(alldat), ]

W <- alldat[, Wnames]
M <- alldat[, Mnames]
A <- alldat[, Aname]
Z <- alldat[, Zname]
Y <- alldat[, Yname]

Z <- ifelse(Z < 0.5, 0, 1)    
# The Z variable in the dataset is a pseudo continuous variable, so we binarize it to its original scale
A <- as.numeric(A)-1

set.seed(1)

# Random Forests nuisance function learners
rf_lrnr <- Lrnr_ranger$new(num.trees = 200)  # Random Forests 

# NDE^R
ria_de <- medoutcon(
  W = W, A = A, Z = Z, M = M, Y = Y,
  g_learners = rf_lrnr,
  h_learners = rf_lrnr,
  b_learners = rf_lrnr,
  q_learners = rf_lrnr,
  r_learners = rf_lrnr,
  effect = "direct",
  estimator = "onestep",
  estimator_args = list(cv_folds = 2)
)

# NIE^R
ria_ie <- medoutcon(
  W = W, A = A, Z = Z, M = M, Y = Y,
  g_learners = rf_lrnr,
  h_learners = rf_lrnr,
  b_learners = rf_lrnr,
  q_learners = rf_lrnr,
  r_learners = rf_lrnr,
  effect = "direct",
  estimator = "onestep",
  estimator_args = list(cv_folds = 2)
)

# NDE
natural_de <- medoutcon(
  W = W, A = A, Z = NULL, M = M, Y = Y,
  g_learners = rf_lrnr,
  h_learners = rf_lrnr,
  b_learners = rf_lrnr,
  q_learners = rf_lrnr,
  r_learners = rf_lrnr,
  effect = "direct",
  estimator = "onestep",
  estimator_args = list(cv_folds = 2)
)

# NIE
natural_ie <- medoutcon(
  W = W, A = A, Z = NULL, M = M, Y = Y,
  g_learners = rf_lrnr,
  h_learners = rf_lrnr,
  b_learners = rf_lrnr,
  q_learners = rf_lrnr,
  r_learners = rf_lrnr,
  effect = "direct",
  estimator = "onestep",
  estimator_args = list(cv_folds = 2)
)

# TE-TE^R
point <- natural_de$theta + natural_ie$theta - ria_de$theta - ria_ie$theta  # point estimate
point
se <- stats::var(natural_de$eif + natural_ie$eif - ria_de$eif - ria_ie$eif)/length(A)   # se
(1-stats::pnorm(abs(point/se)))*2   # p value
point - stats::qnorm(1-0.05/2)*se   # lower CI
point + stats::qnorm(1-0.05/2)*se   # upper CI

# TE
natural_de$theta + natural_ie$theta  - stats::qnorm(1-0.05/2)*stats::var(natural_de$eif + natural_ie$eif )/length(A)
natural_de$theta + natural_ie$theta  + stats::qnorm(1-0.05/2)*stats::var(natural_de$eif + natural_ie$eif )/length(A)

# TE^R
ria_de$theta + ria_ie$theta  - stats::qnorm(1-0.05/2)*stats::var(ria_de$eif + ria_ie$eif )/length(A)
ria_de$theta + ria_ie$theta  + stats::qnorm(1-0.05/2)*stats::var(ria_de$eif + ria_ie$eif )/length(A)

