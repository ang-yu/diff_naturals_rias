
library(foreign)
library(sl3)
library(ranger)
remotes::install_github('ang-yu/ria_test')
library(crumble)
library(mlr3extralearners)

data <- read.dta("/Users/Ang/Desktop/Research/Cross-world_estimands/ICPSR_34563/DS0001/34563-0001-Zipped_package/mto_sci_puf_pseudo_20130206.dta")

# The variable names follow the notation of Diaz et al. (2021)
Wnames <- c( "ps_x_f_ad_edgradhs",  "ps_x_f_ad_nevmarr","ps_x_f_ad_parentu18",
          "ps_x_f_ad_working", "ps_x_f_hh_afdc" , "ps_x_f_hh_disabl", "ps_x_f_hh_size2", "ps_x_f_hh_size3", "ps_x_f_hh_size4",
          "ps_x_f_hood_unsafenit",  "ps_x_f_hood_verydissat", "ps_x_f_hous_mov3tm","ps_x_f_hous_movschl", "ps_x_f_hous_sec8bef")

Mnames <- c("ps_f_c9010t_perpov_dw", "ps_f_spl_moves_n")

Aname <- "ra_poolgrp_exps8"
Zname <- "ps_f_svy_cmove"
Yname <- "ps_f_mh_idx_z_ad"

alldat <- data[data$ra_site!=1, c(Wnames,Aname, Zname, Mnames, Yname)]

sd(alldat[,Yname]) # the outcome is standardized already

# drop 3 rows with missing values
sum(is.na(alldat))
alldat <- alldat[complete.cases(alldat), ]

set.seed(1)

test <- crumble(
  data = alldat,
  trt = Aname, 
  outcome = Yname,
  covar = Wnames,
  mediators = Mnames,
  moc = Zname,
  d0 = \(data, trt) factor(rep(levels(alldat[,Aname])[1], nrow(data)), levels = levels(alldat[,Aname])), 
  d1 = \(data, trt) factor(rep(levels(alldat[,Aname])[2], nrow(data)), levels = levels(alldat[,Aname])), 
  effect = "Te",
  learners = c("mean", "glm", "ranger"), 
  nn_module = sequential_module(),
  control = crumble_control(crossfit_folds = 2L, mlr3superlearner_folds = 5L, zprime_folds = 5L, epochs = 10L)
)

test
