# Raw BCS70 downloads -> manuscript Table 1 (c048).
# From the BCS70_Klein_Kuehhirt folder: Rscript raw_to_table1.R
# Or supply that folder as an argument: Rscript raw_to_table1.R /path/to/folder
# Requires haven, dplyr, callr, ria.test, mlr3extralearners, and their dependencies.
# ria.test must be commit 9473141f05e1479293416a7b311c822dde5dbcee.
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (length(args)) args[1] else ".")
setwd(root)
stopifnot(dir.exists("raw"))
dir.create("derived", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

# Small helpers for translating the authors' Stata preparation code.
library(haven)
library(dplyr)

read_source <- function(filename, variables = NULL, patterns = NULL) {
  paths <- list.files(file.path(root, "raw"), pattern = "[.]dta$", full.names = TRUE, ignore.case = TRUE)
  path <- paths[tolower(basename(paths)) == tolower(filename)]
  if (length(path) != 1L) stop("Expected one raw file: ", filename)
  header <- read_dta(path, n_max = 0)
  lower <- tolower(names(header))
  wanted <- unique(c("bcsid", variables, lower[grepl(paste(patterns, collapse = "|"), lower) & length(patterns) > 0]))
  absent <- setdiff(wanted, lower)
  if (length(absent)) stop(filename, ": missing variables: ", paste(absent, collapse = ", "))
  cols <- names(header)[lower %in% wanted]
  x <- read_dta(path, col_select = all_of(cols))
  names(x) <- tolower(names(x))
  x <- x |> mutate(across(where(is.labelled), zap_labels)) |> rename(caseid = bcsid)
  x
}

missing_codes <- function(x, codes) replace(x, x %in% codes, NA_real_)
zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# Stata recode preserves values not named in a rule, including negative codes.
recode_ranges <- function(x, lower, upper, values) {
  out <- x
  for (i in seq_along(values)) out[which(x >= lower[i] & x <= upper[i])] <- values[i]
  out
}
row_extreme <- function(a, b, fun) {
  out <- fun(a, b, na.rm = TRUE)
  out[is.na(a) & is.na(b)] <- NA_real_
  out
}
join_parent <- function(x, y) {
  stopifnot(!anyDuplicated(y$caseid), !anyNA(y$caseid))
  left_join(x, y, by = "caseid", relationship = "many-to-one")
}

bas_sum <- function(x) {
  x <- as.matrix(x)
  x[x %in% c(-6, -3, 9)] <- NA_real_
  x[x == 2 & !is.na(x)] <- 0
  out <- rowSums(x, na.rm = TRUE)
  out[rowSums(!is.na(x)) == 0] <- NA_real_
  out
}

# Income periods follow cr_parcharac.do, including the authors' divisors.
weekly_income <- function(x) {
  n <- names(x)
  periods <- c("b7cnetpd", "b7pnetpd", grep("^b7bprd", n, value = TRUE),
               "b7crdprd", "b7crdpr2", grep("^b7iprd", n, value = TRUE))
  amounts <- c("b7cnetpy", "b7pnetpy", grep("^b7bamt", n, value = TRUE),
               "b7tcdamt", "b7tcdam2", grep("^b7iamt", n, value = TRUE))
  stopifnot(length(periods) == length(amounts), length(amounts) >= 28L)
  pay <- as.matrix(x[amounts])
  # The authors convert only the first 28 components; later ones remain unscaled.
  for (i in seq_len(28)) {
    p <- x[[periods[i]]]
    if (i > 2) p <- recode_ranges(p, 3:10, 3:10, c(20, 3, 4, 21, 22, 23, 5, 6))
    p <- missing_codes(p, c(-1, -8, 6))
    divisor <- rep(1, length(p))
    codes <- c(2, 3, 4, 5, 20, 21, 22, 23)
    divisors <- c(2, 4, 4.345, 52.143, 3, 8.690, 13.036, 26.072)
    for (j in seq_along(codes)) divisor[which(p == codes[j])] <- divisors[j]
    value <- x[[amounts[i]]] / divisor
    value[is.na(p) | (!is.na(value) & value <= 0)] <- NA_real_
    pay[, i] <- value
  }
  pay[!is.na(pay) & pay <= 0] <- NA_real_
  out <- rowSums(pay, na.rm = TRUE)
  replace(out, out == 0, NA_real_)
}

# 1. Birth: region and cohort member's birth weight (SN 2666).
birth <- read_source("bcs7072a.dta", "a0278") |>
  transmute(caseid, bweight = missing_codes(a0278, -3:-2))
region <- read_source("bcs1derived.dta", "bd1regn") |> rename(region = bd1regn)

# 2. Age 10: grandparents' resources and cohort member's cognitive ability.
s3 <- read_source("sn3723.dta", c(paste0("c1_", c(1:9, 12:20)),
  "a12_2", "a12_3", "a4a_42", "a4b_22", "d2", "e3_5", "e3_14", "e3_23",
  "c2_17a", "c2_18a", paste0("c9_", 1:8), paste0("i", 3504:3644)))
education <- function(offset) {
  out <- rep(NA_real_, nrow(s3))
  items <- c(9, 1, 2, 3, 4, 5, 6) + offset
  levels <- c(1, 2, 2, 3, 4, 4, 5)
  for (i in seq_along(items)) out[which(s3[[paste0("c1_", items[i])]] == 1)] <- levels[i]
  out
}
grand <- s3 |> transmute(caseid,
  grandfatheduc = education(0), grandmotheduc = education(11),
  grandeduc = pmin(row_extreme(grandfatheduc, grandmotheduc, pmax), 4),
  granddegree = as.numeric(grandeduc >= 4),
  grandeth = as.integer(a12_2 %in% 4:8 | a12_3 %in% 4:8),
  grandhome = recode_ranges(d2, c(1, 3), c(2, 7), c(1, 0)),
  grandhealth = as.integer(e3_5 %in% 1:2 | e3_14 %in% 1:2 | e3_23 %in% 1:2),
  parsib = missing_codes(a4a_42, -8) +
    if_else(is.na(a4b_22) & !is.na(missing_codes(a4a_42, -8)), 0, a4b_22) - 1,
  parsib = recode_ranges(parsib, 4, 12, 4))

# Occupational coding is a separate collection (SN 7023).
father <- read_source("bcs3_occupation_coding_father.dta", "b3fanssec")
mother <- read_source("bcs3_occupation_coding_mother.dta", "b3manssec")
occupation <- s3 |> select(caseid, c2_17a, c2_18a, starts_with("c9_")) |>
  join_parent(father) |> join_parent(mother)
nssec <- function(x) recode_ranges(x, c(2,4.1,7.1,8.1,10,12.1,13.1,14.1),
  c(3.4,5,7.4,9.2,11.2,12.7,13.5,14.2), 1:8)
occupation <- occupation |> mutate(
  grandfathsoc_cat = if_else(c2_17a %in% c(-1, 1:31), 8, nssec(b3fanssec)),
  grandmothsoc_cat = if_else(c2_18a %in% c(-1, 1:45), 8, nssec(b3manssec)),
  grandparclass = row_extreme(grandfathsoc_cat, grandmothsoc_cat, pmin),
  grandparclass = recode_ranges(grandparclass, c(1,3,5), c(2,4,8), 1:3),
  grandinc = c9_1)
for (i in 2:7) occupation$grandinc[which(occupation[[paste0("c9_", i)]] == 1)] <- i
occupation$grandinc <- recode_ranges(occupation$grandinc, 1:7, 1:7,
                                    c(17.5,42,74.5,124.5,174.5,224.5,275))
grand <- grand |> join_parent(select(occupation, caseid, starts_with("grand")))

# Four BAS subtests; a skipped item is zero, but an entirely absent test is NA.
subtests <- data.frame(
  word = bas_sum(s3[paste0("i", 3504:3540)]),
  digits = bas_sum(s3[paste0("i", 3541:3574)]),
  similarities = NA_real_, matrices = bas_sum(s3[paste0("i", 3617:3644)]))
sim <- as.matrix(s3[paste0("i", 3575:3616)])
sim[sim %in% c(-6,-3,9)] <- NA_real_
pairs <- sim[, seq(1,42,2)] == 1 & sim[, seq(2,42,2)] == 1
subtests$similarities <- rowSums(pairs, na.rm = TRUE)
subtests$similarities[rowSums(!is.na(sim)) == 0] <- NA_real_
standard <- as.data.frame(lapply(subtests, function(x) 100 + 15 * zscore(x)))
complete <- complete.cases(standard)
pca <- prcomp(standard[complete, ], center = TRUE, scale. = TRUE)
pc1 <- pca$x[,1] * sign(sum(pca$rotation[,1]))
cognition <- data.frame(caseid = s3$caseid, score20_pca = NA_real_)
cognition$score20_pca[complete] <- zscore(pc1)

# 3. Age 34 variables needed for complete-case selection and age adjustment,
# even though they are not covariates in the RIA test (SN 5585).
s7 <- read_source("bcs_2004_followup.dta", c("b7saveam", "b7save", "b7khlstt",
  "b7ten2", "b7ten", "bd7ns8", sprintf("bd7ns8%02d", 1:11), "bd7ecact",
  "b7plefd2", "b7cnetpd", "b7pnetpd", "b7cnetpy", "b7pnetpy", "b7crdprd",
  "b7crdpr2", "b7tcdamt", "b7tcdam2"), patterns = "^b7(bprd|iprd|bamt|iamt)")
educ7 <- read_source("bcs7derived.dta", "bd7hachq")
parents <- s7 |> join_parent(educ7) |> transmute(caseid,
  pareduc = recode_ranges(bd7hachq, c(0,1,4,6), c(0,3,5,8), 1:4),
  partres = case_when(b7plefd2 == -1 ~ 1,
    between(b7plefd2,14,16) ~ if_else(bd7ns811 %in% 1:2,3,2),
    between(b7plefd2,17,38) ~ if_else(bd7ns811 %in% 1:2,5,4), TRUE ~ NA_real_),
  parsave = case_when(b7save == 2 ~ 0, b7saveam > 0 ~ b7saveam, TRUE ~ NA_real_),
  parinc = weekly_income(s7),
  parhealth = recode_ranges(missing_codes(b7khlstt,c(-9,-8,-7)),4,5,4),
  parhome = case_when(b7ten == 1 ~ 1, b7ten %in% c(4,5,7) ~ 0,
    b7ten2 %in% c(-9,-8,-1) ~ NA_real_, b7ten2 %in% 1:2 ~ 1, TRUE ~ 0),
  parclass = missing_codes(bd7ns8,c(-3,-1)))
for (v in sprintf("bd7ns8%02d",1:10)) {
  use <- is.na(parents$parclass) & !(s7[[v]] %in% -1)
  parents$parclass[use] <- s7[[v]][use]
}
parents$parclass[is.na(parents$parclass) & !(s7$bd7ecact %in% c(-7,-8,1,2,3))] <- 8
parent_data <- grand |> join_parent(cognition) |> join_parent(birth) |>
  join_parent(region) |> join_parent(parents)

# 4. Child outcomes: one row per child, with separate verbal/numerical samples.
children <- read_source("bcs_2004_child_assessment_bas.dta",
  c("childid","age","nmonth","basnva","basenca","baswra","basnsa")) |>
  filter(age >= 3) |> transmute(caseid, childid, age_month = age * 12 + nmonth,
  cognum = if_else(age <= 5,missing_codes(basenca,c(-7,-1)),missing_codes(basnsa,c(-7,-1))),
  cogverb = if_else(age <= 5,missing_codes(basnva,c(-7,-1)),missing_codes(baswra,c(-7,-1))))
data <- children |> join_parent(parent_data)
predictors <- c("score20_pca","pareduc","parclass","parhome","parinc","parsave",
  "parhealth","parsib","partres","granddegree","grandinc","grandparclass",
  "grandhome","grandhealth","region","bweight")
data <- data |> mutate(missnr = rowSums(is.na(pick(all_of(predictors)))),
  sample0 = grandeth == 0 & (is.na(region) | region != 11),
  sample1 = coalesce(sample0 & missnr == 0,FALSE),
  ansamp_cognum = sample1 & !is.na(cognum), ansamp_cogverb = sample1 & !is.na(cogverb))

# Crawford age adjustment: subtract ONLY the intercept and age-month terms.
# Using all regression residuals here would incorrectly remove the covariate effects.
adjustment <- paste("factor(age_month) + score20_pca + factor(pareduc) + factor(parclass)",
  "+ parinc + parsave + factor(parhealth) + parhome + factor(parsib) + factor(partres)",
  "+ factor(grandeduc) + grandinc + factor(grandparclass) + factor(grandhome)",
  "+ factor(grandhealth) + factor(region)")
for (outcome in c("cognum","cogverb")) {
  flag <- paste0("ansamp_",outcome)
  index <- which(data[[flag]])
  fit <- lm(as.formula(paste(outcome,"~",adjustment)),data = data[index, ],na.action = na.fail)
  mm <- model.matrix(fit)
  age_columns <- grepl("^factor\\(age_month\\)",colnames(mm)) | colnames(mm) == "(Intercept)"
  beta <- coef(fit)[age_columns]; beta[is.na(beta)] <- 0
  adjusted <- data[[outcome]][index] - as.vector(mm[,age_columns,drop=FALSE] %*% beta)
  data[[paste0(outcome,"_adj")]] <- NA_real_
  data[[paste0(outcome,"_adj")]][index] <- adjusted
  cutoff <- if (outcome == "cognum") -66 else -78
  data[[flag]][index[adjusted < cutoff]] <- FALSE
}

stopifnot(!anyDuplicated(data[c("caseid","childid")]),
          !anyNA(data$age_month[data$sample1]))
# Retain only identifiers and variables used by the RIA estimator.
variables <- c("caseid", "childid", "granddegree", "region", "bweight",
  "grandhealth", "grandhome", "grandinc", "grandparclass", "score20_pca")
for (outcome in c("verbal", "numerical")) {
  y <- if (outcome == "verbal") "cogverb_adj" else "cognum_adj"
  flag <- if (outcome == "verbal") "ansamp_cogverb" else "ansamp_cognum"
  sample <- data[data[[flag]], c(variables, y)]
  expected <- if (outcome == "verbal") c(1898, 1138) else c(1884, 1133)
  stopifnot(nrow(sample) == expected[1], n_distinct(sample$caseid) == expected[2],
            all(complete.cases(sample)))
  saveRDS(sample, file.path(root, "derived", paste0(outcome, "_sample.rds")))
  cat(outcome, ":", nrow(sample), "children;", n_distinct(sample$caseid), "families\n")
}

# 5. Fit each outcome in a fresh R process to preserve c048's random initialization.
# The function is embedded here: no other R scripts are read or sourced.
for (outcome in c("verbal", "numerical")) {
  cat("Fitting", outcome, "with c048 settings...\n")
  callr::r(function(root, outcome) {
    setwd(root)
    library(ria.test)
    library(mlr3extralearners)
    stopifnot(identical(packageDescription("ria.test")$RemoteSha,
      "9473141f05e1479293416a7b311c822dde5dbcee"))
    x <- as.data.frame(readRDS(file.path("derived", paste0(outcome, "_sample.rds"))))
    y <- if (outcome == "verbal") "cogverb_adj" else "cognum_adj"
    pre <- "region"
    post <- c("bweight", "grandhealth", "grandhome", "grandinc", "grandparclass")
    mediator <- "score20_pca"
    x <- x[c("caseid", "granddegree", pre, post, mediator, y)]
    for (v in c("granddegree", "region", "grandhealth", "grandhome", "grandparclass"))
      x[[v]] <- factor(x[[v]])
    stopifnot(all(complete.cases(x)), identical(levels(x$granddegree), c("0", "1")))
    # Standardize for c048 training; the final section restores score-point units.
    for (v in c("bweight", "grandinc", mediator, y)) x[[v]] <- as.numeric(scale(x[[v]]))

    set.seed(1)
    fit <- ria.test(
      data = x, trt = "granddegree", outcome = y, pre = pre, mediators = mediator,
      post = post, id = "caseid",
      d0 = \(data, trt) factor(rep("0", nrow(data)), levels = levels(data[[trt]])),
      d1 = \(data, trt) factor(rep("1", nrow(data)), levels = levels(data[[trt]])),
      learners = c("mean", "glm", "ranger"),
      nn_module = sequential_module(layers = 1, hidden = 20, dropout = 0.2),
      control = ria.test.control(crossfit_folds = 5L, mlr3superlearner_folds = 10L,
        lprime_folds = 2L, epochs = 10L, learning_rate = 0.001, batch_size = 64,
        device = "cpu", torch_seed = 1L))
    # Adam weight decay is fixed at 0.01 in the pinned package version.
    dir.create("results", showWarnings = FALSE)
    write.csv(as.data.frame(tidy(fit)), file.path("results", paste0(outcome, ".csv")),
              row.names = FALSE)
    if (outcome == "verbal") capture.output(sessionInfo(), file = "results/sessionInfo.txt")
  }, args = list(root = root, outcome = outcome), show = TRUE)
}

# 6. Export Table 1 in age-adjusted test-score points.
# Convert c048 estimates to the original outcome units and export Table 1.
results <- lapply(c("verbal", "numerical"), function(outcome) {
  x <- readRDS(file.path("derived", paste0(outcome, "_sample.rds")))
  y <- if (outcome == "verbal") "cogverb_adj" else "cognum_adj"
  tab <- read.csv(file.path("results", paste0(outcome, ".csv")))
  columns <- c("estimate", "std.error", "conf.low", "conf.high")
  tab[columns] <- tab[columns] * sd(x[[y]])
  cbind(outcome, tab)
})
table1 <- do.call(rbind, results)
write.csv(table1, "results/table1.csv", row.names = FALSE)

labels <- c("TE", "TE$^R$", "$\\text{TE}-\\text{TE}^R$", "NIE$^R$", "NDE$^R$")
tex <- c("\\begin{tabular}{c cc cc}",
  " & \\multicolumn{2}{c}{Verbal ability} & \\multicolumn{2}{c}{Numerical ability} \\\\",
  "Estimand & Estimate & 95\\% CI & Estimate & 95\\% CI \\\\", "\\hline")
for (i in seq_along(labels)) {
  v <- results[[1]][i, ]; n <- results[[2]][i, ]
  line <- sprintf("%s & %.3f & (%.3f, %.3f) & %.3f & (%.3f, %.3f) \\\\",
    labels[i], v$estimate, v$conf.low, v$conf.high, n$estimate, n$conf.low, n$conf.high)
  tex <- c(tex, sub("(-", "($-$", line, fixed = TRUE))
}
writeLines(c(tex, "\\hline", "\\end{tabular}"), "results/table1.tex")
print(table1, row.names = FALSE, digits = 4)
