remotes::install_github("ang-yu/ria.test", upgrade = "never")
library(ria.test)

draw_truncated_normal <- function(mu) {
  p <- runif(length(mu), min = pnorm(-1 - mu), max = pnorm(1 - mu))
  mu + qnorm(p)
}

simulate_data <- function(n, scenario) {
  X1 <- rbeta(n, 2, 3)
  X2 <- rbeta(n, 2, 3)
  X3 <- rbeta(n, 2, 3)
  A <- rbinom(n, 1, plogis(0.5 * X1 + 0.5 * X2 - 1))

  if (scenario$binary_pair) {
    U_L <- runif(n)
    L1 <- as.integer(U_L < 0.2 + 0.6 * A)
  } else {
    L1 <- draw_truncated_normal(-0.4 + 0.5 * A + 0.2 * X3^2)
  }
  L2 <- draw_truncated_normal(0.2 - 0.5 * A + 0.5 * sin(X2))

  if (scenario$binary_pair) {
    U_M <- runif(n)
    M1 <- as.integer(U_M < 0.25 + 0.5 * L1)
  } else {
    M1 <- draw_truncated_normal(-0.5 + 0.4 * L1 + 0.6 * A + 0.4 * X2 + 0.2 * X3)
  }
  M2 <- draw_truncated_normal(-0.5 + 0.4 * L2 + 0.6 * A + 0.4 * X1 + 0.2 * X3)

  h <- ifelse(A == 0, scenario$h0, scenario$h1)
  Y <- 0.2 * (M1 + M2) + 0.3 * (L1 + L2) + 0.4 * A - 0.5 * cos(X1) - 1.5 +
    h * (L1 * M1) + rnorm(n)
  data.frame(X1, X2, X3, A, L1, L2, M1, M2, Y)
}

run_ria_study <- function(repetitions = 200L, n = 1500L) {
  design <- data.frame(
    scenario = c("Liu original", "Reinforcement", "Partial cancellation"),
    binary_pair = c(FALSE, TRUE, TRUE),
    h0 = c(0, -0.625, 3.125),
    h1 = c(0, 5, 5)
  )
  # Exact gaps for the binary alternatives; both gaps are zero in the additive Liu model.
  design$a <- 0.06 * design$h1                        # NIE - NIE^R
  design$b <- 0.02 * design$h1 - 0.08 * design$h0     # NDE - NDE^R

  jobs <- expand.grid(s = seq_len(nrow(design)), r = seq_len(repetitions))
  previous_plan <- future::plan(future::multisession, workers = 6L)
  on.exit(future::plan(previous_plan), add = TRUE)

  fit_repetition <- function(s, r) {
    scenario <- design[s, ]
    torch::torch_set_num_threads(1L)
    seed <- (r - 1L) * nrow(design) + s
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    dat <- simulate_data(n, scenario)
    fit <- suppressMessages(ria.test::ria.test(
      data = dat, trt = "A", out = "Y", med = c("M1", "M2"),
      pre = c("X1", "X2", "X3"), post = c("L1", "L2"),
      d0 = function(data, trt) rep(0, nrow(data)),
      d1 = function(data, trt) rep(1, nrow(data))
    ))
    estimates <- ria.test::tidy(fit)
    effect <- subset(estimates, estimand == "TE - TE^R")
    if (nrow(effect) != 1L || !is.finite(effect$estimate) ||
        !is.finite(effect$std.error) || effect$std.error <= 0)
      stop("Invalid estimate or SE in ", scenario$scenario, ", repetition ", r, ".")
    detail <- data.frame(scenario = scenario$scenario, repetition = r, seed = seed, n = n,
                         h0 = scenario$h0, h1 = scenario$h1, true_Delta = scenario$a + scenario$b,
                         estimates)
    done(sprintf("(%s, repetition %d/%d)", scenario$scenario, r, repetitions))
    detail
  }

  fits <- progressr::with_progress({
    done <- progressr::progressor(steps = nrow(jobs))
    future.apply::future_mapply(fit_repetition, jobs$s, jobs$r, future.seed = NULL, future.stdout = FALSE,
                                future.chunk.size = 1L, future.packages = "ria.test", SIMPLIFY = FALSE)
  }, handlers = progressr::handler_progress(format = "Completed :current/:total fits :message",
                                            clear = FALSE, enable = TRUE), enable = TRUE)

  iterations <- do.call(rbind, fits)
  write.csv(iterations, "ria_test_simulation_iterations.csv", row.names = FALSE)
  target <- subset(iterations, estimand == "TE - TE^R")
  results <- design[c("scenario", "a", "b")]
  results$Delta <- results$a + results$b
  for (s in seq_len(nrow(design))) {
    rows <- target$scenario == design$scenario[s]
    estimate <- target$estimate[rows]
    se <- target$std.error[rows]
    error <- estimate - results$Delta[s]
    bias <- mean(error)
    results[s, "bias"] <- bias
    results[s, "sqrt_n_bias"] <- sqrt(n) * bias
    results[s, "n_MSE"] <- n * mean(error^2)
    half_width <- qnorm(0.975) * se
    results[s, "coverage_95"] <- mean(abs(error) <= half_width)
    results[s, "rejection_rate"] <- mean(abs(estimate) > half_width)
  }
  write.csv(results, "ria_test_simulation_summary.csv", row.names = FALSE)
  invisible(results)
}

run_ria_study()
