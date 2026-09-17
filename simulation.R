library(ggplot2)
library(ggpubr)

# L = -1 + A + epsilon_L, epsilon_L ~ N(0, 4).
# M = 1{U_M < plogis(-3 + A + b*L)}, U_M ~ Uniform(0, 1).
# Y = -M + L*M + A*L*M + epsilon_Y, epsilon_Y ~ N(0, 1).
# Errors are independent and shared across counterfactual assignments.

sim <- function(b, n = 200000) {
  set.seed(1)
  
  L_epsilon <- rnorm(n, 0, 2)
  L_0 <- -1 + L_epsilon
  L_1 <- L_epsilon
  
  M_epsilon <- runif(n)
  M_0 <- as.integer(M_epsilon < plogis(-3 + b*L_0))
  M_1 <- as.integer(M_epsilon < plogis(-3 + 1 + b*L_1))
  G_0 <- sample(M_0, n, replace = FALSE)
  G_1 <- sample(M_1, n, replace = FALSE)
  
  Y_epsilon <- rnorm(n, 0, 1)
  Y_1M_1 <- -M_1 + L_1*M_1 + L_1*M_1 + Y_epsilon
  Y_1M_0 <- -M_0 + L_1*M_0 + L_1*M_0 + Y_epsilon
  Y_0M_0 <- -M_0 + L_0*M_0 + Y_epsilon
  
  Y_1G_1 <- -G_1 + L_1*G_1 + L_1*G_1 + Y_epsilon
  Y_1G_0 <- -G_0 + L_1*G_0 + L_1*G_0 + Y_epsilon
  Y_0G_0 <- -G_0 + L_0*G_0 + Y_epsilon
  
  NIE <- mean(Y_1M_1-Y_1M_0)
  NIER <- mean(Y_1G_1-Y_1G_0)
  
  NDE <- mean(Y_1M_0-Y_0M_0)
  NDER <- mean(Y_1G_0-Y_0G_0)
  
  return(c(NIE, NIER, NDE, NDER))
}

parameter_seq <- seq(-3, 3, 0.025)
results <- as.data.frame(t(sapply(parameter_seq, sim)))
results$p <- parameter_seq
colnames(results)[1:4] <- c("NIE", "NIER", "NDE", "NDER")

# Keep separate sign-reversal intervals from being joined by the ribbon.
results$region_ie <- cumsum(c(TRUE, diff(results$NIE * results$NIER < 0) == 1))
results$region_de <- cumsum(c(TRUE, diff(results$NDE * results$NDER < 0) == 1))

figure_ie <- ggplot(data = results, aes(x = p)) +
  geom_ribbon(data = results[which(results$NIE * results$NIER < 0), ],
              aes(ymin = pmin(NIE, NIER), ymax = pmax(NIE, NIER), group = region_ie),
              fill = "gray", alpha = 1) +
  geom_hline(yintercept = 0, colour = "gray50", linewidth = 0.3) +
  geom_line(aes(y = NIE, colour = "NIE", linetype = "NIE")) +  
  geom_line(aes(y = NIER, colour = "NIE^R", linetype = "NIE^R")) +  
  scale_colour_manual(values = c("NIE" = "blue", "NIE^R" = "red"),
                      name = "Estimands", labels = expression(NIE, NIE^R)) +
  scale_linetype_manual(values = c("NIE" = "solid", "NIE^R" = "dashed"),
                        name = "Estimands", labels = expression(NIE, NIE^R)) +
  theme_minimal() +
  labs(title = NULL, x = "b value", y = NULL) +
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        plot.margin = margin(14, 8, 6, 14)) 


figure_de <- ggplot(data = results, aes(x = p)) +
  geom_ribbon(data = results[which(results$NDE * results$NDER < 0), ],
              aes(ymin = pmin(NDE, NDER), ymax = pmax(NDE, NDER), group = region_de),
              fill = "gray", alpha = 1) +
  geom_hline(yintercept = 0, colour = "gray50", linewidth = 0.3) +
  geom_line(aes(y = NDE, colour = "NDE", linetype = "NDE")) +  
  geom_line(aes(y = NDER, colour = "NDE^R", linetype = "NDE^R")) +  
  scale_colour_manual(values = c("NDE" = "blue", "NDE^R" = "red"),
                      name = "Estimands", labels = expression(NDE, NDE^R)) +
  scale_linetype_manual(values = c("NDE" = "solid", "NDE^R" = "dashed"),
                        name = "Estimands", labels = expression(NDE, NDE^R)) +
  theme_minimal() +
  labs(title = NULL, x = "b value", y = NULL) +
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        plot.margin = margin(14, 8, 6, 14)) 

plot <- ggarrange(figure_ie, figure_de,
          labels = c("(a)", "(b)"),
          ncol = 2, nrow = 1)

ggsave(paste("/Users/Ang/Desktop/Research/Cross-world_estimands/sim_figure",".jpg", sep=""), plot, width=9, height=3.5)
