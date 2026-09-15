library(ggplot2)
library(ggpubr)

# DGP: L = A + epsilon_L; M = 1 - A + L + 3*A*L + epsilon_M;
# Y = 2*A + L + 2*M + L*M + b*A*L*M + epsilon_Y.
# Errors are independent N(0, 1) and shared across counterfactuals.
# Exact effects: NIE = 12 + 6*b; NIER = 9 + 3*b;
# NDE = 4 + 2*b; NDER = 4 + b.

sim <- function(b) {
  set.seed(1)
  
  L_epsilon <- rnorm(100000, 0, 1)
  L_0 <- L_epsilon
  L_1 <- 1 + L_epsilon
  
  M_epsilon <- rnorm(100000, 0, 1)
  M_0 <- 1 + L_0 + M_epsilon
  M_1 <- 1 - 1 + L_1 + 3*L_1 + M_epsilon
  G_0 <- sample(M_0, 100000, replace = FALSE)
  G_1 <- sample(M_1, 100000, replace = FALSE)
  
  Y_epsilon <- rnorm(100000, 0, 1)
  Y_1M_1 <- 2 + L_1 + 2*M_1 + L_1*M_1 + b*L_1*M_1 + Y_epsilon
  Y_1M_0 <- 2 + L_1 + 2*M_0 + L_1*M_0 + b*L_1*M_0 + Y_epsilon
  Y_0M_0 <- L_0 + 2*M_0 + L_0*M_0 + Y_epsilon
  
  Y_1G_1 <- 2 + L_1 + 2*G_1 + L_1*G_1 + b*L_1*G_1 + Y_epsilon
  Y_1G_0 <- 2 + L_1 + 2*G_0 + L_1*G_0 + b*L_1*G_0 + Y_epsilon
  Y_0G_0 <- L_0 + 2*G_0 + L_0*G_0 + Y_epsilon
  
  NIE <- mean(Y_1M_1-Y_1M_0)
  NIER <- mean(Y_1G_1-Y_1G_0)
  
  NDE <- mean(Y_1M_0-Y_0M_0)
  NDER <- mean(Y_1G_0-Y_0G_0)
  
  return(c(NIE, NIER, NDE, NDER))
}

parameter_seq <- seq(-3, 0, 0.01)
results <- as.data.frame( t(sapply(parameter_seq, sim)) )
results$p <- parameter_seq
colnames(results)[1:4] <- c("NIE", "NIER", "NDE", "NDER")

figure_ie <- ggplot(data = results, aes(x = p)) +
  geom_ribbon(data = results[which(results$NIE * results$NIER < 0), ],
              aes(ymin = pmin(NIE, NIER), ymax = pmax(NIE, NIER)),
              fill = "gray", alpha = 1) +
  geom_hline(yintercept = 0, colour = "gray50", linewidth = 0.3) +
  geom_line(aes(y = NIE, colour = "NIE", linetype = "NIE")) +  
  geom_line(aes(y = NIER, colour = "NIE^R", linetype = "NIE^R")) +  
  scale_colour_manual(values = c("NIE" = "blue", "NIE^R" = "red"),
                      name = "Estimands", labels = c("NIE", "NIE^R")) +
  scale_linetype_manual(values = c("NIE" = "solid", "NIE^R" = "dashed"),
                        name = "Estimands", labels = c("NIE", "NIE^R")) +
  theme_minimal() +
  labs(title = NULL, x = "b value", y = NULL) +
  theme(plot.background = element_rect(fill = "white", colour = "white")) 


figure_de <- ggplot(data = results, aes(x = p)) +
  geom_ribbon(data = results[which(results$NDE * results$NDER < 0), ],
              aes(ymin = pmin(NDE, NDER), ymax = pmax(NDE, NDER)),
              fill = "gray", alpha = 1) +
  geom_hline(yintercept = 0, colour = "gray50", linewidth = 0.3) +
  geom_line(aes(y = NDE, colour = "NDE", linetype = "NDE")) +  
  geom_line(aes(y = NDER, colour = "NDE^R", linetype = "NDE^R")) +  
  scale_colour_manual(values = c("NDE" = "blue", "NDE^R" = "red"),
                      name = "Estimands", labels = c("NDE", "NDE^R")) +
  scale_linetype_manual(values = c("NDE" = "solid", "NDE^R" = "dashed"),
                        name = "Estimands", labels = c("NDE", "NDE^R")) +
  theme_minimal() +
  labs(title = NULL, x = "b value", y = NULL) +
  theme(plot.background = element_rect(fill = "white", colour = "white")) 

plot <- ggarrange(figure_ie, figure_de,
          labels = c("(a)", "(b)"),
          ncol = 2, nrow = 1)

ggsave("/Users/Ang/Desktop/sim_figure_direct_interaction.jpg", plot,
       width = 9, height = 3.5)
