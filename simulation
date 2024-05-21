
library(ggplot2)

sim <- function(v) {
  set.seed(102)
  A <- rbinom(100000, 1, 0.5)
  
  L_epsilon <- rnorm(100000, 0, v)
  L_0 <- L_epsilon
  L_1 <- 1 + L_epsilon
  
  M_epsilon <- rnorm(100000, 0, 1)
  M_0 <- L_0 + M_epsilon
  M_1 <- -1 + L_1 + cos(L_1) + M_epsilon
  G_0 <- sample(M_0, 100000, replace = FALSE)
  G_1 <- sample(M_1, 100000, replace = FALSE)
  
  Y_epsilon <- rnorm(100000, 0, 1)
  Y_1M_1 <- 1 + L_1 + M_1 + L_1*M_1 + Y_epsilon
  Y_1M_0 <- 1 + L_1 + M_0 + L_1*M_0 + Y_epsilon
  
  Y_1G_1 <- 1 + L_1 + G_1 + L_1*G_1 + Y_epsilon
  Y_1G_0 <- 1 + L_1 + G_0 + L_1*G_0 + Y_epsilon
  
  NIE <- mean(Y_1M_1-Y_1M_0)
  NIER <- mean(Y_1G_1-Y_1G_0)
  
  return(c(NIE, NIER))
}

parameter_seq <- seq(0, 5, 0.001)
results <- as.data.frame( t(sapply(parameter_seq, sim)) )
results$p <- parameter_seq
colnames(results)[1:2] <- c("NIE", "NIER")

shading_data <- results[which(results$NIE < 0)[1]:4946,]

figure <- ggplot(data = results, aes(x = p)) +
  geom_line(aes(y = NIE, colour = "NIE", linetype = "NIE")) +  
  geom_line(aes(y = NIER, colour = "NIER", linetype = "NIER")) +  
  labs(title = "Plot of y = NIE and y = NIER", 
       x = "X axis", y = "Y axis") +
  scale_colour_manual(values = c("NIE" = "blue", "NIER" = "red"),
                      name = "Estimands", labels = c("NIE", "NIER")) +
  scale_linetype_manual(values = c("NIE" = "solid", "NIER" = "dashed"),
                        name = "Estimands", labels = c("NIE", "NIER")) +
  theme_minimal() +
  geom_ribbon(data=shading_data, aes(ymin=pmin(NIE, NIER), ymax=pmax(NIE, NIER)), fill = "gray", alpha = 1) +
  geom_hline(yintercept=0, color="black") +
  #annotate("point", x = results$p[which(results$NIE < 0)[1]], y = results$NIER[which(results$NIE < 0)[1]], color = "black", size = 2) +
  labs(title = NULL, x = "Parameter value", y = NULL) +
  theme(plot.background = element_rect(fill = "white", colour = "white")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())


