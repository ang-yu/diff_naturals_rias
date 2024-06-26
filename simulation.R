
library(ggplot2)
library(ggpubr)

logit <- function (x) { sapply(x, function (y) {rbinom(1,1,exp(y)/(1+exp(y)))} ) }

sim <- function(b) {
  set.seed(1)
  
  L_epsilon <- rnorm(100000, 0, 1)
  L_0 <- L_epsilon
  L_1 <- 1 + L_epsilon
  
  M_0 <- logit(L_0)
  M_1 <- logit(1 + L_1 + b*L_1)
  G_0 <- sample(M_0, 100000, replace = FALSE)
  G_1 <- sample(M_1, 100000, replace = FALSE)
  
  Y_epsilon <- rnorm(100000, 0, 1)
  Y_1M_1 <- 1 + L_1 + M_1 + L_1*M_1 + Y_epsilon
  Y_1M_0 <- 1 + L_1 + M_0 + L_1*M_0 + Y_epsilon
  Y_0M_0 <- L_0 + M_1 + L_0*M_1 + Y_epsilon
  
  Y_1G_1 <- 1 + L_1 + G_1 + L_1*G_1 + Y_epsilon
  Y_1G_0 <- 1 + L_1 + G_0 + L_1*G_0 + Y_epsilon
  Y_0G_0 <- L_0 + G_0 + L_0*G_0 + Y_epsilon
  
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
  geom_line(aes(y = NIE, colour = "NIE", linetype = "NIE")) +  
  geom_line(aes(y = NIER, colour = "NIER", linetype = "NIER")) +  
  labs(title = "Plot of y = NIE and y = NIER", 
       x = "X axis", y = "Y axis") +
  scale_colour_manual(values = c("NIE" = "blue", "NIER" = "red"),
                      name = "Estimands", labels = c("NIE", "NIER")) +
  scale_linetype_manual(values = c("NIE" = "solid", "NIER" = "dashed"),
                        name = "Estimands", labels = c("NIE", "NIER")) +
  theme_minimal() +
  geom_ribbon(data=results[which(results$NIE < 0 & results$NIER > 0),], aes(ymin=pmin(NIE, NIER), ymax=pmax(NIE, NIER)), fill = "gray", alpha = 1) +
  labs(title = NULL, x = "b value", y = NULL) +
  theme(plot.background = element_rect(fill = "white", colour = "white")) 


figure_de <- ggplot(data = results, aes(x = p)) +
  geom_line(aes(y = NDE, colour = "NDE", linetype = "NDE")) +  
  geom_line(aes(y = NDER, colour = "NDER", linetype = "NDER")) +  
  labs(title = "Plot of y = NDE and y = NDER", 
       x = "X axis", y = "Y axis") +
  scale_colour_manual(values = c("NDE" = "blue", "NDER" = "red"),
                      name = "Estimands", labels = c("NDE", "NDER")) +
  scale_linetype_manual(values = c("NDE" = "solid", "NDER" = "dashed"),
                        name = "Estimands", labels = c("NDE", "NDER")) +
  theme_minimal() +
  labs(title = NULL, x = "b value", y = NULL) +
  theme(plot.background = element_rect(fill = "white", colour = "white")) 

ggarrange(figure_ie, figure_de,
          labels = c("(a)", "(b)"),
          ncol = 2, nrow = 1)
