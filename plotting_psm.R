library(MatchIt)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(gridExtra)

library(effsize)

numeric_cov = c('age', 'bmi', 'apache',  'admission_gfr', 'creat_before', 'lact_before', 'SatO2', 'map')
plot_vars <- numeric_cov
plot_vars_name <- c('Patient age', 'Patient BMI', 'APACHE II Score', 'GFR at admission', 'Creatinin Day 0', 'Lactate Day 0', 'SatO2 Day 0', 'MAP Day 0')
df_psm <- read.csv("C:\\Users\\MartinFaltys\\OneDrive - Faltys\\Desktop\\matched.csv")
df_ana <- read.csv("C:\\Users\\MartinFaltys\\OneDrive - Faltys\\Desktop\\total.csv")

df_ana[is.na(df_ana$albumina), 'albumina'] = 0

#convert categorical variables to factors
df_ana$albumina = as.factor(df_ana$albumina)
  
std_errors <- matrix(ncol=3, nrow=length(plot_vars)*2)
for(i in 1:length(numeric_cov)){
  v = plot_vars[i]
  std_errors[i,1] <- plot_vars_name[i]
  std_errors[i,2] <- 'Before matching'
  std_errors[i,3] <- abs(cohen.d(unlist(df_ana[, v]) ~ unlist(df_ana[,'albumina']))$estimate) * 100
}

for(i in 1:length(numeric_cov)){
  v = plot_vars[i]
  std_errors[length(plot_vars)+i,1] <- plot_vars_name[i]
  std_errors[length(plot_vars)+i,2] <- 'After matching'
  std_errors[length(plot_vars)+i,3] <- abs(cohen.d(unlist(df_psm[, v]) ~ unlist(df_psm[,'albumina']))$estimate) * 100
}
output <- data.frame(std_errors)
output$X3 <- as.numeric(as.character(output$X3))

ggplot(output) +
  geom_point(aes(x = X3, y = reorder(X1, X3, max), color=X2)) + 
  labs(colour = NULL, title="Matching quality", subtitle=paste("Total per group:", nrow(df_psm)/2)) +
  xlab("Standardized difference") +
  ylab("Covariates") +
  geom_vline(xintercept = 10, linetype="dotted", color = "blue", size=0.4) +
  geom_vline(xintercept = 0, size=0.2) +
  theme(legend.position="top")

