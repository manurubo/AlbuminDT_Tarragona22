setwd('C:/Users/Maria/Downloads/DATATHON')
# Loading
library("readxl")
# xls files
data1 <- read.csv("C:/Users/Maria/Downloads/DATATHON/matched_final.csv")

#data2 <- data2[data2$Mortality == 1,] #we delete the rows that are not Mortality

#data2 <-
 # data1 %>% 
  #group_by(Patients) %>% 
  #filter(row_number()==1)


#data_new <- data2  
data2 <- data1 %>% 
  mutate(Hospital_LOS = Hospital_LOS / 1440) %>% 
  mutate(ICU_LOS = ICU_LOS / 1440) 


summary(data2)


library(dplyr)
library(survival)
library(survminer)

install.packages("survminer")
library(survivaal)


s <- Surv(data2$Hospital_LOS, data2$Mortality)
class(s)
s
head(data2)

survfit(s~1)
survfit(Surv(Hospital_LOS, Mortality)~1, data=data2)
sfit <- survfit(Surv(Hospital_LOS, Mortality)~1, data=data2)
sfit

summary(sfit)

sfit <- survfit(Surv(Hospital_LOS, Mortality)~Albumin, data=data2)
sfit
summary(sfit)

range(data2$Hospital_LOS)
seq(0, 60, 1)

summary(sfit, times=seq(0, 60, 1))

sfit <- survfit(Surv(Hospital_LOS, Mortality)~Albumin, data=data2)
plot(sfit)

install.packages("ggplot2")
library(survminer)
ggsurvplot(sfit)


# NOT RUN {
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Example 1: Survival curves with two groups
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit survival curves
#++++++++++++++++++++++++++++++++++++
require("survival")
fit<- survfit(Surv(Hospital_LOS, Mortality) ~ Albumin, data = data2)

# Basic survival curves
ggsurvplot(fit, data = data2)

# Customized survival curves
ggsurvplot(fit, data = data2,
           xlim = c(0, 60), #limit to 60 days
           break.time.by = 5, #intervval days
           surv.median.line = "hv", # Add medians survival
           times = 365.25,
           
           # Change legends: title & labels
           legend.title = "Albumin",
           legend.labs = c("No", "Yes"),
           # Add p-value and tervals
           pval = TRUE,
           
           conf.int = TRUE,
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           
           # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
           # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
           palette = c("#E7B800", "#2E9FDF"),
           ggtheme = theme_bw() # Change ggplot2 theme
)


############ ICU survival ###########################

