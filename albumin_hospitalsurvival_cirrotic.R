setwd('C:/Users/Maria/Downloads/DATATHON')
# Loading
library("readxl")
# xls files
data5 <- read.csv("C:/Users/Maria/Downloads/DATATHON/matched_final.csv")

#data7 <- data7[data7$Mortality == 1,] #we delete the rows that are not Mortality

#data7 <-
# data1 %>% 
#group_by(Patients) %>% 
#filter(row_number()==1)


#data_new <- data7  
data6 <- data5 %>% 
  mutate(Hospital_LOS = Hospital_LOS / 1440) %>% 
  mutate(ICU_LOS = ICU_LOS / 1440) 

data7 <- data6[data6$cirrosis == 1,] #we delete the rows that are not cirrosis
summary(data7)


library(dplyr)
library(survival)
library(survminer)

install.packages("survminer")
library(survivaal)


s <- Surv(data7$Hospital_LOS, data7$Mortality)
class(s)
s
head(data7)

survfit(s~1)
survfit(Surv(Hospital_LOS, Mortality)~1, data=data7)
sfit <- survfit(Surv(Hospital_LOS, Mortality)~1, data=data7)
sfit

summary(sfit)

sfit <- survfit(Surv(Hospital_LOS, Mortality)~Albumin, data=data7)
sfit
summary(sfit)

range(data7$Hospital_LOS)
seq(0, 28, 1)

summary(sfit, times=seq(0, 28, 1))

sfit <- survfit(Surv(Hospital_LOS, Mortality)~Albumin, data=data7)
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
fit<- survfit(Surv(Hospital_LOS, Mortality) ~ Albumin, data = data7)

# Basic survival curves
ggsurvplot(fit, data = data7)

# Customized survival curves
ggsurvplot(fit, data = data7,
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

