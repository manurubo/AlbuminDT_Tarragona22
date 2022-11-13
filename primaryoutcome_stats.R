library(dplyr)

df_psm <- read.csv("C:\\Users\\MartinFaltys\\OneDrive - Faltys\\Desktop\\matched.csv")

#generate columns with aki stage
df_psm[df_psm$admission_gfr_normal == TRUE, 'admission_ckd'] <- 0
df_psm[df_psm$admission_gfr_normal == FALSE, 'admission_ckd'] <- 1
df_psm[, 'before_aki'] <- 0
df_psm[df_psm$t0_aki1 == TRUE, 'before_aki'] <- 1
df_psm[df_psm$t0_aki2 == TRUE, 'before_aki'] <- 2
df_psm[df_psm$t0_aki3 == TRUE, 'before_aki'] <- 3
df_psm[, 'after_aki'] <- 0
df_psm[df_psm$t3_aki1 == TRUE, 'after_aki'] <- 1
df_psm[df_psm$t3_aki2 == TRUE, 'after_aki'] <- 2
df_psm[df_psm$t3_aki3 == TRUE, 'after_aki'] <- 3

#generate improve label
df_psm$worsening_kidney_function <- 0
df_psm[df_psm$before_aki < df_psm$after_aki, 'worsening_kidney_function'] <- 1

#add factors
df_psm$albumina = as.factor(df_psm$albumina)
df_psm$worsening_kidney_function = as.factor(df_psm$worsening_kidney_function)

chisq.test(df_psm$albumina, df_psm$worsening_kidney_function)
table(df_psm$albumina,df_psm$worsening_kidney_function)
