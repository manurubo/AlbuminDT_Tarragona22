library(ggsankey)
library(ggplot2)
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

df <- df_psm %>% filter(albumina == 1) %>%
  make_long(admission_ckd, before_aki, after_aki)

#no albumin
ggplot(df, aes(x = x, 
              next_x = next_x, 
              node = node, 
              next_node = next_node,
              fill = factor(node))) +
  geom_sankey() +
  theme_sankey(base_size = 16)


#albumin
df <- df_psm %>% filter(albumina == 1) %>%
  make_long(admission_ckd, before_aki, after_aki)

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node))) +
  geom_sankey() +
  theme_sankey(base_size = 16)

