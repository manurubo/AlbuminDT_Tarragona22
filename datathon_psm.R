library(MatchIt)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(gridExtra)
library(effsize)


process_missingness_pattern <- function(row) {
  df_ana_filtered <- df_ana
  
  #pattern: 1=value missing, 0=value not missing
  pattern_columns_with_missing_values = col_with_na[selected_missingness_patterns[row,col_with_na] == 1]
  pattern_columns_with_all_values = col_with_na[selected_missingness_patterns[row,col_with_na] == 0]
  pattern_column_set = c(pattern_columns_with_all_values, col_without_na)
  
  #filter out rows which have measurements where they should not have
  for (col in pattern_columns_with_missing_values) {
    df_ana_filtered <- df_ana_filtered[is.na(df_ana_filtered[,col]),]
  }
  
  #filter out rows which do not have measurements
  df_ana_filtered <- df_ana_filtered %>%  # MatchIt does not allow missing values
    select(a_patientid, albumina, one_of(pattern_column_set)) %>%
    na.omit()
  
  #match
  fmla <- as.formula(paste("albumina ~ ", paste(pattern_column_set, collapse= "+")))
  mod_match <- matchit(fmla,
                       method = "nearest", data = df_ana_filtered, distance="logit", caliper=.3)
  dta_m <- match.data(mod_match)
  
  return (list(fmla, dta_m, mod_match))
}

#constants
group_size_cuttoff = 40

#load data
df_ana_total <- read.csv("C:\\Users\\MartinFaltys\\OneDrive - Faltys\\Desktop\\last_database.csv")
df_ana_total[is.na(df_ana_total$albumina), 'albumina'] = 0
df_ana_total[is.na(df_ana_total$tcrr), 'tcrr'] = 0
df_ana_total['cirrosis'] = 1
df_ana_total[is.na(df_ana_total$Cirrosis), 'cirrosis'] = 0

#convert categorical variables to factors
df_ana_total$hospital_coded = as.factor(df_ana_total$hospital_coded)
df_ana_total$patientsex = as.factor(df_ana_total$patientsex)
df_ana_total$albumina = as.factor(df_ana_total$albumina)
df_ana_total$cirrosis = as.factor(df_ana_total$cirrosis)
df_ana_total$tcrr = as.factor(df_ana_total$tcrr)

#calculate aki
df_ana_total$t3_improve <- df_ana_total$creat_later/df_ana_total$creat_before < 1
df_ana_total$t3_aki0 <- df_ana_total$creat_later/df_ana_total$creat_before >= 1 & df_ana_total$creat_later/df_ana_total$creat_before < 1.5
df_ana_total$t3_aki1 <- df_ana_total$creat_later/df_ana_total$creat_before >= 1.5 & df_ana_total$creat_later/df_ana_total$creat_before < 2
df_ana_total$t3_aki2 <- df_ana_total$creat_later/df_ana_total$creat_before >= 2 & df_ana_total$creat_later/df_ana_total$creat_before < 3
df_ana_total$t3_aki3 <- df_ana_total$creat_later/df_ana_total$creat_before >= 3 | df_ana_total$tcrr == 1
df_ana_total$t3_aki0_or_improve <- df_ana_total$t3_improve | df_ana_total$t3_aki0

df_ana_total$t0_improve <- df_ana_total$creat_before/df_ana_total$first_creat < 1
df_ana_total$t0_aki0 <- df_ana_total$creat_before/df_ana_total$first_creat >= 1 & df_ana_total$creat_before/df_ana_total$first_creat < 1.5
df_ana_total$t0_aki1 <- df_ana_total$creat_before/df_ana_total$first_creat >= 1.5 & df_ana_total$creat_before/df_ana_total$first_creat < 2
df_ana_total$t0_aki2 <- df_ana_total$creat_before/df_ana_total$first_creat >= 2 & df_ana_total$creat_before/df_ana_total$first_creat < 3
df_ana_total$t0_aki3 <- df_ana_total$creat_before/df_ana_total$first_creat >= 3 | df_ana_total$tcrr == 1
df_ana_total$t0_aki0_or_improve <- df_ana_total$t3_improve | df_ana_total$t3_aki0

df_ana_total$admission_gfr <- (140-df_ana_total$age)*df_ana_total$weight/(72*df_ana_total$first_creat) *0.85
df_ana_total$admission_gfr_normal <- df_ana_total$admission_gfr > 60
df_ana_total$admission_gfr_decreased <- df_ana_total$admission_gfr <= 60
df_ana_total$map <- df_ana_total$PAM
df_ana_total[is.na(df_ana_total$PAM), 'map'] = df_ana_total[is.na(df_ana_total$PAM), 'PAM_NI']

df_ana_total$exclude_creatinin <- is.na(df_ana_total$creat_before) | is.na(df_ana_total$creat_later)
print (sum(df_ana_total$exclude_creatinin, na.rm = TRUE )) #number of patient excluded

df_ana_total <- df_ana_total[df_ana_total$exclude_creatinin == FALSE,]
print (nrow(df_ana_total))
print (sum(df_ana_total$albumina == 1, na.rm = TRUE ))

write.csv(df_ana_total,"C:\\Users\\MartinFaltys\\OneDrive - Faltys\\Desktop\\total.csv", row.names = FALSE)

var_set <- c('a_patientid', 'patientsex', 'age', 'bmi', 'cirrosis', 'apache', 'albumina', 'admission_gfr', 'creat_before', 'lact_before', 'map')
df_ana <- df_ana_total %>%  # MatchIt does not allow missing values
  select(one_of(var_set))

###find patterns of missing values
#find columns with na values
col_with_na = colnames(df_ana)[colSums(is.na(df_ana)) > 0]
col_without_na = colnames(df_ana)[colSums(is.na(df_ana)) == 0]

#find all patterns of missingness in the bolus data
n = length(col_with_na)
missingness_pattern = expand.grid(rep(list(0:1), n))
colnames(missingness_pattern) <- col_with_na

for(i in 0:nrow(missingness_pattern)){
  df_ana_filtered <- df_ana[df_ana$albumina==1,]
  for (j in 1:length(col_with_na)) {
    df_ana_filtered <- df_ana_filtered[is.na(df_ana_filtered[,col_with_na[j]]) == as.logical(missingness_pattern[i,j]),]
  }
  missingness_pattern[i,'count'] <- nrow(df_ana_filtered)
}
existing_missingness_patterns = missingness_pattern %>% filter(count>0)
selected_missingness_patterns = existing_missingness_patterns %>% filter(count>=group_size_cuttoff)

print (paste0("Amount of possible patterns: ", 2^n))
print (paste0("Amount of existing patterns: ", nrow(existing_missingness_patterns)))
print (paste0("Lost patients in to small groups: ", sum((existing_missingness_patterns %>% filter(count<=group_size_cuttoff))$count)))
print (selected_missingness_patterns)

#matched for each pattern of missingness and merge to df_total
all_columns = c(var_set)
df_total = as.data.frame(matrix(numeric(),nrow = 0, ncol = length(all_columns)))
colnames(df_total) = all_columns
df_total$hospital_coded <- as.factor(df_total$hospital_coded)
df_total$patientsex = as.factor(df_total$patientsex)
df_total$albumina = as.factor(df_total$albumina)
df_total$cirrosis = as.factor(df_total$cirrosis)

for (i in 1:nrow(selected_missingness_patterns)) {
  results = process_missingness_pattern(i)
  df_total = bind_rows(df_total, results[[2]])
}

#merge with all data from bolus and non bolus
df_m_key_columns = df_total %>%
  select(a_patientid, albumina)

df_total_anal <- merge(df_m_key_columns, rbind(df_ana_total), by=c("a_patientid", "albumina"))
print (nrow(df_total_anal))

write.csv(df_total_anal,"C:\\Users\\MartinFaltys\\OneDrive - Faltys\\Desktop\\matched.csv", row.names = FALSE)

