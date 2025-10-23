library(tidyverse)
#### combining all datasets ####
setwd('your working directory of the created datasets')
chemicals <- read.csv('chemicals_all.csv')
death <- read.csv('Mortality_2003_2016.csv')
covariates <- read.csv('covariate.csv')
nutrients <- read.csv('nutrients_all.csv')

data1 <- merge(chemicals, nutrients, by = c('SEQN', 'cycle'), all = TRUE)
data2 <- merge(data1, covariates, by = c('SEQN','cycle'),  all = TRUE)
data3 <- merge(data2, death,  by = 'SEQN',  all = TRUE)

analysis <- data3 %>% mutate(female = case_when(sex == 1 ~ 0,
                                                   sex == 2 ~ 1,
                                                   TRUE ~ NA_integer_),
                                educ_adult = case_when(educ_adult %in% c(7,9) ~ NA_integer_,
                                                       TRUE ~ educ_adult),
                                # educ_adult 1-less than middle school, 2-middle school, 3-high school degree, 4-college degree, 5- college graduate or above
                                race2 = case_when(race %in% c(1,2) ~ 'Hispanic',
                                                  race == 3 ~ 'Non-Hispanic White',
                                                  race == 4 ~ 'Non-Hispanic Black',
                                                  race == 5 ~ 'Other')) %>% rename(death = mortstat,
                                                                                   code_ICD = ucod_leading,
                                                                                   fu_mon = permth_exm)
analysis <- analysis %>%
  mutate(code_ICD = as.character(code_ICD)) %>%
  mutate(code_ICD = case_when(
    code_ICD == "1" ~ "Heart Diseases",
    code_ICD == "2" ~ "Malignant Neoplasms",
    code_ICD == "3" ~ "Chronic Lower Respiratory Diseases",
    code_ICD == "4" ~ "Accidents (Unintentional Injuries)",
    code_ICD == "5" ~ "Cerebrovascular Diseases",
    code_ICD == "6" ~ "Alzheimer Disease",
    code_ICD == "7" ~ "Diabetes",
    code_ICD == "8" ~ "Influenza and Pneumonia",
    code_ICD == "9" ~ "Nephritis, Nephrotic syndrome and Nephrosis",
    code_ICD == "10" ~ "All Other Causes",
    TRUE ~ code_ICD  # Default case, retain original value
  ))

analysis$cvd_death <- ifelse(analysis$death == 1 & analysis$code_ICD %in% c('Cerebrovascular Diseases','Heart Diseases'),1,0)
analysis$cancer_death <- ifelse(analysis$death ==1 & analysis$code_ICD == 'Malignant Neoplasms' ,1,0)
analysis$injury_death <- ifelse(analysis$death ==1 & analysis$code_ICD=="Accidents (Unintentional Injuries)", 1,0)

phthalates <- c('MEP','MBP','MIBP','MBzP','MEHP','MEHHP','MEOHP','MECPP')
phenols <- c("BPA" )
chemical_var <- c(phthalates, phenols)

log_phthalates <- paste0("log_", phthalates)
log_phenols <- paste0("log_", phenols)
log_chemical_var <- paste0("log_", chemical_var)

for (var in chemical_var) {
  analysis[[paste0("log_", var)]] <- log(analysis[[var]])
}


#### fill-in imputation for values < LOD using log-normal distribution ####
library(fitdistrplus)
library(msm)

chemical <- c('MEP','MBP','MIBP','MBzP','MEHP','MEHHP','MEOHP','MECPP','BPA')

impute_chemical <- matrix(NA, nrow=nrow(analysis), ncol=length(chemical))
colnames(impute_chemical) <- chemical

set.seed(123)
for (a in chemical) {  
  LOD_var <- paste0(a, '_LOD')  
  log_chemical <- paste0('log_', a)  
  
  analysis_complete <- analysis[!is.na(analysis[[a]]) & !is.na(analysis[[LOD_var]]),]
  d <- data.frame(
    left= ifelse(analysis_complete[[LOD_var]]==1, NA, analysis_complete[[log_chemical]]),   # Assign left side as NA when censored, LOD variable == 1 represents below LOD
    right=ifelse(analysis_complete[[LOD_var]]==1, min(analysis_complete[[log_chemical]])*sqrt(2), analysis_complete[[log_chemical]]) # Assign right side as LOD when censored
  )
  params <- fitdistcens(d, distr='norm')
  
  imputed_data <- analysis[[log_chemical]]  
  for (i in which(analysis[[LOD_var]] == 1)) {  
    imputed_data[i] <- rtnorm(1, mean = params$estimate['mean'], sd = params$estimate['sd'],  
                              lower = -5, upper = min(analysis_complete[[log_chemical]])*sqrt(2)) 
  }
  impute_chemical[, a] <- imputed_data  
}

new_analysis <- cbind(analysis, impute_chemical)
colnames(new_analysis) <- c(colnames(analysis), paste0('log_',colnames(impute_chemical),'_imp'))

exp_data <- matrix(NA, ncol=length(chemical), nrow=nrow(new_analysis))
colnames(exp_data) <- paste0(colnames(impute_chemical),'_imp')
for (a in colnames(new_analysis[ ,251:278])) {
  new_var_name <- gsub('log_', '', a)
  exp_data[,new_var_name] <- exp(new_analysis[[a]])
}
new_analysis2 <- cbind(new_analysis, exp_data)

## CAS standardization ##
new_analysis2$log_crt <- log(new_analysis2$creatinine)
model <- lm(log_crt ~ age + female + bmi, data = new_analysis2)
new_analysis2$log_crt_predict <- predict(model, newdata = new_analysis2)
new_analysis2$crt_ratio <- exp(new_analysis2$log_crt_predict) / exp(new_analysis2$log_crt)

cratio_chemical <- paste0(chemical,'_imp')

new_var_list <- lapply(cratio_chemical, function(var) {
  new_analysis2[[var]] * new_analysis2$crt_ratio  
})

new_var_df <- as.data.frame(new_var_list)
names(new_var_df) <- paste0(cratio_chemical, "_cratio")

for (var in paste0(cratio_chemical, "_cratio")) {
  new_var_df[[paste0("log_", var)]] <- log(new_var_df[[var]])
}
new_analysis3 <-cbind(new_analysis2, new_var_df) 

##### imputation for covariates by random forest ####
# in imputation here, restrict to no pregnancy/cvd/malignancy, adults only
new_analysis4 <- new_analysis3 %>% filter(age>=20) 
new_analysis4 <- new_analysis4 %>% filter((is.na(pregnant) | pregnant != 1)) %>% filter(! (chd==1 | angina == 1 | heart_attack==1 | stroke ==1 | cancer == 1)) 
exposure_outcome_var <- c('log_MEHP_imp_cratio', 'log_MEHHP_imp_cratio','log_MEOHP_imp_cratio','log_MECPP_imp_cratio',
                          'log_MEP_imp_cratio','log_MBP_imp_cratio','log_MIBP_imp_cratio','log_MBzP_imp_cratio','log_BPA_imp_cratio',
                          'death','cancer_death','cvd_death','fu_mon')
new_analysis5 <- new_analysis4 %>% drop_na(exposure_outcome_var) 

# only keeping predictors in the dataset
cov_to_imp <- c('age', 'female', 'educ_adult', 'race2', 'bmi','cycle','cotinine','income_ratio',
                'AHEI_ALL','alco','mid_year','MET_quar','marital','creatinine')
data_to_impute <- new_analysis5[,c(cov_to_imp,exposure_outcome_var)]

# modify categorical variables to factor
data_to_impute$female <- as.factor(data_to_impute$female)
data_to_impute$educ_adult <- as.factor(data_to_impute$educ_adult)
data_to_impute$race2 <- as.factor(data_to_impute$race2)
data_to_impute$cycle <- as.factor(data_to_impute$cycle)
data_to_impute$mid_year <- as.factor(data_to_impute$mid_year)
data_to_impute$MET_quar <- as.factor(data_to_impute$MET_quar)
data_to_impute$marital <- as.factor(data_to_impute$marital)
data_to_impute$death <- as.factor(data_to_impute$death)
data_to_impute$cancer_death <- as.factor(data_to_impute$cancer_death)
data_to_impute$cvd_death <- as.factor(data_to_impute$cvd_death)

set.seed(123)
library(missForest)
imputed_data <- missForest(data_to_impute, verbose = TRUE, ntree=50)

imputed_cov <- imputed_data$ximp
colnames(imputed_cov) <- paste0(colnames(imputed_cov),'_imp')

cov_to_imp_new_name <- paste0(cov_to_imp,'_imp')

analysis_2 <- cbind(new_analysis5, imputed_cov[,cov_to_imp_new_name])

# write.csv(analysis_2,file='complete_data_impute.csv', row.names = FALSE)

