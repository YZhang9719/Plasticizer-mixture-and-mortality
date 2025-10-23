##############################
# descriptive and analytical analyses #
##############################
library(tidyverse)
library(survival)
library(gam)
library(mgcv)
library(survey)
library(qgcomp)
library(tableone)

#### cleanning up data & creating analytical dataset ####
analysis <- read.csv(file = 'complete_data_impute.csv')
analysis <- analysis %>% filter(age>=20)  # keep adults
analysis <- analysis %>% filter(cycle %in% c('2005-2006','2007-2008','2009-2010','2011-2012','2013-2014','2015-2016')) # keep 05-16 cycles
analysis <- analysis %>% filter((is.na(pregnant) | pregnant != 1)) %>% filter(! (chd==1 | angina == 1 | heart_attack==1 | stroke ==1 | cancer == 1)) # exclude those pregnant or with CVD/malignancy history
analysis <- analysis %>% drop_na(fu_mon,death) # excluding no valid death status or follow up time data

analysis$educ_adult <- as.factor(analysis$educ_adult)
analysis$race2 <- as.factor(analysis$race2)
analysis$cycle <- as.factor(analysis$cycle)
analysis$alco_45 <- ifelse(analysis$alco >= 45, 1, 0)

# creating sum DEHP variable by four metabolites - MEHP, MEHHP, MEOHP, MECPP
# imputed and CAS adjusted
weights <- c(MEHP_imp_cratio = 278.34, MECPP_imp_cratio = 308.33, MEHHP_imp_cratio = 294.34, MEOHP_imp_cratio = 292.33)
analysis$sum_DEHP_imp_cratio <- (rowSums(analysis[, names(weights)] / weights, na.rm = FALSE))*390.56
# not adjusted for CAS
weights_2 <- c(MEHP = 278.34, MECPP = 308.33, MEHHP = 294.34, MEOHP = 292.33)
analysis$sum_DEHP <- (rowSums(analysis[, names(weights_2)] / weights_2, na.rm = FALSE))*390.56

analysis$log_sum_DEHP_imp_cratio <- log(analysis$sum_DEHP_imp_cratio)
analysis$alco_imp_45 <- ifelse(analysis$alco_imp>=45,1,0)
analysis$log_creatinine <- log(analysis$creatinine)
analysis$cancer_death <- ifelse(analysis$death ==1 & analysis$code_ICD == 'Malignant Neoplasms' ,1,0)
analysis$injury_death <- ifelse(analysis$death ==1 & analysis$code_ICD=="Accidents (Unintentional Injuries)", 1,0)
analysis$cvd_death <- ifelse(analysis$death == 1 & analysis$code_ICD %in% c('Cerebrovascular Diseases','Heart Diseases'),1,0)
analysis$cotinine_10 <- ifelse(analysis$cotinine >= 10, 1, 0)
analysis <- analysis %>% mutate(income_ratio_cat = case_when(income_ratio < 1 ~ 1,
                                                                             income_ratio >=1 & income_ratio < 1.5 ~ 2,
                                                                             income_ratio >=1.5 ~ 3))
analysis$weight_phth <- analysis$weight_phth/6 # adjust weight by number of cycles, using subsample specific weights
  
Xnm <- c('MBP_imp_cratio', 'MEP_imp_cratio', 'MBzP_imp_cratio',  'MIBP_imp_cratio', 'sum_DEHP_imp_cratio','MEHP_imp_cratio','MEHHP_imp_cratio','MECPP_imp_cratio','MEOHP_imp_cratio',
         'BPA_imp_cratio')
covariate <- c('age_imp','female_imp','educ_adult_imp','race2_imp','bmi_imp','cotinine_imp','income_ratio_imp','AHEI_ALL_imp','cycle_imp','mid_year_imp','alco_imp','MET_quar_imp','marital_imp','log_creatinine')
analysis_mixture <- analysis %>% drop_na(Xnm, covariate) # this is the dataset for main analyses

#### descriptive analyses ####
###### descriptives of covariates ######
sum(analysis_mixture$death)
sum(analysis_mixture$fu_mon)

library(tableone)
svy_design <- svydesign(data = analysis_mixture, id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~weight_phth, nest = TRUE)
covariate <- c('age', 'bmi','female', 'educ_adult', 'race2','marital',
                'income_ratio_cat', 'cotinine_10','alco_45','mid_year','AHEI_ALL', 'MET','fu_mon')
cat_covariate <- c('female','educ_adult', 'race2','marital','income_ratio_cat','cotinine_10','alco_45','mid_year')
table_svy <- svyCreateTableOne(vars = covariate, factorVars = cat_covariate,data = svy_design)
print(table_svy, showAllLevels = TRUE)
# report the nationally representative mean and percentages. for count, report the original data in the dataset (in below output)
table_count <- CreateTableOne(vars = covariate, factorVars = cat_covariate, data = analysis_mixture)
print(table_count, showAllLevels = TRUE)
quantile(analysis_mixture$MET,probs = seq(0,1,0.25),na.rm = TRUE)

# missingness in figure S1
covariate <- c('age', 'female', 'educ_adult', 'race2', 'marital', 'bmi','cotinine','income_ratio',
               'cycle','AHEI_ALL','creatinine','alco','mid_year','MET_quar')

for (var in covariate) {
  missing_n <- sum(is.na(analysis_mixture[[var]]))
  missing_prop <- sum(is.na(analysis_mixture[[var]]))/nrow(analysis_mixture) * 100
  print(c(var,missing_n,missing_prop))
} 

###### descriptives for chemical ######
analysis_mixture %>% summarize(MBP = quantile(MBP, probs = seq(0,1,0.25)),
                               MEP = quantile(MEP, probs = seq(0,1,0.25)),
                               MBzP = quantile(MBzP, probs = seq(0,1,0.25)),
                               MEHP = quantile(MEHP, probs = seq(0,1,0.25)),
                               MEHHP = quantile(MEHHP, probs = seq(0,1,0.25)),
                               MEOHP = quantile(MEOHP, probs = seq(0,1,0.25)),
                               MECPP = quantile(MECPP, probs = seq(0,1,0.25)),
                               sum_DEHP = quantile(sum_DEHP, probs = seq(0,1,0.25)),
                               MIBP = quantile(MIBP, probs = seq(0,1,0.25)),)

covariate <- c('MBP_LOD','MEP_LOD', 'MEHP_LOD','MBzP_LOD',
                   'MEHHP_LOD','MEOHP_LOD','MIBP_LOD','MECPP_LOD')
cat_covariate <- c('MBP_LOD','MEP_LOD', 'MEHP_LOD','MBzP_LOD',
                   'MEHHP_LOD','MEOHP_LOD','MIBP_LOD','MECPP_LOD')
table_phth <- CreateTableOne(vars = covariate, factorVars = cat_covariate,data = analysis_mixture)
print(table_phth, showAllLevels = TRUE)

analysis_mixture %>% summarize(BPA = quantile(BPA, probs = seq(0,1,0.25), na.rm=TRUE))

covariate <- c('BPA_LOD')
cat_covariate <- c('BPA_LOD')
table_phenol <- CreateTableOne(vars = covariate, factorVars = cat_covariate,data = analysis_mixture)
print(table_phenol, showAllLevels = TRUE)

###### correlation ######
library(corrplot)
Xnm_cratio <- c('MBP_imp_cratio', 'MEP_imp_cratio', 'MBzP_imp_cratio',  'MIBP_imp_cratio', 'MEHP_imp_cratio',
                'MECPP_imp_cratio','MEHHP_imp_cratio','MEOHP_imp_cratio',
                'BPA_imp_cratio')
Xnm_original <- c('MBP_imp', 'MEP_imp', 'MBzP_imp',  'MIBP_imp', 'MEHP_imp','MECPP_imp','MEHHP_imp','MEOHP_imp',
                  'BPA_imp')
correlation_matrix <- cor(analysis_mixture[,c(Xnm_cratio,Xnm_original)], method = 'spearman')
corrplot.mixed(correlation_matrix, lower = 'number',upper = 'circle',
               tl.pos = "lt",
               diag = "n",
               number.cex = 0.6,  
               tl.cex = 0.7) 

#### main analyses ####
#### penalized spline model to test non-linearity  ####
library(survival)
library(mgcv)
library(ggplot2)
library(ggplotify)
library(cowplot)

Xnm_all <- c('MBP_imp_cratio', 'MEP_imp_cratio', 'MBzP_imp_cratio',  'MIBP_imp_cratio', 
             'MEHP_imp_cratio','MEHHP_imp_cratio','MEOHP_imp_cratio','MECPP_imp_cratio',
             'sum_DEHP_imp_cratio',
             'BPA_imp_cratio')
log_Xnm_all <- paste0('log_', Xnm_all)

plot_list_all_cause <- list()
for (chemical in log_Xnm_all) {
  fit <- coxph(as.formula(paste0('Surv(fu_mon, death) ~ pspline(', chemical, ') + 
                 age_imp + female_imp + as.factor(educ_adult_imp) +
                 as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + income_ratio_imp + AHEI_ALL_imp +
                 as.factor(cycle_imp) + mid_year_imp + as.factor(alco_imp_45) + log_creatinine')), 
               data = analysis_mixture)
  pval <- summary(fit)$coef[2,'p']
  ggplot_obj <- as.ggplot(~termplot(fit, se = TRUE, rug = TRUE, terms = paste0('pspline(', chemical, ')'), ylab='',xlab='')) +
    xlab(gsub("log_|_imp_cratio|_update", "",chemical)) +
    annotate("text", x = 0.90, y = 0.75, label = paste0("p-value = ", format(pval, digits=3)),
             size = 4, hjust = 0.95, color = "black") +
    theme(
      axis.title.x = element_text(size = 10, margin = margin(t = 3)), 
      plot.margin = margin(5, 5, 5, 5) 
    )
  plot_list_all_cause[[chemical]] <- ggplot_obj
}

combined_plot_all_cause <- plot_grid(plotlist = plot_list_all_cause, ncol = 4, align = 'hv',rel_heights = c(1, 1))
combined_plot_all_cause_2 <- ggdraw() +
  draw_label("All-Cause Mortality", fontface = 'bold', size = 12, x = 0.5, hjust = 0.5, y = 0.95, vjust = 1) +
  draw_plot(combined_plot_all_cause, y = 0, height = 0.95)  
print(combined_plot_all_cause_2)

# removed cycle 15-16 for CVD analyses b/c NHANES categorized cerebrovascular disease and accident mortality into other causes of death due to the small number of cases in this cycle. 
plot_list_cvd <- list()
for (chemical in log_Xnm_all) {
  fit <- coxph(as.formula(paste0('Surv(fu_mon, cvd_death) ~ pspline(', chemical, ') + 
                 age_imp + female_imp + as.factor(educ_adult_imp) +
                 as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + income_ratio_imp + AHEI_ALL_imp +
                 as.factor(cycle_imp) + mid_year_imp + as.factor(alco_imp_45) + log_creatinine')), 
               data = analysis_mixture[analysis_mixture$cycle !='2015-2016',])
  pval <- summary(fit)$coef[2,'p']
  ggplot_obj <- as.ggplot(~termplot(fit, se = TRUE, rug = TRUE, terms = paste0('pspline(', chemical, ')'), ylab='',xlab='')) +
    xlab(gsub("log_|_imp_cratio|_update", "",chemical)) +
    annotate("text", x = 0.90, y = 0.75, label = paste0("p-value = ", format(pval, digits=3)),
             size = 4, hjust = 0.95, color = "black") +
    theme(
      axis.title.x = element_text(size = 10, margin = margin(t = 3)), 
      plot.margin = margin(5, 5, 5, 5) 
    )
  plot_list_cvd[[chemical]] <- ggplot_obj
}

combined_plot_cvd <- plot_grid(plotlist = plot_list_cvd, ncol = 4, align = 'hv',rel_heights = c(1, 1))
combined_plot_cvd_2 <- ggdraw() +
  draw_label("CVD Mortality", fontface = 'bold', size = 12, x = 0.5, hjust = 0.5, y = 0.95, vjust = 1) +
  draw_plot(combined_plot_cvd, y = 0, height = 0.95)  
print(combined_plot_cvd_2)


plot_list_cancer <- list()
for (chemical in log_Xnm_all) {
  fit <- coxph(as.formula(paste0('Surv(fu_mon, cancer_death) ~ pspline(', chemical, ') + 
                 age_imp + female_imp + as.factor(educ_adult_imp) +
                 as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + income_ratio_imp + AHEI_ALL_imp +
                 as.factor(cycle_imp) + mid_year_imp + as.factor(alco_imp_45) + log_creatinine')), 
               data = analysis_mixture)
  pval <- summary(fit)$coef[2,'p']
  ggplot_obj <- as.ggplot(~termplot(fit, se = TRUE, rug = TRUE, terms = paste0('pspline(', chemical, ')'), ylab='',xlab='')) +
    xlab(gsub("log_|_imp_cratio|_update", "",chemical)) +
    annotate("text", x = 0.90, y = 0.75, label = paste0("p-value = ", format(pval, digits=3)),
             size = 4, hjust = 0.95, color = "black") +
    theme(
      axis.title.x = element_text(size = 10, margin = margin(t = 3)),
      plot.margin = margin(5, 5, 5, 5) 
    )
  plot_list_cancer[[chemical]] <- ggplot_obj
}

combined_plot_cancer <- plot_grid(plotlist = plot_list_cancer, ncol = 4, align = 'hv',rel_heights = c(1, 1))
combined_plot_cancer_2 <- ggdraw() +
  draw_label("Cancer Mortality", fontface = 'bold', size = 12, x = 0.5, hjust = 0.5, y = 0.95, vjust = 1) +
  draw_plot(combined_plot_cancer, y = 0, height = 0.95)  
print(combined_plot_cancer_2)


#### single-chemical analysis  ####
library(survey)
Xnm <- c('MBP_imp_cratio', 'MEP_imp_cratio', 'MBzP_imp_cratio',  'MIBP_imp_cratio', 
             'MEHP_imp_cratio','MEHHP_imp_cratio','MEOHP_imp_cratio','MECPP_imp_cratio',
             'sum_DEHP_imp_cratio',
             'BPA_imp_cratio')
log_Xnm <- paste0('log_', Xnm)

output_all_cause <- matrix(NA, nrow=length(c(log_Xnm )), ncol = 5)
colnames(output_all_cause) <- c('point','lowCI','upperCI','chemical', 'n')
index <- 1
for (chemical in log_Xnm) {
  tryCatch({
    data <- analysis_mixture 
    design <- svydesign(data = data, id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~ weight_phth, nest = TRUE)
    # options(survey.lonely.psu = "adjust")
    
    fit <- svycoxph(as.formula(paste0("Surv(fu_mon, death) ~ ",chemical," + 
                      age_imp + female_imp + as.factor(educ_adult_imp)+
                      as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + income_ratio_imp + AHEI_ALL_imp +
                      as.factor(cycle_imp) + mid_year_imp + as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine")), 
                    design = design, data = data)
    
    point <- exp(coef(fit)[chemical])
    lowCI <- exp(coef(fit)[chemical] - 1.96 * sqrt(vcov(fit)[chemical, chemical]))
    upperCI <- exp(coef(fit)[chemical] + 1.96 * sqrt(vcov(fit)[chemical, chemical]))
    output_all_cause[index, ] <- c(point, lowCI, upperCI, chemical, fit$n)
    index <- index + 1
  }, error = function(e) {
    message("Error with chemical: ", chemical, " - ", e$message)
    output_all_cause[index, ] <- matrix(c(NA, NA, NA, chemical, NA), nrow = 4, ncol = 5, byrow = TRUE)
    index <- index + 1
  })
}


## cvd
# removed 2015-2016 cycle for CVD analyses
output_cvd_cause <- matrix(NA, nrow=length(c(log_Xnm )), ncol = 5)
colnames(output_cvd_cause) <- c('point','lowCI','upperCI','chemical', 'n')
index <- 1
for (chemical in log_Xnm) {
  tryCatch({
    data <- analysis_mixture[analysis_mixture$cycle !='2015-2016',]
    design <- svydesign(data = data, id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~ weight_phth, nest = TRUE)
    # options(survey.lonely.psu = "adjust")
    
    fit <- svycoxph(as.formula(paste0("Surv(fu_mon, cvd_death) ~ ",chemical," + 
                      age_imp + female_imp + as.factor(educ_adult_imp)+
                      as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + income_ratio_imp + AHEI_ALL_imp +
                      as.factor(cycle_imp) + mid_year_imp + as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine")), 
                    design = design, data = data)
    
    point <- exp(coef(fit)[chemical])
    lowCI <- exp(coef(fit)[chemical] - 1.96 * sqrt(vcov(fit)[chemical, chemical]))
    upperCI <- exp(coef(fit)[chemical] + 1.96 * sqrt(vcov(fit)[chemical, chemical]))
    output_cvd_cause[index, ] <- c(point, lowCI, upperCI, chemical, fit$n)
    index <- index + 1
  }, error = function(e) {
    message("Error with chemical: ", chemical, " - ", e$message)
    output_cvd_cause[index, ] <- matrix(c(NA, NA, NA, chemical, NA), nrow = 4, ncol = 5, byrow = TRUE)
    index <- index + 1
  })
}


## cancer
output_cancer_cause <- matrix(NA, nrow=length(c(log_Xnm )), ncol = 5)
colnames(output_cancer_cause) <- c('point','lowCI','upperCI','chemical', 'n')

index <- 1
for (chemical in log_Xnm) {
  tryCatch({
    data <- analysis_mixture 
    design <- svydesign(data = data, id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~ weight_phth, nest = TRUE)
    # options(survey.lonely.psu = "adjust")
    
    fit <- svycoxph(as.formula(paste0("Surv(fu_mon, cancer_death) ~ ",chemical," + 
                      age_imp + female_imp + as.factor(educ_adult_imp)+
                      as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + income_ratio_imp + AHEI_ALL_imp +
                      as.factor(cycle_imp) + mid_year_imp + as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine")), 
                    design = design, data = data)
    
    point <- exp(coef(fit)[chemical])
    lowCI <- exp(coef(fit)[chemical] - 1.96 * sqrt(vcov(fit)[chemical, chemical]))
    upperCI <- exp(coef(fit)[chemical] + 1.96 * sqrt(vcov(fit)[chemical, chemical]))
    output_cancer_cause[index, ] <- c(point, lowCI, upperCI, chemical, fit$n)
    index <- index + 1
  }, error = function(e) {
    message("Error with chemical: ", chemical, " - ", e$message)
    output_cancer_cause[index, ] <- matrix(c(NA, NA, NA, chemical, NA), nrow = 4, ncol = 5, byrow = TRUE)
    index <- index + 1
  })
}

output_all_cause <- as.data.frame(output_all_cause)
output_all_cause$point <- as.numeric(output_all_cause$point)
output_all_cause$lowCI <- as.numeric(output_all_cause$lowCI)
output_all_cause$upperCI <- as.numeric(output_all_cause$upperCI)
output_all_cause$chemical_name <- gsub("log_|_imp_cratio|_update", "",  output_all_cause$chemical)

# Convert chemical_name to factor with desired order
output_all_cause$chemical_name <- factor(output_all_cause$chemical_name, levels =  c('MBP', 'MEP', 'MBzP', 'MIBP', 
                                                                                                   'MEHP', 'MEHHP', 'MEOHP', 'MECPP', 'sum_DEHP',
                                                                                                     'BPA'))

all_cause <- ggplot(data=output_all_cause) + geom_point(aes(x=as.factor(chemical_name), y=point)) + 
  geom_errorbar(aes(x=as.factor(chemical_name),ymin=lowCI, ymax=upperCI)) +
  geom_hline(yintercept= 1, linetype='dashed',color='red') + 
  ggtitle("All-Cause Mortality") +  
  xlab("Biomarker") +  
  ylab("HR (95% CI)")  + 
  scale_y_continuous(limits = c(round(min(output_all_cause$lowCI), 1)-0.05, round(max(output_all_cause$upperCI), 1)+0.05),
                     breaks = seq(round(min(output_all_cause$lowCI), 1)-0.05, round(max(output_all_cause$upperCI), 1)+0.05, by=0.05))


output_cvd_cause <- as.data.frame(output_cvd_cause)
output_cvd_cause$point <- as.numeric(output_cvd_cause$point)
output_cvd_cause$lowCI <- as.numeric(output_cvd_cause$lowCI)
output_cvd_cause$upperCI <- as.numeric(output_cvd_cause$upperCI)
output_cvd_cause$chemical_name <- gsub("log_|_imp_cratio|_update", "",  output_cvd_cause$chemical)

# Convert chemical_name to factor with desired order
output_cvd_cause$chemical_name <- factor(output_cvd_cause$chemical_name, levels =  c('MBP', 'MEP', 'MBzP', 'MIBP', 
                                                                                     'MEHP','MEHHP', 'MEOHP', 'MECPP', 'sum_DEHP',
                                                                                     'BPA'))

cvd_cause <- ggplot(data=output_cvd_cause) + geom_point(aes(x=as.factor(chemical_name), y=point)) + 
  geom_errorbar(aes(x=as.factor(chemical_name),ymin=lowCI, ymax=upperCI)) +
  geom_hline(yintercept= 1, linetype='dashed',color='red') + 
  ggtitle("CVD Mortality") +  
  xlab("Biomarker") +  
  ylab("HR (95% CI)")    + 
  scale_y_continuous(limits = c(round(min(output_cvd_cause$lowCI), 1)-0.05, round(max(output_cvd_cause$upperCI), 1)+0.05),
                     breaks = seq(round(min(output_cvd_cause$lowCI), 1)-0.05, round(max(output_cvd_cause$upperCI), 1)+0.05, by=0.05))


output_cancer_cause <- as.data.frame(output_cancer_cause)
output_cancer_cause$point <- as.numeric(output_cancer_cause$point)
output_cancer_cause$lowCI <- as.numeric(output_cancer_cause$lowCI)
output_cancer_cause$upperCI <- as.numeric(output_cancer_cause$upperCI)
output_cancer_cause$chemical_name <- gsub("log_|_imp_cratio|_update", "",  output_cancer_cause$chemical)

# Convert chemical_name to factor with desired order
output_cancer_cause$chemical_name <- factor(output_cancer_cause$chemical_name, levels =  c('MBP', 'MEP', 'MBzP', 'MIBP', 
                                                                                     'MEHP','MEHHP', 'MEOHP', 'MECPP', 'sum_DEHP',
                                                                                     'BPA'))


cancer_cause <- ggplot(data=output_cancer_cause) + geom_point(aes(x=as.factor(chemical_name), y=point)) + 
  geom_errorbar(aes(x=as.factor(chemical_name),ymin=lowCI, ymax=upperCI)) +
  geom_hline(yintercept= 1, linetype='dashed',color='red') + 
  ggtitle("Cancer Mortality") +  
  xlab("Biomarker") +  
  ylab("HR (95% CI)")    + 
  scale_y_continuous(limits = c(round(min(output_cancer_cause$lowCI), 1)-0.05, round(max(output_cancer_cause$upperCI), 1)+0.05),
                     breaks = seq(round(min(output_cancer_cause$lowCI), 1)-0.05, round(max(output_cancer_cause$upperCI), 1)+0.05, by=0.05))

library(gridExtra)
grid.arrange(all_cause, cancer_cause, cvd_cause,ncol=1)

### QGC with weighting ####
library(qgcomp)
qgc_output_svy <- as.data.frame(matrix(NA, ncol=7, nrow=4))
colnames(qgc_output_svy) <- c('HR','lowCI','upperCI','cause','n_case','person_time','mixture')
qgc_output_svy$cause <- c('all_cause','heart','cancer','injury')
qgc_output_svy$mixture <- c(rep('mixture1',4))

Xnm <- c('MBP_imp_cratio', 'MEP_imp_cratio', 'MBzP_imp_cratio',  'MIBP_imp_cratio', 
         'MEHP_imp_cratio','MEHHP_imp_cratio','MEOHP_imp_cratio','MECPP_imp_cratio',
             'BPA_imp_cratio')
covariate <- c('age_imp','female_imp','educ_adult_imp','race2_imp','marital_imp','bmi_imp','cotinine_imp','income_ratio_imp','AHEI_ALL_imp','cycle_imp','mid_year_imp','alco_imp_45','MET_quar_imp','log_creatinine')

## all cause ##
qgc_output_svy[qgc_output_svy$cause=='all_cause' & qgc_output_svy$mixture == 'mixture1','n_case']<- sum(analysis_mixture$death)
qgc_output_svy[qgc_output_svy$cause=='all_cause' & qgc_output_svy$mixture == 'mixture1','person_time']<- sum(analysis_mixture$fu_mon)
qgc_fit <- qgcomp.cox.noboot(Surv(fu_mon, death) ~ .,expnms=Xnm, 
                             data=analysis_mixture[,c(Xnm, covariate, 'fu_mon', 'death')], q=3, weight = analysis_mixture$weight_phth)
# qgc_fit # point 0.289351; p value: 3.242e-05
qgc_output_svy[qgc_output_svy$cause=='all_cause' & qgc_output_svy$mixture == 'mixture1','HR'] <- exp(qgc_fit$psi)
qgc_output_svy[qgc_output_svy$cause=='all_cause' & qgc_output_svy$mixture == 'mixture1','lowCI']  <- exp(qgc_fit$ci)[1]
qgc_output_svy[qgc_output_svy$cause=='all_cause' & qgc_output_svy$mixture == 'mixture1','upperCI']  <- exp(qgc_fit$ci)[2]

# weights of individual metabolite in the mixture
qgc_fit$pos.psi
qgc_fit$neg.psi
qgc_fit$neg.weights
qgc_fit$pos.weights

## cause specific ##
qgc_output_svy[qgc_output_svy$cause=='cancer' & qgc_output_svy$mixture == 'mixture1','n_case']<- sum(analysis_mixture$cancer_death)
qgc_output_svy[qgc_output_svy$cause=='cancer' & qgc_output_svy$mixture == 'mixture1','person_time']<- sum(analysis_mixture$fu_mon)
qgc_fit <- qgcomp.cox.noboot(Surv(fu_mon, cancer_death) ~ .,expnms=Xnm,
                             data=analysis_mixture[,c(Xnm, covariate, 'fu_mon', 'cancer_death')], q=3, weight = analysis_mixture$weight_phth)
# qgc_fit # point 0.289351; p value: 3.242e-05
qgc_output_svy[qgc_output_svy$cause=='cancer' & qgc_output_svy$mixture == 'mixture1','HR'] <- exp(qgc_fit$psi)
qgc_output_svy[qgc_output_svy$cause=='cancer' & qgc_output_svy$mixture == 'mixture1','lowCI']  <- exp(qgc_fit$ci)[1]
qgc_output_svy[qgc_output_svy$cause=='cancer' & qgc_output_svy$mixture == 'mixture1','upperCI']  <- exp(qgc_fit$ci)[2]

qgc_fit$pos.psi
qgc_fit$neg.psi
qgc_fit$neg.weights
qgc_fit$pos.weights

# removed 2015-2016 cycles for CVD and accident analyses
analysis_mixture_cvd <- analysis_mixture[analysis_mixture$cycle !='2015-2016',]
qgc_output_svy[qgc_output_svy$cause=='heart' & qgc_output_svy$mixture == 'mixture1','n_case']<- sum(analysis_mixture_cvd$cvd_death)
qgc_output_svy[qgc_output_svy$cause=='heart' & qgc_output_svy$mixture == 'mixture1','person_time']<- sum(analysis_mixture_cvd$fu_mon)
qgc_fit <- qgcomp.cox.noboot(Surv(fu_mon, cvd_death) ~ .,expnms=Xnm,
                             data=analysis_mixture_cvd[,c(Xnm, covariate, 'fu_mon', 'cvd_death')], q=3, weight = analysis_mixture_cvd$weight_phth)
# qgc_fit # point 0.289351; p value: 3.242e-05
qgc_output_svy[qgc_output_svy$cause=='heart' & qgc_output_svy$mixture == 'mixture1','HR'] <- exp(qgc_fit$psi)
qgc_output_svy[qgc_output_svy$cause=='heart' & qgc_output_svy$mixture == 'mixture1','lowCI']  <- exp(qgc_fit$ci)[1]
qgc_output_svy[qgc_output_svy$cause=='heart' & qgc_output_svy$mixture == 'mixture1','upperCI']  <- exp(qgc_fit$ci)[2]

qgc_fit$pos.psi
qgc_fit$neg.psi
qgc_fit$neg.weights
qgc_fit$pos.weights


qgc_output_svy[qgc_output_svy$cause=='injury' & qgc_output_svy$mixture == 'mixture1','n_case']<- sum(analysis_mixture_cvd$injury_death)
qgc_output_svy[qgc_output_svy$cause=='injury' & qgc_output_svy$mixture == 'mixture1','person_time']<- sum(analysis_mixture_cvd$fu_mon)
qgc_fit <- qgcomp.cox.noboot(Surv(fu_mon, injury_death) ~ .,expnms=Xnm,
                             data=analysis_mixture_cvd[,c(Xnm, covariate, 'fu_mon', 'injury_death')], q=3, weight = analysis_mixture_cvd$weight_phth)

qgc_output_svy[qgc_output_svy$cause=='injury' & qgc_output_svy$mixture == 'mixture1','HR'] <- exp(qgc_fit$psi)
qgc_output_svy[qgc_output_svy$cause=='injury' & qgc_output_svy$mixture == 'mixture1','lowCI']  <- exp(qgc_fit$ci)[1]
qgc_output_svy[qgc_output_svy$cause=='injury' & qgc_output_svy$mixture == 'mixture1','upperCI']  <- exp(qgc_fit$ci)[2]


PAF_N <- function(point, lowCI, upperCI, prevalence, n_death){
  PAF_point = (prevalence *(point-1)/(prevalence*(point-1)+1))*100
  PAF_lowCI = (prevalence *(lowCI-1)/(prevalence*(lowCI-1)+1))*100
  PAF_upperCI = (prevalence *(upperCI-1)/(prevalence*(upperCI-1)+1))*100
  Attri_N_point = PAF_point*n_death*0.01
  Attri_N_lowCI = PAF_lowCI*n_death*0.01
  Attri_N_upperCI = PAF_upperCI*n_death*0.01
  print(c(PAF_point,PAF_lowCI,PAF_upperCI,
          Attri_N_point,
          Attri_N_lowCI,
          Attri_N_upperCI))
}
# all cause
PAF_N(qgc_output_svy[qgc_output_svy$cause=='all_cause','HR'], qgc_output_svy[qgc_output_svy$cause=='all_cause','lowCI'], qgc_output_svy[qgc_output_svy$cause=='all_cause','upperCI'], 0.33,2487556) # obtained average all-cause death from surveillance: : 2487556
# cancer
PAF_N(qgc_output_svy[qgc_output_svy$cause=='cancer','HR'], qgc_output_svy[qgc_output_svy$cause=='cancer','lowCI'], qgc_output_svy[qgc_output_svy$cause=='cancer','upperCI'], 0.33,589396)# average cancer death: 589396
# CVD
PAF_N(qgc_output_svy[qgc_output_svy$cause=='heart','HR'], qgc_output_svy[qgc_output_svy$cause=='heart','lowCI'], qgc_output_svy[qgc_output_svy$cause=='heart','upperCI'], 0.33,615978) # average CVD death: 615978

#### EMM analyses by vitamins accounting for weight  ####
library(qgcompint)
library(car)
# supplements use available after 2007 cycle
supp <- read.csv('supplement.csv')
analysis_supp <- merge(analysis_mixture, supp, by='SEQN') 
###### folate ######
analysis_mixture_fol <- analysis_supp %>% drop_na(fol_rbc,FA_supp)
analysis_mixture_fol <- analysis_mixture_fol %>% mutate(FA_supp_yes = case_when(FA_supp == 0 ~ 0,
                                                                                FA_supp > 0 ~ 1,
                                                                                TRUE ~ NA_integer_))
## restricting to non FA user ##
analysis_mixture_fol_nosupp <- analysis_mixture_fol %>% filter(FA_supp_yes == 0) 
# create tertile folate concentration variable by cycle-specific concentrations b/c measurement tool differed across years
analysis_mixture_fol_nosupp <- analysis_mixture_fol_nosupp %>% group_by(cycle) %>%
  mutate(tertile_fol_rbc = cut(fol_rbc, breaks = quantile(fol_rbc, probs = seq(0, 1, 1/3), na.rm = TRUE),
                               include.lowest = TRUE,
                               labels = c(1, 2, 3)))
analysis_mixture_fol_nosupp <- as.data.frame(analysis_mixture_fol_nosupp)

table_fol <- as.data.frame(matrix(NA, ncol=8, nrow=9))
colnames(table_fol) <- c('HR','lowCI','upperCI','tertile','pval','cause','n_case','person_time')
table_fol$tertile <- rep(c(1,2,3),3)
table_fol$cause <- c(rep('all_cause',3), rep('heart',3), rep('cancer',3))

fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                               age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'tertile_fol_rbc',
                             data=analysis_mixture_fol_nosupp, q=3,
                             weight = analysis_mixture_fol_nosupp$weight_phth)
table_fol[table_fol$tertile == 1 & table_fol$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_fol[table_fol$tertile == 2 & table_fol$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_fol[table_fol$tertile == 3 & table_fol$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=3)[2,c('hr','ll.hr','ul.hr')]

# joint Wald test of both interaction terms = 0
table_fol[table_fol$cause == 'all_cause', 'pval'] <- linearHypothesis(
  fit,
  c("tertile_fol_rbc2:mixture = 0", 
    "tertile_fol_rbc3:mixture = 0"),
  vcov. = vcov(fit)
)$`Pr(>Chisq)`[2]

fit_cancer <- qgcomp.emm.cox.noboot(Surv(fu_mon, cancer_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                                      age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                                      income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                                      as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                                    expnms=Xnm,
                                    emmvar = 'tertile_fol_rbc',
                                    data=analysis_mixture_fol_nosupp, q=3,
                                    weight = analysis_mixture_fol_nosupp$weight_phth)
table_fol[table_fol$tertile == 1 & table_fol$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cancer, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_fol[table_fol$tertile == 2 & table_fol$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cancer, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_fol[table_fol$tertile == 3 & table_fol$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cancer, emmval=3)[2,c('hr','ll.hr','ul.hr')]

table_fol[table_fol$cause == 'cancer', 'pval'] <- linearHypothesis(
  fit_cancer,
  c("tertile_fol_rbc2:mixture = 0", 
    "tertile_fol_rbc3:mixture = 0"),
  vcov. = vcov(fit_cancer)
)$`Pr(>Chisq)`[2]


table_fol[table_fol$tertile == 1,]$person_time <- sum(analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$tertile_fol_rbc ==1, ]$fu_mon)
table_fol[table_fol$tertile == 2,]$person_time <- sum(analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$tertile_fol_rbc ==2, ]$fu_mon)
table_fol[table_fol$tertile == 3,]$person_time <- sum(analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$tertile_fol_rbc ==3, ]$fu_mon)

table_fol[table_fol$cause=='all_cause' & table_fol$tertile == 1,]$n_case <- sum(analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$tertile_fol_rbc ==1, ]$death)
table_fol[table_fol$cause=='cancer' & table_fol$tertile == 1,]$n_case <- sum(analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$tertile_fol_rbc ==1, ]$cancer_death)

table_fol[table_fol$cause=='all_cause' & table_fol$tertile == 2,]$n_case <- sum(analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$tertile_fol_rbc ==2, ]$death)
table_fol[table_fol$cause=='cancer' & table_fol$tertile == 2,]$n_case <- sum(analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$tertile_fol_rbc ==2, ]$cancer_death)

table_fol[table_fol$cause=='all_cause' & table_fol$tertile == 3,]$n_case <- sum(analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$tertile_fol_rbc ==3, ]$death)
table_fol[table_fol$cause=='cancer' & table_fol$tertile == 3,]$n_case <- sum(analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$tertile_fol_rbc ==3, ]$cancer_death)


# removed 2015-2016 cycle for CVD analyses
fit_cvd <- qgcomp.emm.cox.noboot(Surv(fu_mon, cvd_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                                   age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                                   income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                                   as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                                 expnms=Xnm,
                                 emmvar = 'tertile_fol_rbc',
                                 data=analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$cycle !='2015-2016',], q=3,
                                 weight = analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$cycle !='2015-2016',]$weight_phth)
table_fol[table_fol$tertile == 1 & table_fol$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cvd, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_fol[table_fol$tertile == 2 & table_fol$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cvd, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_fol[table_fol$tertile == 3 & table_fol$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cvd, emmval=3)[2,c('hr','ll.hr','ul.hr')]


table_fol[table_fol$cause == 'heart', 'pval'] <- linearHypothesis(
  fit_cvd,
  c("tertile_fol_rbc2:mixture = 0", 
    "tertile_fol_rbc3:mixture = 0"),
  vcov. = vcov(fit_cvd)
)$`Pr(>Chisq)`[2]


table_fol[table_fol$tertile == 1 & table_fol$cause == 'heart',]$person_time <- sum(analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$cycle !='2015-2016' & analysis_mixture_fol_nosupp$tertile_fol_rbc ==1, ]$fu_mon)
table_fol[table_fol$tertile == 2 & table_fol$cause == 'heart',]$person_time <- sum(analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$cycle !='2015-2016' & analysis_mixture_fol_nosupp$tertile_fol_rbc ==2, ]$fu_mon)
table_fol[table_fol$tertile == 3 & table_fol$cause == 'heart',]$person_time <- sum(analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$cycle !='2015-2016' & analysis_mixture_fol_nosupp$tertile_fol_rbc ==3, ]$fu_mon)

table_fol[table_fol$tertile == 1 & table_fol$cause == 'heart',]$n_case <- sum(analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$cycle !='2015-2016' & analysis_mixture_fol_nosupp$tertile_fol_rbc ==1, ]$cvd_death)
table_fol[table_fol$tertile == 2 & table_fol$cause == 'heart',]$n_case <- sum(analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$cycle !='2015-2016' & analysis_mixture_fol_nosupp$tertile_fol_rbc ==2, ]$cvd_death)
table_fol[table_fol$tertile == 3 & table_fol$cause == 'heart',]$n_case <- sum(analysis_mixture_fol_nosupp[analysis_mixture_fol_nosupp$cycle !='2015-2016' & analysis_mixture_fol_nosupp$tertile_fol_rbc ==3, ]$cvd_death)

###### vitamin D ######
analysis_mixture_vitD <- analysis_supp %>% drop_na(vitD,vitD_supp)
analysis_mixture_vitD <- analysis_mixture_vitD %>% group_by(cycle) %>% mutate(vitD_supp_yes = case_when(vitD_supp == 0 ~ 0,
                                                                                                        vitD_supp > 0 ~ 1,
                                                                                                        TRUE ~ NA_integer_))
## restricting to non vitD supplement user ##
analysis_mixture_vitD_nosupp <- analysis_mixture_vitD %>% filter(vitD_supp_yes == 0) 

analysis_mixture_vitD_nosupp <- analysis_mixture_vitD_nosupp %>% 
  mutate(tertile_vitD = cut(vitD, breaks = quantile(vitD, probs = seq(0, 1, 1/3), na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = c(1, 2, 3)))

analysis_mixture_vitD_nosupp <- as.data.frame(analysis_mixture_vitD_nosupp)

table_vitD <- as.data.frame(matrix(NA, ncol=8, nrow=9))
colnames(table_vitD) <- c('HR','lowCI','upperCI','tertile','pval','cause','n_case','person_time')
table_vitD$tertile <- rep(c(1,2,3),3)
table_vitD$cause <- c(rep('all_cause',3), rep('heart',3), rep('cancer',3))


fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio +  BPA_imp_cratio +
                               age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'tertile_vitD',
                             data=analysis_mixture_vitD_nosupp, q=3,
                             weight = analysis_mixture_vitD_nosupp$weight_phth)
table_vitD[table_vitD$tertile == 1 & table_vitD$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_vitD[table_vitD$tertile == 2 & table_vitD$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_vitD[table_vitD$tertile == 3 & table_vitD$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=3)[2,c('hr','ll.hr','ul.hr')]

table_vitD[table_vitD$cause == 'all_cause', 'pval'] <- linearHypothesis(
  fit,
  c("tertile_vitD2:mixture = 0", 
    "tertile_vitD3:mixture = 0"),
  vcov. = vcov(fit)
)$`Pr(>Chisq)`[2]


fit_cancer <- qgcomp.emm.cox.noboot(Surv(fu_mon, cancer_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                                      age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                                      income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                                      as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                                    expnms=Xnm,
                                    emmvar = 'tertile_vitD',
                                    data=analysis_mixture_vitD_nosupp, q=3,
                                    weight = analysis_mixture_vitD_nosupp$weight_phth)
table_vitD[table_vitD$tertile == 1 & table_vitD$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cancer, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_vitD[table_vitD$tertile == 2 & table_vitD$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cancer, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_vitD[table_vitD$tertile == 3 & table_vitD$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cancer, emmval=3)[2,c('hr','ll.hr','ul.hr')]

table_vitD[table_vitD$cause == 'cancer', 'pval'] <- linearHypothesis(
  fit_cancer,
  c("tertile_vitD2:mixture = 0", 
    "tertile_vitD3:mixture = 0"),
  vcov. = vcov(fit_cancer)
)$`Pr(>Chisq)`[2]


table_vitD[table_vitD$tertile == 1,]$person_time <- sum(analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$tertile_vitD ==1, ]$fu_mon)
table_vitD[table_vitD$tertile == 2,]$person_time <- sum(analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$tertile_vitD ==2, ]$fu_mon)
table_vitD[table_vitD$tertile == 3,]$person_time <- sum(analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$tertile_vitD ==3, ]$fu_mon)

table_vitD[table_vitD$cause=='all_cause' & table_vitD$tertile == 1,]$n_case <- sum(analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$tertile_vitD ==1, ]$death)
table_vitD[table_vitD$cause=='cancer' & table_vitD$tertile == 1,]$n_case <- sum(analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$tertile_vitD ==1, ]$cancer_death)

table_vitD[table_vitD$cause=='all_cause' & table_vitD$tertile == 2,]$n_case <- sum(analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$tertile_vitD ==2, ]$death)
table_vitD[table_vitD$cause=='cancer' & table_vitD$tertile == 2,]$n_case <- sum(analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$tertile_vitD ==2, ]$cancer_death)

table_vitD[table_vitD$cause=='all_cause' & table_vitD$tertile == 3,]$n_case <- sum(analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$tertile_vitD ==3, ]$death)
table_vitD[table_vitD$cause=='cancer' & table_vitD$tertile == 3,]$n_case <- sum(analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$tertile_vitD ==3, ]$cancer_death)


# removed 2015-2016 cycle for cvd analyses
fit_cvd <- qgcomp.emm.cox.noboot(Surv(fu_mon, cvd_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                                   age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                                   income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                                   as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                                 expnms=Xnm,
                                 emmvar = 'tertile_vitD',
                                 data=analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$cycle !='2015-2016',], q=3,
                                 weight = analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$cycle !='2015-2016',]$weight_phth)
table_vitD[table_vitD$tertile == 1 & table_vitD$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cvd, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_vitD[table_vitD$tertile == 2 & table_vitD$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cvd, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_vitD[table_vitD$tertile == 3 & table_vitD$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cvd, emmval=3)[2,c('hr','ll.hr','ul.hr')]


table_vitD[table_vitD$cause == 'heart', 'pval'] <- linearHypothesis(
  fit_cvd,
  c("tertile_vitD2:mixture = 0", 
    "tertile_vitD3:mixture = 0"),
  vcov. = vcov(fit_cvd)
)$`Pr(>Chisq)`[2]

table_vitD[table_vitD$tertile == 1 & table_vitD$cause == 'heart',]$person_time <- sum(analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$cycle !='2015-2016' & analysis_mixture_vitD_nosupp$tertile_vitD ==1, ]$fu_mon)
table_vitD[table_vitD$tertile == 2 & table_vitD$cause == 'heart',]$person_time <- sum(analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$cycle !='2015-2016' & analysis_mixture_vitD_nosupp$tertile_vitD ==2, ]$fu_mon)
table_vitD[table_vitD$tertile == 3 & table_vitD$cause == 'heart',]$person_time <- sum(analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$cycle !='2015-2016' & analysis_mixture_vitD_nosupp$tertile_vitD ==3, ]$fu_mon)

table_vitD[table_vitD$tertile == 1 & table_vitD$cause == 'heart',]$n_case <- sum(analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$cycle !='2015-2016' & analysis_mixture_vitD_nosupp$tertile_vitD ==1, ]$cvd_death)
table_vitD[table_vitD$tertile == 2 & table_vitD$cause == 'heart',]$n_case <- sum(analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$cycle !='2015-2016' & analysis_mixture_vitD_nosupp$tertile_vitD ==2, ]$cvd_death)
table_vitD[table_vitD$tertile == 3 & table_vitD$cause == 'heart',]$n_case <- sum(analysis_mixture_vitD_nosupp[analysis_mixture_vitD_nosupp$cycle !='2015-2016' & analysis_mixture_vitD_nosupp$tertile_vitD ==3, ]$cvd_death)

###### vitamin B6 ######
analysis_mixture_vitB6 <- analysis_supp %>% drop_na(vitB6,vitB6_supp)
analysis_mixture_vitB6 <- analysis_mixture_vitB6 %>% mutate(vitB6_supp_yes = case_when(vitB6_supp == 0 ~ 0,
                                                                                       vitB6_supp > 0 ~ 1,
                                                                                       TRUE ~ NA_integer_))

## restricting to non vitB6 supplement user ##
analysis_mixture_vitB6_nosupp <- analysis_mixture_vitB6 %>% filter(vitB6_supp_yes == 0) 

analysis_mixture_vitB6_nosupp <- analysis_mixture_vitB6_nosupp %>% 
  mutate(tertile_vitB6 = cut(vitB6, breaks = quantile(vitB6, probs = seq(0, 1, 1/3), na.rm = TRUE),
                             include.lowest = TRUE,
                             labels = c(1, 2, 3)))

analysis_mixture_vitB6_nosupp <- as.data.frame(analysis_mixture_vitB6_nosupp)

table_vitB6 <- as.data.frame(matrix(NA, ncol=8, nrow=9))
colnames(table_vitB6) <- c('HR','lowCI','upperCI','tertile','pval','cause','n_case','person_time')
table_vitB6$tertile <- c(1,2,3)
table_vitB6$cause <- c(rep('all_cause',3), rep('heart',3), rep('cancer',3))

fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                               age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'tertile_vitB6',
                             data=analysis_mixture_vitB6_nosupp, q=3,
                             weight = analysis_mixture_vitB6_nosupp$weight_phth)
table_vitB6[table_vitB6$tertile == 1 & table_vitB6$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_vitB6[table_vitB6$tertile == 2 & table_vitB6$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_vitB6[table_vitB6$tertile == 3 & table_vitB6$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=3)[2,c('hr','ll.hr','ul.hr')]

table_vitB6[table_vitB6$cause == 'all_cause', 'pval'] <- linearHypothesis(
  fit,
  c("tertile_vitB62:mixture = 0", 
    "tertile_vitB63:mixture = 0"),
  vcov. = vcov(fit)
)$`Pr(>Chisq)`[2]


table_vitB6[table_vitB6$tertile == 1,]$person_time <- sum(analysis_mixture_vitB6_nosupp[analysis_mixture_vitB6_nosupp$tertile_vitB6 ==1, ]$fu_mon)
table_vitB6[table_vitB6$tertile == 2,]$person_time <- sum(analysis_mixture_vitB6_nosupp[analysis_mixture_vitB6_nosupp$tertile_vitB6 ==2, ]$fu_mon)
table_vitB6[table_vitB6$tertile == 3,]$person_time <- sum(analysis_mixture_vitB6_nosupp[analysis_mixture_vitB6_nosupp$tertile_vitB6 ==3, ]$fu_mon)

table_vitB6[table_vitB6$cause=='all_cause' & table_vitB6$tertile == 1,]$n_case <- sum(analysis_mixture_vitB6_nosupp[analysis_mixture_vitB6_nosupp$tertile_vitB6 ==1, ]$death)
table_vitB6[table_vitB6$cause=='all_cause' & table_vitB6$tertile == 2,]$n_case <- sum(analysis_mixture_vitB6_nosupp[analysis_mixture_vitB6_nosupp$tertile_vitB6 ==2, ]$death)
table_vitB6[table_vitB6$cause=='all_cause' & table_vitB6$tertile == 3,]$n_case <- sum(analysis_mixture_vitB6_nosupp[analysis_mixture_vitB6_nosupp$tertile_vitB6 ==3, ]$death)


table_vitB6[table_vitB6$cause=='cancer' & table_vitB6$tertile == 1,]$n_case <- sum(analysis_mixture_vitB6_nosupp[analysis_mixture_vitB6_nosupp$tertile_vitB6 ==1, ]$cancer_death)
table_vitB6[table_vitB6$cause=='cancer' & table_vitB6$tertile == 2,]$n_case <- sum(analysis_mixture_vitB6_nosupp[analysis_mixture_vitB6_nosupp$tertile_vitB6 ==2, ]$cancer_death)
table_vitB6[table_vitB6$cause=='cancer' & table_vitB6$tertile == 3,]$n_case <- sum(analysis_mixture_vitB6_nosupp[analysis_mixture_vitB6_nosupp$tertile_vitB6 ==3, ]$cancer_death)


table_vitB6[table_vitB6$tertile == 1 & table_vitB6$cause == 'heart',]$person_time <- sum(analysis_mixture_vitB6_nosupp[analysis_mixture_vitB6_nosupp$cycle !='2015-2016' & analysis_mixture_vitB6_nosupp$tertile_vitB6 ==1, ]$fu_mon)
table_vitB6[table_vitB6$tertile == 2 & table_vitB6$cause == 'heart',]$person_time <- sum(analysis_mixture_vitB6_nosupp[analysis_mixture_vitB6_nosupp$cycle !='2015-2016' & analysis_mixture_vitB6_nosupp$tertile_vitB6 ==2, ]$fu_mon)
table_vitB6[table_vitB6$tertile == 3 & table_vitB6$cause == 'heart',]$person_time <- sum(analysis_mixture_vitB6_nosupp[analysis_mixture_vitB6_nosupp$cycle !='2015-2016' & analysis_mixture_vitB6_nosupp$tertile_vitB6 ==3, ]$fu_mon)

table_vitB6[table_vitB6$tertile == 1 & table_vitB6$cause == 'heart',]$n_case <- sum(analysis_mixture_vitB6_nosupp[analysis_mixture_vitB6_nosupp$cycle !='2015-2016' & analysis_mixture_vitB6_nosupp$tertile_vitB6 ==1, ]$cvd_death)
table_vitB6[table_vitB6$tertile == 2 & table_vitB6$cause == 'heart',]$n_case <- sum(analysis_mixture_vitB6_nosupp[analysis_mixture_vitB6_nosupp$cycle !='2015-2016' & analysis_mixture_vitB6_nosupp$tertile_vitB6 ==2, ]$cvd_death)
table_vitB6[table_vitB6$tertile == 3 & table_vitB6$cause == 'heart',]$n_case <- sum(analysis_mixture_vitB6_nosupp[analysis_mixture_vitB6_nosupp$cycle !='2015-2016' & analysis_mixture_vitB6_nosupp$tertile_vitB6 ==3, ]$cvd_death)


###### vitamin B12 ######
analysis_mixture_vitB12 <- analysis_supp %>% drop_na(vitB12,vitB12_supp)
analysis_mixture_vitB12 <- analysis_mixture_vitB12 %>% mutate(vitB12_supp_yes = case_when(vitB12_supp == 0 ~ 0,
                                                                                          vitB12_supp > 0 ~ 1,
                                                                                          TRUE ~ NA_integer_))
## restricting to non vitB12 supplement user ##
analysis_mixture_vitB12_nosupp <- analysis_mixture_vitB12 %>% filter(vitB12_supp_yes == 0) 

analysis_mixture_vitB12_nosupp <- analysis_mixture_vitB12_nosupp %>% 
  mutate(tertile_vitB12 = cut(vitB12, breaks = quantile(vitB12, probs = seq(0, 1, 1/3), na.rm = TRUE),
                              include.lowest = TRUE,
                              labels = c(1, 2, 3)))
analysis_mixture_vitB12_nosupp <- as.data.frame(analysis_mixture_vitB12_nosupp)

table_vitB12 <- as.data.frame(matrix(NA, ncol=8, nrow=9))
colnames(table_vitB12) <- c('HR','lowCI','upperCI','tertile','pval','cause','n_case','person_time')
table_vitB12$tertile <- c(1,2,3)
table_vitB12$cause <- c(rep('all_cause',3), rep('heart',3), rep('cancer',3))

fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                               age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'tertile_vitB12',
                             data=analysis_mixture_vitB12_nosupp, q=3,
                             weight = analysis_mixture_vitB12_nosupp$weight_phth)
table_vitB12[table_vitB12$tertile == 1 & table_vitB12$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_vitB12[table_vitB12$tertile == 2 & table_vitB12$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_vitB12[table_vitB12$tertile == 3 & table_vitB12$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=3)[2,c('hr','ll.hr','ul.hr')]


table_vitB12[table_vitB12$cause == 'all_cause', 'pval'] <- linearHypothesis(
  fit,
  c("tertile_vitB122:mixture = 0", 
    "tertile_vitB123:mixture = 0"),
  vcov. = vcov(fit)
)$`Pr(>Chisq)`[2]


table_vitB12[table_vitB12$tertile == 1,]$person_time <- sum(analysis_mixture_vitB12_nosupp[analysis_mixture_vitB12_nosupp$tertile_vitB12 ==1, ]$fu_mon)
table_vitB12[table_vitB12$tertile == 2,]$person_time <- sum(analysis_mixture_vitB12_nosupp[analysis_mixture_vitB12_nosupp$tertile_vitB12 ==2, ]$fu_mon)
table_vitB12[table_vitB12$tertile == 3,]$person_time <- sum(analysis_mixture_vitB12_nosupp[analysis_mixture_vitB12_nosupp$tertile_vitB12 ==3, ]$fu_mon)

table_vitB12[table_vitB12$cause=='all_cause' & table_vitB12$tertile == 1,]$n_case <- sum(analysis_mixture_vitB12_nosupp[analysis_mixture_vitB12_nosupp$tertile_vitB12 ==1, ]$death)
table_vitB12[table_vitB12$cause=='all_cause' & table_vitB12$tertile == 2,]$n_case <- sum(analysis_mixture_vitB12_nosupp[analysis_mixture_vitB12_nosupp$tertile_vitB12 ==2, ]$death)
table_vitB12[table_vitB12$cause=='all_cause' & table_vitB12$tertile == 3,]$n_case <- sum(analysis_mixture_vitB12_nosupp[analysis_mixture_vitB12_nosupp$tertile_vitB12 ==3, ]$death)

table_vitB12[table_vitB12$cause=='cancer' & table_vitB12$tertile == 1,]$n_case <- sum(analysis_mixture_vitB12_nosupp[analysis_mixture_vitB12_nosupp$tertile_vitB12 ==1, ]$cancer_death)
table_vitB12[table_vitB12$cause=='cancer' & table_vitB12$tertile == 2,]$n_case <- sum(analysis_mixture_vitB12_nosupp[analysis_mixture_vitB12_nosupp$tertile_vitB12 ==2, ]$cancer_death)
table_vitB12[table_vitB12$cause=='cancer' & table_vitB12$tertile == 3,]$n_case <- sum(analysis_mixture_vitB12_nosupp[analysis_mixture_vitB12_nosupp$tertile_vitB12 ==3, ]$cancer_death)

table_vitB12[table_vitB12$tertile == 1 & table_vitB12$cause == 'heart',]$person_time <- sum(analysis_mixture_vitB12_nosupp[analysis_mixture_vitB12_nosupp$cycle !='2015-2016' & analysis_mixture_vitB12_nosupp$tertile_vitB12 ==1, ]$fu_mon)
table_vitB12[table_vitB12$tertile == 2 & table_vitB12$cause == 'heart',]$person_time <- sum(analysis_mixture_vitB12_nosupp[analysis_mixture_vitB12_nosupp$cycle !='2015-2016' & analysis_mixture_vitB12_nosupp$tertile_vitB12 ==2, ]$fu_mon)
table_vitB12[table_vitB12$tertile == 3 & table_vitB12$cause == 'heart',]$person_time <- sum(analysis_mixture_vitB12_nosupp[analysis_mixture_vitB12_nosupp$cycle !='2015-2016' & analysis_mixture_vitB12_nosupp$tertile_vitB12 ==3, ]$fu_mon)

table_vitB12[table_vitB12$tertile == 1 & table_vitB12$cause == 'heart',]$n_case <- sum(analysis_mixture_vitB12_nosupp[analysis_mixture_vitB12_nosupp$cycle !='2015-2016' & analysis_mixture_vitB12_nosupp$tertile_vitB12 ==1, ]$cvd_death)
table_vitB12[table_vitB12$tertile == 2 & table_vitB12$cause == 'heart',]$n_case <- sum(analysis_mixture_vitB12_nosupp[analysis_mixture_vitB12_nosupp$cycle !='2015-2016' & analysis_mixture_vitB12_nosupp$tertile_vitB12 ==2, ]$cvd_death)
table_vitB12[table_vitB12$tertile == 3 & table_vitB12$cause == 'heart',]$n_case <- sum(analysis_mixture_vitB12_nosupp[analysis_mixture_vitB12_nosupp$cycle !='2015-2016' & analysis_mixture_vitB12_nosupp$tertile_vitB12 ==3, ]$cvd_death)


###### AHEI ######
analysis_mixture_AHEI <- analysis_supp %>% drop_na(AHEI_ALL)
analysis_mixture_AHEI <- analysis_mixture_AHEI %>% 
  mutate(tertile_AHEI = cut(AHEI_ALL, breaks = quantile(AHEI_ALL, probs = seq(0, 1, 1/3), na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = c(1, 2, 3)))
analysis_mixture_AHEI <- as.data.frame(analysis_mixture_AHEI)

table_AHEI <- as.data.frame(matrix(NA, ncol=8, nrow=9))
colnames(table_AHEI) <- c('HR','lowCI','upperCI','tertile','pval','cause','n_case','person_time')
table_AHEI$tertile <- rep(c(1,2,3),3)
table_AHEI$cause <- c(rep('all_cause',3), rep('heart',3), rep('cancer',3))

fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                               age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'tertile_AHEI',
                             data=analysis_mixture_AHEI, q=3,
                             weight = analysis_mixture_AHEI$weight_phth)
table_AHEI[table_AHEI$tertile == 1 & table_AHEI$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_AHEI[table_AHEI$tertile == 2 & table_AHEI$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_AHEI[table_AHEI$tertile == 3 & table_AHEI$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=3)[2,c('hr','ll.hr','ul.hr')]


table_AHEI[table_AHEI$cause == 'all_cause', 'pval'] <- linearHypothesis(
  fit,
  c("tertile_AHEI2:mixture = 0", 
    "tertile_AHEI3:mixture = 0"),
  vcov. = vcov(fit)
)$`Pr(>Chisq)`[2]


fit_cancer <- qgcomp.emm.cox.noboot(Surv(fu_mon, cancer_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                                      age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                                      income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                                      as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                                    expnms=Xnm,
                                    emmvar = 'tertile_AHEI',
                                    data=analysis_mixture_AHEI, q=3,
                                    weight = analysis_mixture_AHEI$weight_phth)
table_AHEI[table_AHEI$tertile == 1 & table_AHEI$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cancer, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_AHEI[table_AHEI$tertile == 2 & table_AHEI$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cancer, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_AHEI[table_AHEI$tertile == 3 & table_AHEI$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cancer, emmval=3)[2,c('hr','ll.hr','ul.hr')]

table_AHEI[table_AHEI$cause == 'cancer', 'pval'] <- linearHypothesis(
  fit_cancer,
  c("tertile_AHEI2:mixture = 0", 
    "tertile_AHEI3:mixture = 0"),
  vcov. = vcov(fit_cancer)
)$`Pr(>Chisq)`[2]


table_AHEI[table_AHEI$tertile == 1,]$person_time <- sum(analysis_mixture_AHEI[analysis_mixture_AHEI$tertile_AHEI ==1, ]$fu_mon)
table_AHEI[table_AHEI$tertile == 2,]$person_time <- sum(analysis_mixture_AHEI[analysis_mixture_AHEI$tertile_AHEI ==2, ]$fu_mon)
table_AHEI[table_AHEI$tertile == 3,]$person_time <- sum(analysis_mixture_AHEI[analysis_mixture_AHEI$tertile_AHEI ==3, ]$fu_mon)

table_AHEI[table_AHEI$cause=='all_cause' & table_AHEI$tertile == 1,]$n_case <- sum(analysis_mixture_AHEI[analysis_mixture_AHEI$tertile_AHEI ==1, ]$death)
table_AHEI[table_AHEI$cause=='cancer' & table_AHEI$tertile == 1,]$n_case <- sum(analysis_mixture_AHEI[analysis_mixture_AHEI$tertile_AHEI ==1, ]$cancer_death)

table_AHEI[table_AHEI$cause=='all_cause' & table_AHEI$tertile == 2,]$n_case <- sum(analysis_mixture_AHEI[analysis_mixture_AHEI$tertile_AHEI ==2, ]$death)
table_AHEI[table_AHEI$cause=='cancer' & table_AHEI$tertile == 2,]$n_case <- sum(analysis_mixture_AHEI[analysis_mixture_AHEI$tertile_AHEI ==2, ]$cancer_death)

table_AHEI[table_AHEI$cause=='all_cause' & table_AHEI$tertile == 3,]$n_case <- sum(analysis_mixture_AHEI[analysis_mixture_AHEI$tertile_AHEI ==3, ]$death)
table_AHEI[table_AHEI$cause=='cancer' & table_AHEI$tertile == 3,]$n_case <- sum(analysis_mixture_AHEI[analysis_mixture_AHEI$tertile_AHEI ==3, ]$cancer_death)


fit_cvd <- qgcomp.emm.cox.noboot(Surv(fu_mon, cvd_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                                   age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                                   income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                                   as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                                 expnms=Xnm,
                                 emmvar = 'tertile_AHEI',
                                 data=analysis_mixture_AHEI[analysis_mixture_AHEI$cycle !='2015-2016',], q=3,
                                 weight = analysis_mixture_AHEI[analysis_mixture_AHEI$cycle !='2015-2016',]$weight_phth)
table_AHEI[table_AHEI$tertile == 1 & table_AHEI$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cvd, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_AHEI[table_AHEI$tertile == 2 & table_AHEI$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cvd, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_AHEI[table_AHEI$tertile == 3 & table_AHEI$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cvd, emmval=3)[2,c('hr','ll.hr','ul.hr')]


table_AHEI[table_AHEI$cause == 'heart', 'pval'] <- linearHypothesis(
  fit_cvd,
  c("tertile_AHEI2:mixture = 0", 
    "tertile_AHEI3:mixture = 0"),
  vcov. = vcov(fit_cvd)
)$`Pr(>Chisq)`[2]


table_AHEI[table_AHEI$tertile == 1 & table_AHEI$cause == 'heart',]$person_time <- sum(analysis_mixture_AHEI[analysis_mixture_AHEI$cycle !='2015-2016' & analysis_mixture_AHEI$tertile_AHEI ==1, ]$fu_mon)
table_AHEI[table_AHEI$tertile == 2 & table_AHEI$cause == 'heart',]$person_time <- sum(analysis_mixture_AHEI[analysis_mixture_AHEI$cycle !='2015-2016' & analysis_mixture_AHEI$tertile_AHEI ==2, ]$fu_mon)
table_AHEI[table_AHEI$tertile == 3 & table_AHEI$cause == 'heart',]$person_time <- sum(analysis_mixture_AHEI[analysis_mixture_AHEI$cycle !='2015-2016' & analysis_mixture_AHEI$tertile_AHEI ==3, ]$fu_mon)

table_AHEI[table_AHEI$tertile == 1 & table_AHEI$cause == 'heart',]$n_case <- sum(analysis_mixture_AHEI[analysis_mixture_AHEI$cycle !='2015-2016' & analysis_mixture_AHEI$tertile_AHEI ==1, ]$cvd_death)
table_AHEI[table_AHEI$tertile == 2 & table_AHEI$cause == 'heart',]$n_case <- sum(analysis_mixture_AHEI[analysis_mixture_AHEI$cycle !='2015-2016' & analysis_mixture_AHEI$tertile_AHEI ==2, ]$cvd_death)
table_AHEI[table_AHEI$tertile == 3 & table_AHEI$cause == 'heart',]$n_case <- sum(analysis_mixture_AHEI[analysis_mixture_AHEI$cycle !='2015-2016' & analysis_mixture_AHEI$tertile_AHEI ==3, ]$cvd_death)

#### sensitivity analysis ####
##### 1. QGC with  DEHP sum instead of individual metabolites ####
qgc_output_update_DEHP_sum <- as.data.frame(matrix(NA, ncol=7, nrow=4))
colnames(qgc_output_update_DEHP_sum) <- c('HR','lowCI','upperCI','cause','n_case','person_time','mixture')
qgc_output_update_DEHP_sum$cause <- c('all_cause','heart','cancer','injury')
qgc_output_update_DEHP_sum$mixture <- c(rep('mixture1',4))

Xnm <- c('MBP_imp_cratio', 'MEP_imp_cratio', 'MBzP_imp_cratio',  'MIBP_imp_cratio',
         'sum_DEHP_imp_cratio',
         'BPA_imp_cratio')
covariate <- c('age_imp','female_imp','educ_adult_imp','race2_imp','marital_imp','bmi_imp','cotinine_imp','income_ratio_imp','AHEI_ALL_imp','cycle_imp','mid_year_imp','alco_imp_45','MET_quar_imp','log_creatinine')

## all cause ##
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='all_cause' & qgc_output_update_DEHP_sum$mixture == 'mixture1','n_case']<- sum(analysis_mixture$death)
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='all_cause' & qgc_output_update_DEHP_sum$mixture == 'mixture1','person_time']<- sum(analysis_mixture$fu_mon)
qgc_fit <- qgcomp.cox.noboot(Surv(fu_mon, death) ~ .,expnms=Xnm,
                             data=analysis_mixture[,c(Xnm, covariate, 'fu_mon', 'death')], q=3, weights=analysis_mixture$weight_phth)
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='all_cause' & qgc_output_update_DEHP_sum$mixture == 'mixture1','HR'] <- exp(qgc_fit$psi)
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='all_cause' & qgc_output_update_DEHP_sum$mixture == 'mixture1','lowCI']  <- exp(qgc_fit$ci)[1]
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='all_cause' & qgc_output_update_DEHP_sum$mixture == 'mixture1','upperCI']  <- exp(qgc_fit$ci)[2]

qgc_fit$pos.psi
qgc_fit$neg.psi
qgc_fit$neg.weights
qgc_fit$pos.weights


## cause specific ##
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='cancer' & qgc_output_update_DEHP_sum$mixture == 'mixture1','n_case']<- sum(analysis_mixture$cancer_death)
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='cancer' & qgc_output_update_DEHP_sum$mixture == 'mixture1','person_time']<- sum(analysis_mixture$fu_mon)
qgc_fit <- qgcomp.cox.noboot(Surv(fu_mon, cancer_death) ~ .,expnms=Xnm,
                             data=analysis_mixture[,c(Xnm, covariate, 'fu_mon', 'cancer_death')], q=3, weights=analysis_mixture$weight_phth)
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='cancer' & qgc_output_update_DEHP_sum$mixture == 'mixture1','HR'] <- exp(qgc_fit$psi)
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='cancer' & qgc_output_update_DEHP_sum$mixture == 'mixture1','lowCI']  <- exp(qgc_fit$ci)[1]
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='cancer' & qgc_output_update_DEHP_sum$mixture == 'mixture1','upperCI']  <- exp(qgc_fit$ci)[2]

qgc_fit$pos.psi
qgc_fit$neg.psi
qgc_fit$neg.weights
qgc_fit$pos.weights

# removed 2015-2016 cycles for CVD analyses
analysis_mixture_cvd <- analysis_mixture[analysis_mixture$cycle !='2015-2016',]
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='heart' & qgc_output_update_DEHP_sum$mixture == 'mixture1','n_case']<- sum(analysis_mixture_cvd$cvd_death)
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='heart' & qgc_output_update_DEHP_sum$mixture == 'mixture1','person_time']<- sum(analysis_mixture_cvd$fu_mon)
qgc_fit <- qgcomp.cox.noboot(Surv(fu_mon, cvd_death) ~ .,expnms=Xnm,
                             data=analysis_mixture_cvd[,c(Xnm, covariate, 'fu_mon', 'cvd_death')], q=3, weights=analysis_mixture_cvd$weight_phth)
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='heart' & qgc_output_update_DEHP_sum$mixture == 'mixture1','HR'] <- exp(qgc_fit$psi)
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='heart' & qgc_output_update_DEHP_sum$mixture == 'mixture1','lowCI']  <- exp(qgc_fit$ci)[1]
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='heart' & qgc_output_update_DEHP_sum$mixture == 'mixture1','upperCI']  <- exp(qgc_fit$ci)[2]


qgc_fit$pos.psi
qgc_fit$neg.psi
qgc_fit$neg.weights
qgc_fit$pos.weights

qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='injury' & qgc_output_update_DEHP_sum$mixture == 'mixture1','n_case']<- sum(analysis_mixture_cvd$injury_death)
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='injury' & qgc_output_update_DEHP_sum$mixture == 'mixture1','person_time']<- sum(analysis_mixture_cvd$fu_mon)
qgc_fit <- qgcomp.cox.noboot(Surv(fu_mon, injury_death) ~ .,expnms=Xnm,
                             data=analysis_mixture_cvd[,c(Xnm, covariate, 'fu_mon', 'injury_death')], q=3, weights=analysis_mixture_cvd$weight_phth)
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='injury' & qgc_output_update_DEHP_sum$mixture == 'mixture1','HR'] <- exp(qgc_fit$psi)
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='injury' & qgc_output_update_DEHP_sum$mixture == 'mixture1','lowCI']  <- exp(qgc_fit$ci)[1]
qgc_output_update_DEHP_sum[qgc_output_update_DEHP_sum$cause=='injury' & qgc_output_update_DEHP_sum$mixture == 'mixture1','upperCI']  <- exp(qgc_fit$ci)[2]

##### 2. EMM among total population #####
###### folate ######
library(qgcompint)
analysis_mixture_fol <- analysis_mixture %>% drop_na(fol_rbc)
analysis_mixture_fol <- analysis_mixture_fol %>% group_by(cycle) %>%
  mutate(tertile_fol_rbc = cut(fol_rbc, breaks = quantile(fol_rbc, probs = seq(0, 1, 1/3), na.rm = TRUE),
                               include.lowest = TRUE,
                               labels = c(1, 2, 3)))
analysis_mixture_fol <- as.data.frame(analysis_mixture_fol)
table_fol_total <- as.data.frame(matrix(NA, ncol=8, nrow=9))
colnames(table_fol_total) <- c('HR','lowCI','upperCI','tertile','pval','cause','n_case','person_time')
table_fol_total$tertile <- rep(c(1,2,3),3)
table_fol_total$cause <- c(rep('all_cause',3), rep('heart',3), rep('cancer',3))

fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                               age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'tertile_fol_rbc',
                             data=analysis_mixture_fol, q=3, weights = analysis_mixture_fol$weight_phth)
table_fol_total[table_fol_total$tertile == 1 & table_fol_total$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_fol_total[table_fol_total$tertile == 2 & table_fol_total$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_fol_total[table_fol_total$tertile == 3 & table_fol_total$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=3)[2,c('hr','ll.hr','ul.hr')]



fit_cancer <- qgcomp.emm.cox.noboot(Surv(fu_mon, cancer_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                                      age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                                      income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                                      as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                                    expnms=Xnm,
                                    emmvar = 'tertile_fol_rbc',
                                    data=analysis_mixture_fol, q=3, weights = analysis_mixture_fol$weight_phth)
table_fol_total[table_fol_total$tertile == 1 & table_fol_total$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cancer, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_fol_total[table_fol_total$tertile == 2 & table_fol_total$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cancer, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_fol_total[table_fol_total$tertile == 3 & table_fol_total$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cancer, emmval=3)[2,c('hr','ll.hr','ul.hr')]


table_fol_total[table_fol_total$tertile == 1,]$person_time <- sum(analysis_mixture_fol[analysis_mixture_fol$tertile_fol_rbc ==1, ]$fu_mon)
table_fol_total[table_fol_total$tertile == 2,]$person_time <- sum(analysis_mixture_fol[analysis_mixture_fol$tertile_fol_rbc ==2, ]$fu_mon)
table_fol_total[table_fol_total$tertile == 3,]$person_time <- sum(analysis_mixture_fol[analysis_mixture_fol$tertile_fol_rbc ==3, ]$fu_mon)

table_fol_total[table_fol_total$cause=='all_cause' & table_fol_total$tertile == 1,]$n_case <- sum(analysis_mixture_fol[analysis_mixture_fol$tertile_fol_rbc ==1, ]$death)
table_fol_total[table_fol_total$cause=='cancer' & table_fol_total$tertile == 1,]$n_case <- sum(analysis_mixture_fol[analysis_mixture_fol$tertile_fol_rbc ==1, ]$cancer_death)

table_fol_total[table_fol_total$cause=='all_cause' & table_fol_total$tertile == 2,]$n_case <- sum(analysis_mixture_fol[analysis_mixture_fol$tertile_fol_rbc ==2, ]$death)
table_fol_total[table_fol_total$cause=='cancer' & table_fol_total$tertile == 2,]$n_case <- sum(analysis_mixture_fol[analysis_mixture_fol$tertile_fol_rbc ==2, ]$cancer_death)

table_fol_total[table_fol_total$cause=='all_cause' & table_fol_total$tertile == 3,]$n_case <- sum(analysis_mixture_fol[analysis_mixture_fol$tertile_fol_rbc ==3, ]$death)
table_fol_total[table_fol_total$cause=='cancer' & table_fol_total$tertile == 3,]$n_case <- sum(analysis_mixture_fol[analysis_mixture_fol$tertile_fol_rbc ==3, ]$cancer_death)


fit_cvd <- qgcomp.emm.cox.noboot(Surv(fu_mon, cvd_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                                   age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                                   income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                                   as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                                 expnms=Xnm,
                                 emmvar = 'tertile_fol_rbc',
                                 data=analysis_mixture_fol[analysis_mixture_fol$cycle != '2015-2016',], q=3, weights = analysis_mixture_fol[analysis_mixture_fol$cycle != '2015-2016',]$weight_phth)
table_fol_total[table_fol_total$tertile == 1 & table_fol_total$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cvd, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_fol_total[table_fol_total$tertile == 2 & table_fol_total$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cvd, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_fol_total[table_fol_total$tertile == 3 & table_fol_total$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cvd, emmval=3)[2,c('hr','ll.hr','ul.hr')]


table_fol_total[table_fol_total$tertile == 1 & table_fol_total$cause=='heart',]$person_time <- sum(analysis_mixture_fol[analysis_mixture_fol$cycle !='2015-2016' & analysis_mixture_fol$tertile_fol_rbc ==1, ]$fu_mon)
table_fol_total[table_fol_total$tertile == 2 & table_fol_total$cause=='heart',]$person_time <- sum(analysis_mixture_fol[analysis_mixture_fol$cycle !='2015-2016' & analysis_mixture_fol$tertile_fol_rbc ==2, ]$fu_mon)
table_fol_total[table_fol_total$tertile == 3 & table_fol_total$cause=='heart',]$person_time <- sum(analysis_mixture_fol[analysis_mixture_fol$cycle !='2015-2016' & analysis_mixture_fol$tertile_fol_rbc ==3, ]$fu_mon)

table_fol_total[table_fol_total$cause=='heart' & table_fol_total$tertile == 1,]$n_case <- sum(analysis_mixture_fol[analysis_mixture_fol$cycle !='2015-2016' & analysis_mixture_fol$tertile_fol_rbc ==1, ]$cvd_death)
table_fol_total[table_fol_total$cause=='heart' & table_fol_total$tertile == 2,]$n_case <- sum(analysis_mixture_fol[analysis_mixture_fol$cycle !='2015-2016' & analysis_mixture_fol$tertile_fol_rbc ==2, ]$cvd_death)
table_fol_total[table_fol_total$cause=='heart' & table_fol_total$tertile == 3,]$n_case <- sum(analysis_mixture_fol[analysis_mixture_fol$cycle !='2015-2016' & analysis_mixture_fol$tertile_fol_rbc ==3, ]$cvd_death)

###### vitamin D ######
analysis_mixture_vitD <- analysis_mixture %>% drop_na(vitD)
analysis_mixture_vitD <- analysis_mixture_vitD %>% 
  mutate(tertile_vitD = cut(vitD, breaks = quantile(vitD, probs = seq(0, 1, 1/3), na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = c(1, 2, 3)))

analysis_mixture_vitD <- as.data.frame(analysis_mixture_vitD)

table_vitD_total <- as.data.frame(matrix(NA, ncol=8, nrow=9))
colnames(table_vitD_total) <- c('HR','lowCI','upperCI','tertile','pval','cause','n_case','person_time')
table_vitD_total$tertile <- rep(c(1,2,3),3)
table_vitD_total$cause <- c(rep('all_cause',3), rep('heart',3), rep('cancer',3))


fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                               age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'tertile_vitD',
                             data=analysis_mixture_vitD, q=3, weights = analysis_mixture_vitD$weight_phth)
table_vitD_total[table_vitD_total$tertile == 1 & table_vitD_total$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_vitD_total[table_vitD_total$tertile == 2 & table_vitD_total$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_vitD_total[table_vitD_total$tertile == 3 & table_vitD_total$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=3)[2,c('hr','ll.hr','ul.hr')]


fit_cancer <- qgcomp.emm.cox.noboot(Surv(fu_mon, cancer_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                                      age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                                      income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                                      as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                                    expnms=Xnm,
                                    emmvar = 'tertile_vitD',
                                    data=analysis_mixture_vitD, q=3, weights = analysis_mixture_vitD$weight_phth)
table_vitD_total[table_vitD_total$tertile == 1 & table_vitD_total$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cancer, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_vitD_total[table_vitD_total$tertile == 2 & table_vitD_total$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cancer, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_vitD_total[table_vitD_total$tertile == 3 & table_vitD_total$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cancer, emmval=3)[2,c('hr','ll.hr','ul.hr')]




table_vitD_total[table_vitD_total$tertile == 1,]$person_time <- sum(analysis_mixture_vitD[analysis_mixture_vitD$tertile_vitD ==1, ]$fu_mon)
table_vitD_total[table_vitD_total$tertile == 2,]$person_time <- sum(analysis_mixture_vitD[analysis_mixture_vitD$tertile_vitD ==2, ]$fu_mon)
table_vitD_total[table_vitD_total$tertile == 3,]$person_time <- sum(analysis_mixture_vitD[analysis_mixture_vitD$tertile_vitD ==3, ]$fu_mon)

table_vitD_total[table_vitD_total$cause=='all_cause' & table_vitD_total$tertile == 1,]$n_case <- sum(analysis_mixture_vitD[analysis_mixture_vitD$tertile_vitD ==1, ]$death)
table_vitD_total[table_vitD_total$cause=='cancer' & table_vitD_total$tertile == 1,]$n_case <- sum(analysis_mixture_vitD[analysis_mixture_vitD$tertile_vitD ==1, ]$cancer_death)

table_vitD_total[table_vitD_total$cause=='all_cause' & table_vitD_total$tertile == 2,]$n_case <- sum(analysis_mixture_vitD[analysis_mixture_vitD$tertile_vitD ==2, ]$death)
table_vitD_total[table_vitD_total$cause=='cancer' & table_vitD_total$tertile == 2,]$n_case <- sum(analysis_mixture_vitD[analysis_mixture_vitD$tertile_vitD ==2, ]$cancer_death)

table_vitD_total[table_vitD_total$cause=='all_cause' & table_vitD_total$tertile == 3,]$n_case <- sum(analysis_mixture_vitD[analysis_mixture_vitD$tertile_vitD ==3, ]$death)
table_vitD_total[table_vitD_total$cause=='cancer' & table_vitD_total$tertile == 3,]$n_case <- sum(analysis_mixture_vitD[analysis_mixture_vitD$tertile_vitD ==3, ]$cancer_death)


fit_cvd <- qgcomp.emm.cox.noboot(Surv(fu_mon, cvd_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                                   age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                                   income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                                   as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                                 expnms=Xnm,
                                 emmvar = 'tertile_vitD',
                                 data=analysis_mixture_vitD[analysis_mixture_vitD$cycle != '2015-2016',], q=3, weights = analysis_mixture_vitD[analysis_mixture_vitD$cycle != '2015-2016',]$weight_phth)
table_vitD_total[table_vitD_total$tertile == 1 & table_vitD_total$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cvd, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_vitD_total[table_vitD_total$tertile == 2 & table_vitD_total$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cvd, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_vitD_total[table_vitD_total$tertile == 3 & table_vitD_total$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit_cvd, emmval=3)[2,c('hr','ll.hr','ul.hr')]


table_vitD_total[table_vitD_total$tertile == 1 & table_vitD_total$cause == 'heart',]$person_time <- sum(analysis_mixture_vitD[analysis_mixture_vitD$cycle != '2015-2016' & analysis_mixture_vitD$tertile_vitD ==1, ]$fu_mon)
table_vitD_total[table_vitD_total$tertile == 2 & table_vitD_total$cause == 'heart',]$person_time <- sum(analysis_mixture_vitD[analysis_mixture_vitD$cycle != '2015-2016' & analysis_mixture_vitD$tertile_vitD ==2, ]$fu_mon)
table_vitD_total[table_vitD_total$tertile == 3 & table_vitD_total$cause == 'heart',]$person_time <- sum(analysis_mixture_vitD[analysis_mixture_vitD$cycle != '2015-2016' & analysis_mixture_vitD$tertile_vitD ==3, ]$fu_mon)

table_vitD_total[table_vitD_total$cause=='heart' & table_vitD_total$tertile == 1,]$n_case <- sum(analysis_mixture_vitD[analysis_mixture_vitD$cycle != '2015-2016' & analysis_mixture_vitD$tertile_vitD ==1, ]$cvd_death)
table_vitD_total[table_vitD_total$cause=='heart' & table_vitD_total$tertile == 2,]$n_case <- sum(analysis_mixture_vitD[analysis_mixture_vitD$cycle != '2015-2016' & analysis_mixture_vitD$tertile_vitD ==2, ]$cvd_death)
table_vitD_total[table_vitD_total$cause=='heart' & table_vitD_total$tertile == 3,]$n_case <- sum(analysis_mixture_vitD[analysis_mixture_vitD$cycle != '2015-2016' & analysis_mixture_vitD$tertile_vitD ==3, ]$cvd_death)



###### vitamin B6 ######
analysis_mixture_vitB6 <- analysis_mixture %>% drop_na(vitB6)

analysis_mixture_vitB6 <- analysis_mixture_vitB6 %>% 
  mutate(tertile_vitB6 = cut(vitB6, breaks = quantile(vitB6, probs = seq(0, 1, 1/3), na.rm = TRUE),
                             include.lowest = TRUE,
                             labels = c(1, 2, 3)))

analysis_mixture_vitB6 <- as.data.frame(analysis_mixture_vitB6)
table_vitB6_total <- as.data.frame(matrix(NA, ncol=8, nrow=9))
colnames(table_vitB6_total) <- c('HR','lowCI','upperCI','tertile','pval','cause','n_case','person_time')
table_vitB6_total$tertile <- c(1,2,3)
table_vitB6_total$cause <- c(rep('all_cause',3), rep('heart',3), rep('cancer',3))

fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                               age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'tertile_vitB6',
                             data=analysis_mixture_vitB6, q=3, weights = analysis_mixture_vitB6$weight_phth)
table_vitB6_total[table_vitB6_total$tertile == 1 & table_vitB6_total$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_vitB6_total[table_vitB6_total$tertile == 2 & table_vitB6_total$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_vitB6_total[table_vitB6_total$tertile == 3 & table_vitB6_total$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=3)[2,c('hr','ll.hr','ul.hr')]


fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, cancer_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                               age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'tertile_vitB6',
                             data=analysis_mixture_vitB6, q=3, weights = analysis_mixture_vitB6$weight_phth)
table_vitB6_total[table_vitB6_total$tertile == 1 & table_vitB6_total$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_vitB6_total[table_vitB6_total$tertile == 2 & table_vitB6_total$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_vitB6_total[table_vitB6_total$tertile == 3 & table_vitB6_total$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=3)[2,c('hr','ll.hr','ul.hr')]

fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, cvd_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                               age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'tertile_vitB6',
                             data=analysis_mixture_vitB6[analysis_mixture_vitB6$cycle != '2015-2016',], q=3, weights = analysis_mixture_vitB6[analysis_mixture_vitB6$cycle != '2015-2016',]$weight_phth)
table_vitB6_total[table_vitB6_total$tertile == 1 & table_vitB6_total$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_vitB6_total[table_vitB6_total$tertile == 2 & table_vitB6_total$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_vitB6_total[table_vitB6_total$tertile == 3 & table_vitB6_total$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=3)[2,c('hr','ll.hr','ul.hr')]


table_vitB6_total[table_vitB6_total$tertile == 1,]$person_time <- sum(analysis_mixture_vitB6[analysis_mixture_vitB6$tertile_vitB6 ==1, ]$fu_mon)
table_vitB6_total[table_vitB6_total$tertile == 2,]$person_time <- sum(analysis_mixture_vitB6[analysis_mixture_vitB6$tertile_vitB6 ==2, ]$fu_mon)
table_vitB6_total[table_vitB6_total$tertile == 3,]$person_time <- sum(analysis_mixture_vitB6[analysis_mixture_vitB6$tertile_vitB6 ==3, ]$fu_mon)


table_vitB6_total[table_vitB6_total$cause=='all_cause' & table_vitB6_total$tertile == 1,]$n_case <- sum(analysis_mixture_vitB6[analysis_mixture_vitB6$tertile_vitB6 ==1, ]$death)
table_vitB6_total[table_vitB6_total$cause=='all_cause' & table_vitB6_total$tertile == 2,]$n_case <- sum(analysis_mixture_vitB6[analysis_mixture_vitB6$tertile_vitB6 ==2, ]$death)
table_vitB6_total[table_vitB6_total$cause=='all_cause' & table_vitB6_total$tertile == 3,]$n_case <- sum(analysis_mixture_vitB6[analysis_mixture_vitB6$tertile_vitB6 ==3, ]$death)


table_vitB6_total[table_vitB6_total$cause=='cancer' & table_vitB6_total$tertile == 1,]$n_case <- sum(analysis_mixture_vitB6[analysis_mixture_vitB6$tertile_vitB6 ==1, ]$cancer_death)
table_vitB6_total[table_vitB6_total$cause=='cancer' & table_vitB6_total$tertile == 2,]$n_case <- sum(analysis_mixture_vitB6[analysis_mixture_vitB6$tertile_vitB6 ==2, ]$cancer_death)
table_vitB6_total[table_vitB6_total$cause=='cancer' & table_vitB6_total$tertile == 3,]$n_case <- sum(analysis_mixture_vitB6[analysis_mixture_vitB6$tertile_vitB6 ==3, ]$cancer_death)


table_vitB6_total[table_vitB6_total$cause=='heart' & table_vitB6_total$tertile == 1,]$n_case <- sum(analysis_mixture_vitB6[analysis_mixture_vitB6$cycle != '2015-2016' & analysis_mixture_vitB6$tertile_vitB6 ==1, ]$cvd_death)
table_vitB6_total[table_vitB6_total$cause=='heart' & table_vitB6_total$tertile == 2,]$n_case <- sum(analysis_mixture_vitB6[analysis_mixture_vitB6$cycle != '2015-2016' & analysis_mixture_vitB6$tertile_vitB6 ==2, ]$cvd_death)
table_vitB6_total[table_vitB6_total$cause=='heart' & table_vitB6_total$tertile == 3,]$n_case <- sum(analysis_mixture_vitB6[analysis_mixture_vitB6$cycle != '2015-2016' & analysis_mixture_vitB6$tertile_vitB6 ==3, ]$cvd_death)

table_vitB6_total[table_vitB6_total$cause=='heart' & table_vitB6_total$tertile == 1,]$person_time <- sum(analysis_mixture_vitB6[analysis_mixture_vitB6$cycle != '2015-2016' & analysis_mixture_vitB6$tertile_vitB6 ==1, ]$fu_mon)
table_vitB6_total[table_vitB6_total$cause=='heart' & table_vitB6_total$tertile == 2,]$person_time <- sum(analysis_mixture_vitB6[analysis_mixture_vitB6$cycle != '2015-2016' & analysis_mixture_vitB6$tertile_vitB6 ==2, ]$fu_mon)
table_vitB6_total[table_vitB6_total$cause=='heart' & table_vitB6_total$tertile == 3,]$person_time <- sum(analysis_mixture_vitB6[analysis_mixture_vitB6$cycle != '2015-2016' & analysis_mixture_vitB6$tertile_vitB6 ==3, ]$fu_mon)


###### vitamin B12 ######
analysis_mixture_vitB12 <- analysis_mixture %>% drop_na(vitB12)
analysis_mixture_vitB12 <- analysis_mixture_vitB12 %>% 
  mutate(tertile_vitB12 = cut(vitB12, breaks = quantile(vitB12, probs = seq(0, 1, 1/3), na.rm = TRUE),
                              include.lowest = TRUE,
                              labels = c(1, 2, 3)))

analysis_mixture_vitB12 <- as.data.frame(analysis_mixture_vitB12)

table_vitB12_total <- as.data.frame(matrix(NA, ncol=8, nrow=9))
colnames(table_vitB12_total) <- c('HR','lowCI','upperCI','tertile','pval','cause','n_case','person_time')
table_vitB12_total$tertile <- c(1,2,3)
table_vitB12_total$cause <- c(rep('all_cause',3), rep('heart',3), rep('cancer',3))

fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                               age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'tertile_vitB12',
                             data=analysis_mixture_vitB12, q=3, weights = analysis_mixture_vitB12$weight_phth)
table_vitB12_total[table_vitB12_total$tertile == 1 & table_vitB12_total$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_vitB12_total[table_vitB12_total$tertile == 2 & table_vitB12_total$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_vitB12_total[table_vitB12_total$tertile == 3 & table_vitB12_total$cause == 'all_cause', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=3)[2,c('hr','ll.hr','ul.hr')]


fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, cancer_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                               age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'tertile_vitB12',
                             data=analysis_mixture_vitB12, q=3, weights = analysis_mixture_vitB12$weight_phth)
table_vitB12_total[table_vitB12_total$tertile == 1 & table_vitB12_total$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_vitB12_total[table_vitB12_total$tertile == 2 & table_vitB12_total$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_vitB12_total[table_vitB12_total$tertile == 3 & table_vitB12_total$cause == 'cancer', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=3)[2,c('hr','ll.hr','ul.hr')]

table_vitB12_total[table_vitB12_total$tertile == 1,]$person_time <- sum(analysis_mixture_vitB12[analysis_mixture_vitB12$tertile_vitB12 ==1, ]$fu_mon)
table_vitB12_total[table_vitB12_total$tertile == 2,]$person_time <- sum(analysis_mixture_vitB12[analysis_mixture_vitB12$tertile_vitB12 ==2, ]$fu_mon)
table_vitB12_total[table_vitB12_total$tertile == 3,]$person_time <- sum(analysis_mixture_vitB12[analysis_mixture_vitB12$tertile_vitB12 ==3, ]$fu_mon)


table_vitB12_total[table_vitB12_total$cause=='all_cause' & table_vitB12_total$tertile == 1,]$n_case <- sum(analysis_mixture_vitB12[analysis_mixture_vitB12$tertile_vitB12 ==1, ]$death)
table_vitB12_total[table_vitB12_total$cause=='all_cause' & table_vitB12_total$tertile == 2,]$n_case <- sum(analysis_mixture_vitB12[analysis_mixture_vitB12$tertile_vitB12 ==2, ]$death)
table_vitB12_total[table_vitB12_total$cause=='all_cause' & table_vitB12_total$tertile == 3,]$n_case <- sum(analysis_mixture_vitB12[analysis_mixture_vitB12$tertile_vitB12 ==3, ]$death)


table_vitB12_total[table_vitB12_total$cause=='cancer' & table_vitB12_total$tertile == 1,]$n_case <- sum(analysis_mixture_vitB12[analysis_mixture_vitB12$tertile_vitB12 ==1, ]$cancer_death)
table_vitB12_total[table_vitB12_total$cause=='cancer' & table_vitB12_total$tertile == 2,]$n_case <- sum(analysis_mixture_vitB12[analysis_mixture_vitB12$tertile_vitB12 ==2, ]$cancer_death)
table_vitB12_total[table_vitB12_total$cause=='cancer' & table_vitB12_total$tertile == 3,]$n_case <- sum(analysis_mixture_vitB12[analysis_mixture_vitB12$tertile_vitB12 ==3, ]$cancer_death)

fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, cvd_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                               age_imp + female_imp + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'tertile_vitB12',
                             data=analysis_mixture_vitB12[analysis_mixture_vitB12$cycle != '2015-2016',], q=3, weights = analysis_mixture_vitB12[analysis_mixture_vitB12$cycle != '2015-2016',]$weight_phth)
table_vitB12_total[table_vitB12_total$tertile == 1 & table_vitB12_total$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
table_vitB12_total[table_vitB12_total$tertile == 2 & table_vitB12_total$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=2)[2,c('hr','ll.hr','ul.hr')]
table_vitB12_total[table_vitB12_total$tertile == 3 & table_vitB12_total$cause == 'heart', c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=3)[2,c('hr','ll.hr','ul.hr')]

table_vitB12_total[table_vitB12_total$cause=='heart' & table_vitB12_total$tertile == 1,]$n_case <- sum(analysis_mixture_vitB12[analysis_mixture_vitB12$cycle != '2015-2016' & analysis_mixture_vitB12$tertile_vitB12 ==1, ]$cvd_death)
table_vitB12_total[table_vitB12_total$cause=='heart' & table_vitB12_total$tertile == 2,]$n_case <- sum(analysis_mixture_vitB12[analysis_mixture_vitB12$cycle != '2015-2016' & analysis_mixture_vitB12$tertile_vitB12 ==2, ]$cvd_death)
table_vitB12_total[table_vitB12_total$cause=='heart' & table_vitB12_total$tertile == 3,]$n_case <- sum(analysis_mixture_vitB12[analysis_mixture_vitB12$cycle != '2015-2016' & analysis_mixture_vitB12$tertile_vitB12 ==3, ]$cvd_death)

table_vitB12_total[table_vitB12_total$cause=='heart' & table_vitB12_total$tertile == 1,]$person_time <- sum(analysis_mixture_vitB12[analysis_mixture_vitB12$cycle != '2015-2016' & analysis_mixture_vitB12$tertile_vitB12 ==1, ]$fu_mon)
table_vitB12_total[table_vitB12_total$cause=='heart' & table_vitB12_total$tertile == 2,]$person_time <- sum(analysis_mixture_vitB12[analysis_mixture_vitB12$cycle != '2015-2016' & analysis_mixture_vitB12$tertile_vitB12 ==2, ]$fu_mon)
table_vitB12_total[table_vitB12_total$cause=='heart' & table_vitB12_total$tertile == 3,]$person_time <- sum(analysis_mixture_vitB12[analysis_mixture_vitB12$cycle != '2015-2016' & analysis_mixture_vitB12$tertile_vitB12 ==3, ]$fu_mon)


##### 3. restriction to elder population ####
mortality_age <- analysis_mixture %>%
  group_by(age) %>%
  summarise(total_deaths = sum(death, na.rm = TRUE),
            total_follow_up = sum(fu_mon, na.rm = TRUE)) %>%
  mutate(mortality_rate = total_deaths / total_follow_up)

analysis_mixture_elder <- analysis_mixture %>% filter(age>=55)

library(qgcomp)
qgc_output_elder <- as.data.frame(matrix(NA, ncol=7, nrow=8))
colnames(qgc_output_elder) <- c('HR','lowCI','upperCI','cause','n_case','person_time','mixture')
qgc_output_elder$cause <- rep(c('all_cause','heart','cancer','injury'),2)
qgc_output_elder$mixture <- c(rep('mixture1',4),rep('mixture2',4))

Xnm <- c('MBP_imp_cratio', 'MEP_imp_cratio', 'MBzP_imp_cratio',  'MIBP_imp_cratio','MEHP_imp_cratio' ,'MEHHP_imp_cratio','MEOHP_imp_cratio','MECPP_imp_cratio',
         'BPA_imp_cratio')
covariate <- c('age_imp','female_imp','educ_adult_imp','race2_imp','marital_imp','bmi_imp','cotinine_imp','income_ratio_imp','AHEI_ALL_imp','cycle_imp','mid_year_imp','alco_imp_45','MET_quar_imp','log_creatinine')

## all cause ##
qgc_output_elder[qgc_output_elder$cause=='all_cause' & qgc_output_elder$mixture == 'mixture1','n_case']<- sum(analysis_mixture_elder$death)
qgc_output_elder[qgc_output_elder$cause=='all_cause' & qgc_output_elder$mixture == 'mixture1','person_time']<- sum(analysis_mixture_elder$fu_mon)
qgc_fit <- qgcomp.cox.noboot(Surv(fu_mon, death) ~ .,expnms=Xnm,
                             data=analysis_mixture_elder[,c(Xnm, covariate, 'fu_mon', 'death')], q=3, weights = analysis_mixture_elder$weight_phth)
qgc_output_elder[qgc_output_elder$cause=='all_cause' & qgc_output_elder$mixture == 'mixture1','HR'] <- exp(qgc_fit$psi)
qgc_output_elder[qgc_output_elder$cause=='all_cause' & qgc_output_elder$mixture == 'mixture1','lowCI']  <- exp(qgc_fit$ci)[1]
qgc_output_elder[qgc_output_elder$cause=='all_cause' & qgc_output_elder$mixture == 'mixture1','upperCI']  <- exp(qgc_fit$ci)[2]

## cause specific ##
qgc_output_elder[qgc_output_elder$cause=='cancer' & qgc_output_elder$mixture == 'mixture1','n_case']<- sum(analysis_mixture_elder$cancer_death)
qgc_output_elder[qgc_output_elder$cause=='cancer' & qgc_output_elder$mixture == 'mixture1','person_time']<- sum(analysis_mixture_elder$fu_mon)
qgc_fit <- qgcomp.cox.noboot(Surv(fu_mon, cancer_death) ~ .,expnms=Xnm,
                             data=analysis_mixture_elder[,c(Xnm, covariate, 'fu_mon', 'cancer_death')], q=3, weights = analysis_mixture_elder$weight_phth)
qgc_output_elder[qgc_output_elder$cause=='cancer' & qgc_output_elder$mixture == 'mixture1','HR'] <- exp(qgc_fit$psi)
qgc_output_elder[qgc_output_elder$cause=='cancer' & qgc_output_elder$mixture == 'mixture1','lowCI']  <- exp(qgc_fit$ci)[1]
qgc_output_elder[qgc_output_elder$cause=='cancer' & qgc_output_elder$mixture == 'mixture1','upperCI']  <- exp(qgc_fit$ci)[2]


# removed 2015-2016 cycle for CVD and accident analyses
analysis_mixture_elder <- analysis_mixture %>% filter(age>=55)
analysis_mixture_cvd_elder <- analysis_mixture %>% filter(age>=55 & cycle !='2015-2016')

qgc_output_elder[qgc_output_elder$cause=='heart' & qgc_output_elder$mixture == 'mixture1','n_case']<- sum(analysis_mixture_cvd_elder$cvd_death)
qgc_output_elder[qgc_output_elder$cause=='heart' & qgc_output_elder$mixture == 'mixture1','person_time']<- sum(analysis_mixture_cvd_elder$fu_mon)
qgc_fit <- qgcomp.cox.noboot(Surv(fu_mon, cvd_death) ~ .,expnms=Xnm,
                             data=analysis_mixture_cvd_elder[,c(Xnm, covariate, 'fu_mon', 'cvd_death')], q=3, weights = analysis_mixture_cvd_elder$weight_phth)
qgc_output_elder[qgc_output_elder$cause=='heart' & qgc_output_elder$mixture == 'mixture1','HR'] <- exp(qgc_fit$psi)
qgc_output_elder[qgc_output_elder$cause=='heart' & qgc_output_elder$mixture == 'mixture1','lowCI']  <- exp(qgc_fit$ci)[1]
qgc_output_elder[qgc_output_elder$cause=='heart' & qgc_output_elder$mixture == 'mixture1','upperCI']  <- exp(qgc_fit$ci)[2]


qgc_output_elder[qgc_output_elder$cause=='injury' & qgc_output_elder$mixture == 'mixture1','n_case']<- sum(analysis_mixture_cvd_elder$injury_death)
qgc_output_elder[qgc_output_elder$cause=='injury' & qgc_output_elder$mixture == 'mixture1','person_time']<- sum(analysis_mixture_cvd_elder$fu_mon)
qgc_fit <- qgcomp.cox.noboot(Surv(fu_mon, injury_death) ~ .,expnms=Xnm,
                             data=analysis_mixture_cvd_elder[,c(Xnm, covariate, 'fu_mon', 'injury_death')], q=3)
qgc_output_elder[qgc_output_elder$cause=='injury' & qgc_output_elder$mixture == 'mixture1','HR'] <- exp(qgc_fit$psi)
qgc_output_elder[qgc_output_elder$cause=='injury' & qgc_output_elder$mixture == 'mixture1','lowCI']  <- exp(qgc_fit$ci)[1]
qgc_output_elder[qgc_output_elder$cause=='injury' & qgc_output_elder$mixture == 'mixture1','upperCI']  <- exp(qgc_fit$ci)[2]


##### 4. sex-specific results ######
library(qgcompint)
qgc_output_sex <- as.data.frame(matrix(NA, ncol=9, nrow=12))
colnames(qgc_output_sex) <- c('HR','lowCI','upperCI','cause','pval','n_case','person_time','mixture','sex')
qgc_output_sex$cause <- rep(c('all_cause','heart','cancer'),4)
qgc_output_sex$mixture <- c(rep('mixture1',6),rep('mixture2',6))
qgc_output_sex$sex <- rep(c(rep('f',3),rep('m',3)),2)

Xnm <- c('MBP_imp_cratio', 'MEP_imp_cratio', 'MBzP_imp_cratio',  'MIBP_imp_cratio', 'MEHP_imp_cratio','MEHHP_imp_cratio','MEOHP_imp_cratio','MECPP_imp_cratio',
         'BPA_imp_cratio')
covariate <- c('age_imp','educ_adult_imp','race2_imp','marital_imp','bmi_imp','cotinine_imp','income_ratio_imp','AHEI_ALL_imp','cycle_imp','mid_year_imp','alco_imp','MET_quar_imp','log_creatinine')

analysis_mixture$female_imp <- as.factor(analysis_mixture$female_imp)
## all cause ##
qgc_output_sex[qgc_output_sex$cause=='all_cause' & qgc_output_sex$sex=='f' & qgc_output_sex$mixture == 'mixture1','n_case']<- sum(analysis_mixture[analysis_mixture$female_imp==1,]$death)
qgc_output_sex[qgc_output_sex$sex=='f' & qgc_output_sex$mixture == 'mixture1','person_time']<- sum(analysis_mixture[analysis_mixture$female_imp==1,]$fu_mon)
qgc_output_sex[qgc_output_sex$cause=='all_cause' & qgc_output_sex$sex=='m' & qgc_output_sex$mixture == 'mixture1','n_case']<- sum(analysis_mixture[analysis_mixture$female_imp==0,]$death)
qgc_output_sex[qgc_output_sex$sex=='m' & qgc_output_sex$mixture == 'mixture1','person_time']<- sum(analysis_mixture[analysis_mixture$female_imp==0,]$fu_mon)

fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                               age_imp  + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'female_imp',
                             data=analysis_mixture, q=3, weights = analysis_mixture$weight_phth)
qgc_output_sex[qgc_output_sex$cause=='all_cause' & qgc_output_sex$sex=='m' & qgc_output_sex$mixture == 'mixture1',c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=0)[2,c('hr','ll.hr','ul.hr')]
qgc_output_sex[qgc_output_sex$cause=='all_cause' & qgc_output_sex$sex=='f' & qgc_output_sex$mixture == 'mixture1',c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
qgc_output_sex[qgc_output_sex$cause=='all_cause' & qgc_output_sex$mixture == 'mixture1','pval'] <- fit$pval[3]

## cause specific ##
qgc_output_sex[qgc_output_sex$cause=='cancer' & qgc_output_sex$sex=='f' & qgc_output_sex$mixture == 'mixture1','n_case']<- sum(analysis_mixture[analysis_mixture$female_imp==1,]$cancer_death)
qgc_output_sex[qgc_output_sex$cause=='cancer' & qgc_output_sex$sex=='m' & qgc_output_sex$mixture == 'mixture1','n_case']<- sum(analysis_mixture[analysis_mixture$female_imp==0,]$cancer_death)

fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, cancer_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                               age_imp  + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'female_imp',
                             data=analysis_mixture, q=3, weights = analysis_mixture$weight_phth)
qgc_output_sex[qgc_output_sex$cause=='cancer' & qgc_output_sex$sex=='m' & qgc_output_sex$mixture == 'mixture1',c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=0)[2,c('hr','ll.hr','ul.hr')]
qgc_output_sex[qgc_output_sex$cause=='cancer' & qgc_output_sex$sex=='f' & qgc_output_sex$mixture == 'mixture1',c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
qgc_output_sex[qgc_output_sex$cause=='cancer' & qgc_output_sex$mixture == 'mixture1','pval'] <- fit$pval[3]

#  removed 15-16 cycle for cvd analysis
qgc_output_sex[qgc_output_sex$cause=='heart' & qgc_output_sex$sex=='f' & qgc_output_sex$mixture == 'mixture1','n_case']<- sum(analysis_mixture[analysis_mixture$female_imp==1 & analysis_mixture$cycle !='2015-2016',]$cvd_death)
qgc_output_sex[qgc_output_sex$cause=='heart' & qgc_output_sex$sex=='m' & qgc_output_sex$mixture == 'mixture1','n_case']<- sum(analysis_mixture[analysis_mixture$female_imp==0 & analysis_mixture$cycle !='2015-2016',]$cvd_death)

qgc_output_sex[qgc_output_sex$sex=='f' & qgc_output_sex$mixture == 'mixture1' & qgc_output_sex$cause=='heart','person_time']<- sum(analysis_mixture[analysis_mixture$female_imp==1 & analysis_mixture$cycle !='2015-2016',]$fu_mon)
qgc_output_sex[qgc_output_sex$sex=='m' & qgc_output_sex$mixture == 'mixture1' & qgc_output_sex$cause=='heart','person_time']<- sum(analysis_mixture[analysis_mixture$female_imp==0 & analysis_mixture$cycle !='2015-2016',]$fu_mon)

fit <- qgcomp.emm.cox.noboot(Surv(fu_mon, cvd_death) ~ MBP_imp_cratio + MEP_imp_cratio + MBzP_imp_cratio + MIBP_imp_cratio + MEHP_imp_cratio + MEHHP_imp_cratio + MEOHP_imp_cratio + MECPP_imp_cratio + BPA_imp_cratio +
                               age_imp  + as.factor(educ_adult_imp) + as.factor(race2_imp) + as.factor(marital_imp) + bmi_imp + cotinine_imp + 
                               income_ratio_imp + AHEI_ALL_imp + as.factor(cycle_imp) + as.factor(mid_year_imp) +
                               as.factor(alco_imp_45) + as.factor(MET_quar_imp) + log_creatinine,
                             expnms=Xnm,
                             emmvar = 'female_imp',
                             data=analysis_mixture[analysis_mixture$cycle !='2015-2016',], q=3, weights = analysis_mixture[analysis_mixture$cycle !='2015-2016',]$weight_phth)
qgc_output_sex[qgc_output_sex$cause=='heart' & qgc_output_sex$sex=='m' & qgc_output_sex$mixture == 'mixture1',c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=0)[2,c('hr','ll.hr','ul.hr')]
qgc_output_sex[qgc_output_sex$cause=='heart' & qgc_output_sex$sex=='f' & qgc_output_sex$mixture == 'mixture1',c('HR','lowCI','upperCI')] <- pointwisebound(fit, emmval=1)[2,c('hr','ll.hr','ul.hr')]
qgc_output_sex[qgc_output_sex$cause=='heart' & qgc_output_sex$mixture == 'mixture1','pval'] <- fit$pval[3]

##### 6. other risk factors to obtain PAFs ######
# use all available population in 2005-2016 cycles to obtain estimates for other risk factors

PAF <- function(point, lowCI, upperCI, prevalence){
  PAF_point = (prevalence *(point-1)/(prevalence*(point-1)+1))*100
  PAF_lowCI = (prevalence *(lowCI-1)/(prevalence*(lowCI-1)+1))*100
  PAF_upperCI = (prevalence *(upperCI-1)/(prevalence*(upperCI-1)+1))*100
  print(c(PAF_point,PAF_lowCI,PAF_upperCI))
}

## factor 1: cotinine ≥ 10 ##
analysis$cotinine_10 <- ifelse(analysis$cotinine>=10,1,0) 
prop.table(table(analysis$cotinine_10)) # 0.2583
design <- svydesign(data = analysis, id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~ weight_demo, nest = TRUE)

fit <- svycoxph(Surv(fu_mon, death) ~  as.factor(cotinine_10) + alco + 
                  age_imp + female_imp + bmi_imp + as.factor(educ_adult_imp) +
                  as.factor(race2_imp) + as.factor(marital_imp) + income_ratio_imp + AHEI_ALL_imp + as.factor(MET_quar_imp) +
                  as.factor(cycle_imp) + mid_year_imp  , 
                design = design, data = analysis)
summary(fit)$conf.int[1,] # 1.79 (1.54, 2.07)
fit$n # n=20642

PAF(1.79, 1.54, 2.07,0.25)
# 16.49269% (11.89427, 21.10454)

## factor 2: physical inactivity  ##
analysis$MET_quar_1quar <- ifelse(analysis$MET_quar==1,1,0)
design <- svydesign(data = analysis, id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~ weight_demo, nest = TRUE)
fit <- svycoxph(Surv(fu_mon, death) ~ as.factor(MET_quar_1quar) + 
                  AHEI_ALL_imp  + cotinine_imp + 
                  alco_imp + 
                  age_imp + female_imp + bmi_imp + as.factor(educ_adult_imp) +
                  as.factor(race2_imp) + as.factor(marital_imp) + income_ratio_imp +
                  as.factor(cycle_imp) + mid_year_imp  , 
                design = design, data = analysis)
summary(fit)$conf.int[1,] # 1.15 (1.00, 1.31)
fit$n # n=20882
PAF(1.15,1.00255,1.31,0.25)
# 3.614458 (0.06370939, 7.192575)

### creating figure for PAF ###
PAF <- data.frame(
  Category = c("Plasticizer Mixture", "Tobacco",  "Physical Inactivity"),
  point = c(10.31, 16.49, 3.61),
  lowCI = c(0.78, 11.89,0.06),
  highCI = c(20.38, 21.10,7.19)
)

ggplot(PAF, aes(x = reorder(Category, -point), y = point)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "skyblue") +
  geom_errorbar(aes(ymin = lowCI, ymax = highCI), 
                width = 0.2, color = "blue") +
  geom_text(aes(label = paste0(point, "%")), 
            hjust = 1.05,vjust=-0.5, color = "blue", fontface = "bold", size = 6) + 
  labs(title = "Population Attributable Fraction of All-Cause Mortality", x = "", y = "Percentage", size = 12) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16),      
        axis.title.x = element_text(size = 14),   
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 14))


##### 7. QGC - complete case analysis [sensitivity to test covariate missingness] #####
covariate_ori <- c('age','female','educ_adult','race2','marital','bmi','cotinine','income_ratio','AHEI_ALL','cycle','mid_year','alco_45','MET_quar','log_creatinine')
analysis_mixture_complete <- analysis_mixture %>% drop_na(covariate_ori) # n=4986

library(qgcomp)
qgc_output_svy_complete <- as.data.frame(matrix(NA, ncol=7, nrow=4))
colnames(qgc_output_svy_complete) <- c('HR','lowCI','upperCI','cause','n_case','person_time','mixture')
qgc_output_svy_complete$cause <- c('all_cause','heart','cancer','injury')
qgc_output_svy_complete$mixture <- c(rep('mixture1',4))

Xnm <- c('MBP_imp_cratio', 'MEP_imp_cratio', 'MBzP_imp_cratio',  'MIBP_imp_cratio', 
         'MEHP_imp_cratio','MEHHP_imp_cratio','MEOHP_imp_cratio','MECPP_imp_cratio',
         'BPA_imp_cratio')
covariate_ori <- c('age','female','educ_adult','race2','marital','bmi','cotinine','income_ratio','AHEI_ALL','cycle','mid_year','alco_45','MET_quar','log_creatinine')
## all cause ##
qgc_output_svy_complete[qgc_output_svy_complete$cause=='all_cause' & qgc_output_svy_complete$mixture == 'mixture1','n_case']<- sum(analysis_mixture_complete$death)
qgc_output_svy_complete[qgc_output_svy_complete$cause=='all_cause' & qgc_output_svy_complete$mixture == 'mixture1','person_time']<- sum(analysis_mixture_complete$fu_mon)
qgc_fit <- qgcomp.cox.noboot(Surv(fu_mon, death) ~ .,expnms=Xnm, 
                             data=analysis_mixture_complete[,c(Xnm, covariate_ori, 'fu_mon', 'death')], q=3, weight = analysis_mixture_complete$weight_phth)
qgc_output_svy_complete[qgc_output_svy_complete$cause=='all_cause' & qgc_output_svy_complete$mixture == 'mixture1','HR'] <- exp(qgc_fit$psi)
qgc_output_svy_complete[qgc_output_svy_complete$cause=='all_cause' & qgc_output_svy_complete$mixture == 'mixture1','lowCI']  <- exp(qgc_fit$ci)[1]
qgc_output_svy_complete[qgc_output_svy_complete$cause=='all_cause' & qgc_output_svy_complete$mixture == 'mixture1','upperCI']  <- exp(qgc_fit$ci)[2]
# HR: 1.45 (1.00, 2.11)
