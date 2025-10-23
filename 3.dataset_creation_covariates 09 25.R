library(haven)
library(httr)
library(tidyverse)

#### socio-demographic data ####
# 2005-2006
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
demo_05 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

demo_05 <- demo_05 %>% mutate(cycle = '2005-2006') %>% rename(sex = RIAGENDR, age = RIDAGEYR, race = RIDRETH1, income_ratio = INDFMPIR,
                              educ_adult = DMDEDUC2,
                              pregnant = RIDEXPRG,
                              weight_demo = WTMEC2YR,
                              mid_year = RIDEXMON) %>% select(SEQN,SDMVPSU, SDMVSTRA, cycle,mid_year, weight_demo, sex, age,
                                                                 race, income_ratio, educ_adult, pregnant,DMDMARTL)
# 2007-2008
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DEMO_E.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
demo_07 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

demo_07 <- demo_07 %>% mutate(cycle = '2007-2008')%>% rename(sex = RIAGENDR, age = RIDAGEYR, race = RIDRETH1, income_ratio = INDFMPIR,
                              educ_adult = DMDEDUC2,
                              pregnant = RIDEXPRG,
                              weight_demo = WTMEC2YR,
                              mid_year = RIDEXMON) %>% select(SEQN,SDMVPSU, SDMVSTRA, cycle,mid_year, weight_demo, sex, age,
                                                                 race, income_ratio, educ_adult, pregnant,DMDMARTL)

# 2009-2010
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DEMO_F.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
demo_09 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

demo_09 <- demo_09 %>% mutate(cycle = '2009-2010')%>% rename(sex = RIAGENDR, age = RIDAGEYR, race = RIDRETH1, income_ratio = INDFMPIR,
                              educ_adult = DMDEDUC2,
                              pregnant = RIDEXPRG,
                              weight_demo = WTMEC2YR,
                              mid_year = RIDEXMON) %>% select(SEQN,SDMVPSU, SDMVSTRA, cycle,mid_year, weight_demo, sex, age,
                                                                 race, income_ratio, educ_adult, pregnant,DMDMARTL)

# 2011-2012
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
demo_11 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

demo_11 <- demo_11 %>% mutate(cycle = '2011-2012')%>% rename(sex = RIAGENDR, age = RIDAGEYR, race = RIDRETH1, income_ratio = INDFMPIR,
                              educ_adult = DMDEDUC2,
                              pregnant = RIDEXPRG,
                              weight_demo = WTMEC2YR,
                              mid_year = RIDEXMON) %>% select(SEQN,SDMVPSU, SDMVSTRA, cycle,mid_year,weight_demo, sex, age,
                                                                 race, income_ratio, educ_adult, pregnant,DMDMARTL)

# 2013-2014
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
demo_13 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

demo_13 <- demo_13 %>% mutate(cycle = '2013-2014')%>% rename(sex = RIAGENDR, age = RIDAGEYR, race = RIDRETH1, income_ratio = INDFMPIR,
                              educ_adult = DMDEDUC2,
                              pregnant = RIDEXPRG,
                              weight_demo = WTMEC2YR,
                              mid_year = RIDEXMON) %>% select(SEQN,SDMVPSU, SDMVSTRA, cycle,mid_year, weight_demo, sex, age,
                                                                 race, income_ratio, educ_adult, pregnant,DMDMARTL)
# 2015-2016
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
demo_15 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

demo_15 <- demo_15 %>% mutate(cycle = '2015-2016')%>% rename(sex = RIAGENDR, age = RIDAGEYR, race = RIDRETH1, income_ratio = INDFMPIR,
                              educ_adult = DMDEDUC2,
                              pregnant = RIDEXPRG,
                              weight_demo = WTMEC2YR,
                              mid_year = RIDEXMON) %>% select(SEQN,SDMVPSU, SDMVSTRA, cycle,mid_year,weight_demo, sex, age,
                                                                 race, income_ratio, educ_adult, pregnant,DMDMARTL)


# merge all datasets #
demo <- rbind(demo_05, demo_07,demo_09,demo_11,demo_13,demo_15)
demo <- demo %>% mutate(marital = case_when(DMDMARTL %in% c(1,6) ~ 1,
                                                DMDMARTL %in% c(2,3,4) ~ 2,
                                                DMDMARTL == 5 ~ 3,
                                                TRUE ~ NA_integer_)) %>% select(-DMDMARTL)

# sex: 1 = male 2= female
# age: in years at examination
# race: 1 Mexican American 2 other hispanic 3 Non-hispanic White 4 non-hispanic black 5 other race including multirace
# income_ratio: a ratio of family income to poverty threshold 
# educ_adult: 1	Less Than 9th Grade; 2	9-11th Grade (Includes 12th grade with no diploma); 3	High School Grad/GED or Equivalent; 
#             4	Some College or AA degree; 5	College Graduate or above;	7	Refused; 9	Don't Know
# pregnant: 1 positive preg urine test or self-reported pregnant; 2 not pregnant 3 cannot ascertain
# mid_year: 1 November 1 through April 30; 2 May 1 through October 31	
# marital: 1 - married/living with partner; 2 - widowed/divorcd/separated; 3 - never married

#### BMI ####
# 2005-2006
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BMX_D.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
bmi_05 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

bmi_05 <- bmi_05 %>% rename(bmi = BMXBMI) %>% select(SEQN, bmi)

# 2007-2008
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BMX_E.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
bmi_07 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

bmi_07 <- bmi_07 %>% rename(bmi = BMXBMI) %>% select(SEQN, bmi)

# 2009-2010
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BMX_F.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
bmi_09 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

bmi_09 <- bmi_09 %>% rename(bmi = BMXBMI) %>% select(SEQN, bmi)

# 2011-2012
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BMX_G.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
bmi_11 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

bmi_11 <- bmi_11 %>% rename(bmi = BMXBMI) %>% select(SEQN, bmi)

# 2013-2014
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BMX_H.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
bmi_13 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

bmi_13 <- bmi_13 %>% rename(bmi = BMXBMI) %>% select(SEQN, bmi)

# 2015-2016
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
bmi_15 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

bmi_15 <- bmi_15 %>% rename(bmi = BMXBMI) %>% select(SEQN, bmi)

bmi <- rbind(bmi_05, bmi_07, bmi_09, bmi_11, bmi_13, bmi_15)

#### serum cotinine, ng/mL ####
# 2005-2006
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/COT_D.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
cotinine_05 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

cotinine_05 <- cotinine_05 %>% rename (cotinine = LBXCOT) %>% select (SEQN, cotinine)

# 2007-2008
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/COTNAL_E.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
cotinine_07 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

cotinine_07 <- cotinine_07 %>% rename (cotinine = LBXCOT) %>% select (SEQN, cotinine)

# 2009-2010
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/COTNAL_F.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
cotinine_09 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

cotinine_09 <- cotinine_09 %>% rename (cotinine = LBXCOT) %>% select (SEQN, cotinine)

# 2011-2012
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/COTNAL_G.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
cotinine_11 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

cotinine_11 <- cotinine_11 %>% rename (cotinine = LBXCOT) %>% select (SEQN, cotinine)

# 2013-2014
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/COT_H.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
cotinine_13 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

cotinine_13 <- cotinine_13 %>% rename (cotinine = LBXCOT) %>% select (SEQN, cotinine)

# 2015-2016
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/COT_I.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
cotinine_15 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

cotinine_15 <- cotinine_15 %>% rename (cotinine = LBXCOT) %>% select (SEQN, cotinine)


cotinine <- rbind(cotinine_05, cotinine_07, cotinine_09, cotinine_11,
                  cotinine_13, cotinine_15)

# merging all covariates data
data <- merge(demo, bmi, by = 'SEQN', all = TRUE)
covariate_nodiet <- merge(data, cotinine, by = 'SEQN', all = TRUE)

#### diet ####
##### Manually creating AHEI-2010 variable in NHANES 2005-2016
# AHEI 2010 ref:
# Chiuve, S. E., Fung, T. T., Rimm, E. B., Hu, F. B., McCullough, M. L., Wang, M., Stampfer, M. J., & Willett, W. C. (2012). Alternative 
# Dietary Indices Both Strongly Predict Risk of Chronic Disease. The Journal of Nutrition, 142(6), 1009–1018. https://doi.org/10.3945/jn.111.157222

# alternative healthy eating index evaluates diet quality by 11 components. each component has the score between 0 and 10. Highest score of AHEI is 110.
# components:
# 1. vegetables, min 0 serving/day (0 score), max >= 5 servings/day (10 score)
# 2. fruit, min 0 serving, max >= 4 servings (10 score)
# 3. whole grains, Women min 0 g/day, max 75 g/day
#                   Men min 0 g/day, max 90 g/day
# 4. sugar-sweetened beverages and fruit juice, min >= 1 (0 score), max 0 (10 score)
# 5. nuts and legumes, min 0 serving/day, max >= 1 serving/day
# 6. red/processed meat, min >=1/5 servings/d, max 0 serving/day
# 7. trans fat, min >=4% of total energy intake, max <= 0.5% energy
# 8. long-chain (n-3) fats (EPA + DHA), min 0 mg/day, max 250 mg/day
# 9. PUFA, min <= 2% of total energy intake, max >= 10% of total energy intake
# 10. sodium, min highest decile, max, lowest decile
# 11. alcohol, women min >=2.5 drinks/d, max 0.5-1.5 drinks/d
#               men min >=3.5 drinks/d, max 0.5-2 drinks/d


## the pyr or FPED data are from : https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fped-databases/
## these are estimated food equivalents intake

setwd('directory of pyr or FPED data')
library(tidyverse)
library(haven)
#### 2005-2006 
# sex: RIAGENDR
# vege: DR2T_V_TOTAL
# fruit: DR2T_F_TOTAL
# whole grain: DR1T_G_WHOLE
# sugar-sweetened beverages and fruit juice:  ADDED_SUGAR_SSB_SERV
# nuts and legumes: DR1T_V_LEGUMES, DR1T_PF_NUTSDS, DR1T_PF_SOY
# red/processed meat: DR1T_PF_CUREDMEAT, DR1T_PF_MEAT, DR1T_PF_ORGAN
# alcohol: DR1T_A_DRINKS
# trans fat -- TOT 
# EPA, DHA -- TOT (DR1IP205 + DR1IP226)*1000,
# PUFA: TOT DR1IP182 + DR1IP183 + DR1IP184 + DR1IP204 + DR1IP225
# sodium: TOT DR1TSODI
fp0506_d1 <- read_sas("fped_dr1tot_0506.sas7bdat")
fp0506_d2 <- read_sas("fped_dr2tot_0506.sas7bdat")
fp0506 <- merge(fp0506_d1,fp0506_d2, by=c("SEQN", "RIAGENDR"), all = TRUE)
fp0506 <- fp0506 %>%  mutate(V_TOTAL = rowMeans(across(c(DR1T_V_TOTAL, DR2T_V_TOTAL)), na.rm=TRUE),
                             F_TOTAL = rowMeans(across(c(DR1T_F_TOTAL, DR2T_F_TOTAL)), na.rm=TRUE),
                             G_WHL = rowMeans(across(c(DR1T_G_WHOLE, DR2T_G_WHOLE)), na.rm=TRUE),
                             ADD_SUG = rowMeans(across(c(DR1T_ADD_SUGARS, DR2T_ADD_SUGARS)), na.rm=TRUE),
                             M_SOY = rowMeans(across(c(DR1T_PF_SOY, DR2T_PF_SOY)), na.rm=TRUE),
                             M_NUTSD = rowMeans(across(c(DR1T_PF_NUTSDS, DR2T_PF_NUTSDS)), na.rm=TRUE),
                             LEGUMES = rowMeans(across(c(DR1T_V_LEGUMES, DR2T_V_LEGUMES)), na.rm=TRUE),
                             M_FRANK = rowMeans(across(c(DR1T_PF_CUREDMEAT, DR2T_PF_CUREDMEAT)), na.rm=TRUE),
                             M_ORGAN = rowMeans(across(c(DR1T_PF_ORGAN, DR2T_PF_ORGAN)), na.rm=TRUE),
                             M_MEAT = rowMeans(across(c(DR1T_PF_MEAT, DR2T_PF_MEAT)), na.rm=TRUE),
                             A_BEV = rowMeans(across(c(DR1T_A_DRINKS, DR2T_A_DRINKS)), na.rm=TRUE)) %>%  select(SEQN, RIAGENDR,V_TOTAL,F_TOTAL,G_WHL,ADD_SUG,M_SOY,M_NUTSD,LEGUMES, M_FRANK, M_ORGAN, M_MEAT,A_BEV)

dr0506_d1 <- read_xpt("DR1TOT_D.XPT")
dr0506_d2 <- read_xpt("DR2TOT_D.XPT")
dr0506_d1 <- dr0506_d1 %>% select(SEQN, DR1TKCAL,DR1TTFAT,DR1TSFAT,DR1TMFAT,DR1TPFAT,DR1TP205,DR1TP226, DR1TP182, DR1TP183,DR1TP184,DR1TP204,DR1TP225,DR1TSODI)
dr0506_d2 <- dr0506_d2 %>% select(SEQN, DR2TKCAL,DR2TTFAT,DR2TSFAT,DR2TMFAT,DR2TPFAT,DR2TP205,DR2TP226, DR2TP182, DR2TP183,DR2TP184,DR2TP204,DR2TP225,DR2TSODI)

dr0506 <- merge(dr0506_d1,dr0506_d2, by='SEQN', all=TRUE)
dr0506 <- dr0506 %>% mutate(energy = rowMeans(across(c(DR1TKCAL,DR2TKCAL)), na.rm=TRUE),
                            tfat = rowMeans(across(c(DR1TTFAT,DR2TTFAT)), na.rm=TRUE),
                            sfat = rowMeans(across(c(DR1TSFAT,DR2TSFAT)), na.rm=TRUE),
                            mfat = rowMeans(across(c(DR1TMFAT,DR2TMFAT)), na.rm=TRUE),
                            pfat = rowMeans(across(c(DR1TPFAT,DR2TPFAT)), na.rm=TRUE),
                            p205 = rowMeans(across(c(DR1TP205,DR2TP205)), na.rm=TRUE),
                            p226 = rowMeans(across(c(DR1TP226,DR2TP226)), na.rm=TRUE),
                            p182 = rowMeans(across(c(DR1TP182,DR2TP182)), na.rm=TRUE),
                            p183 = rowMeans(across(c(DR1TP183,DR2TP183)), na.rm=TRUE),
                            p184 = rowMeans(across(c(DR1TP184,DR2TP184)), na.rm=TRUE),
                            p204 = rowMeans(across(c(DR1TP204,DR2TP204)), na.rm=TRUE),
                            p225 = rowMeans(across(c(DR1TP225,DR2TP225)), na.rm=TRUE),
                            sodium = rowMeans(across(c(DR1TSODI,DR2TSODI)), na.rm=TRUE)) %>% select(SEQN, energy, tfat, sfat, mfat, pfat, p205,p226,p182,p183,p184,p204,p225,sodium)
AHEI0506 <- merge(fp0506, dr0506, by="SEQN")

#### 2007-2008 
fp0708_d1 <- read_sas("fped_dr1tot_0708.sas7bdat")
fp0708_d2 <- read_sas("fped_dr2tot_0708.sas7bdat")
fp0708 <- merge(fp0708_d1,fp0708_d2, by=c("SEQN", "RIAGENDR"), all = TRUE)
fp0708 <- fp0708 %>%  mutate(V_TOTAL = rowMeans(across(c(DR1T_V_TOTAL, DR2T_V_TOTAL)), na.rm=TRUE),
                             F_TOTAL = rowMeans(across(c(DR1T_F_TOTAL, DR2T_F_TOTAL)), na.rm=TRUE),
                             G_WHL = rowMeans(across(c(DR1T_G_WHOLE, DR2T_G_WHOLE)), na.rm=TRUE),
                             ADD_SUG = rowMeans(across(c(DR1T_ADD_SUGARS, DR2T_ADD_SUGARS)), na.rm=TRUE),
                             M_SOY = rowMeans(across(c(DR1T_PF_SOY, DR2T_PF_SOY)), na.rm=TRUE),
                             M_NUTSD = rowMeans(across(c(DR1T_PF_NUTSDS, DR2T_PF_NUTSDS)), na.rm=TRUE),
                             LEGUMES = rowMeans(across(c(DR1T_V_LEGUMES, DR2T_V_LEGUMES)), na.rm=TRUE),
                             M_FRANK = rowMeans(across(c(DR1T_PF_CUREDMEAT, DR2T_PF_CUREDMEAT)), na.rm=TRUE),
                             M_ORGAN = rowMeans(across(c(DR1T_PF_ORGAN, DR2T_PF_ORGAN)), na.rm=TRUE),
                             M_MEAT = rowMeans(across(c(DR1T_PF_MEAT, DR2T_PF_MEAT)), na.rm=TRUE),
                             A_BEV = rowMeans(across(c(DR1T_A_DRINKS, DR2T_A_DRINKS)), na.rm=TRUE)) %>%  select(SEQN, RIAGENDR,V_TOTAL,F_TOTAL,G_WHL,ADD_SUG,M_SOY,M_NUTSD,LEGUMES, M_FRANK, M_ORGAN, M_MEAT,A_BEV)
summary(fp0708)

dr0708_d1 <- read_xpt("DR1TOT_E.XPT")
dr0708_d2 <- read_xpt("DR2TOT_E.XPT")
dr0708_d1 <- dr0708_d1 %>% select(SEQN, DR1TKCAL,DR1TTFAT,DR1TSFAT,DR1TMFAT,DR1TPFAT,DR1TP205,DR1TP226, DR1TP182, DR1TP183,DR1TP184,DR1TP204,DR1TP225,DR1TSODI)
dr0708_d2 <- dr0708_d2 %>% select(SEQN, DR2TKCAL,DR2TTFAT,DR2TSFAT,DR2TMFAT,DR2TPFAT,DR2TP205,DR2TP226, DR2TP182, DR2TP183,DR2TP184,DR2TP204,DR2TP225,DR2TSODI)

dr0708 <- merge(dr0708_d1,dr0708_d2, by='SEQN', all=TRUE)
dr0708 <- dr0708 %>% mutate(energy = rowMeans(across(c(DR1TKCAL,DR2TKCAL)), na.rm=TRUE),
                            tfat = rowMeans(across(c(DR1TTFAT,DR2TTFAT)), na.rm=TRUE),
                            sfat = rowMeans(across(c(DR1TSFAT,DR2TSFAT)), na.rm=TRUE),
                            mfat = rowMeans(across(c(DR1TMFAT,DR2TMFAT)), na.rm=TRUE),
                            pfat = rowMeans(across(c(DR1TPFAT,DR2TPFAT)), na.rm=TRUE),
                            p205 = rowMeans(across(c(DR1TP205,DR2TP205)), na.rm=TRUE),
                            p226 = rowMeans(across(c(DR1TP226,DR2TP226)), na.rm=TRUE),
                            p182 = rowMeans(across(c(DR1TP182,DR2TP182)), na.rm=TRUE),
                            p183 = rowMeans(across(c(DR1TP183,DR2TP183)), na.rm=TRUE),
                            p184 = rowMeans(across(c(DR1TP184,DR2TP184)), na.rm=TRUE),
                            p204 = rowMeans(across(c(DR1TP204,DR2TP204)), na.rm=TRUE),
                            p225 = rowMeans(across(c(DR1TP225,DR2TP225)), na.rm=TRUE),
                            sodium = rowMeans(across(c(DR1TSODI,DR2TSODI)), na.rm=TRUE)) %>% select(SEQN, energy, tfat, sfat, mfat, pfat, p205,p226,p182,p183,p184,p204,p225,sodium)
AHEI0708 <- merge(fp0708, dr0708, by="SEQN")

#### 2009-2010 
fp0910_d1 <- read_sas("fped_dr1tot_0910.sas7bdat")
fp0910_d2 <- read_sas("fped_dr2tot_0910.sas7bdat")
fp0910 <- merge(fp0910_d1,fp0910_d2, by=c("SEQN", "RIAGENDR"), all = TRUE)
fp0910 <- fp0910 %>%  mutate(V_TOTAL = rowMeans(across(c(DR1T_V_TOTAL, DR2T_V_TOTAL)), na.rm=TRUE),
                             F_TOTAL = rowMeans(across(c(DR1T_F_TOTAL, DR2T_F_TOTAL)), na.rm=TRUE),
                             G_WHL = rowMeans(across(c(DR1T_G_WHOLE, DR2T_G_WHOLE)), na.rm=TRUE),
                             ADD_SUG = rowMeans(across(c(DR1T_ADD_SUGARS, DR2T_ADD_SUGARS)), na.rm=TRUE),
                             M_SOY = rowMeans(across(c(DR1T_PF_SOY, DR2T_PF_SOY)), na.rm=TRUE),
                             M_NUTSD = rowMeans(across(c(DR1T_PF_NUTSDS, DR2T_PF_NUTSDS)), na.rm=TRUE),
                             LEGUMES = rowMeans(across(c(DR1T_V_LEGUMES, DR2T_V_LEGUMES)), na.rm=TRUE),
                             M_FRANK = rowMeans(across(c(DR1T_PF_CUREDMEAT, DR2T_PF_CUREDMEAT)), na.rm=TRUE),
                             M_ORGAN = rowMeans(across(c(DR1T_PF_ORGAN, DR2T_PF_ORGAN)), na.rm=TRUE),
                             M_MEAT = rowMeans(across(c(DR1T_PF_MEAT, DR2T_PF_MEAT)), na.rm=TRUE),
                             A_BEV = rowMeans(across(c(DR1T_A_DRINKS, DR2T_A_DRINKS)), na.rm=TRUE)) %>%  select(SEQN, RIAGENDR,V_TOTAL,F_TOTAL,G_WHL,ADD_SUG,M_SOY,M_NUTSD,LEGUMES, M_FRANK, M_ORGAN, M_MEAT,A_BEV)

dr0910_d1 <- read_xpt("DR1TOT_F.XPT")
dr0910_d2 <- read_xpt("DR2TOT_F.XPT")
dr0910_d1 <- dr0910_d1 %>% select(SEQN, DR1TKCAL,DR1TTFAT,DR1TSFAT,DR1TMFAT,DR1TPFAT,DR1TP205,DR1TP226, DR1TP182, DR1TP183,DR1TP184,DR1TP204,DR1TP225,DR1TSODI)
dr0910_d2 <- dr0910_d2 %>% select(SEQN, DR2TKCAL,DR2TTFAT,DR2TSFAT,DR2TMFAT,DR2TPFAT,DR2TP205,DR2TP226, DR2TP182, DR2TP183,DR2TP184,DR2TP204,DR2TP225,DR2TSODI)

dr0910 <- merge(dr0910_d1,dr0910_d2, by='SEQN', all=TRUE)
dr0910 <- dr0910 %>% mutate(energy = rowMeans(across(c(DR1TKCAL,DR2TKCAL)), na.rm=TRUE),
                            tfat = rowMeans(across(c(DR1TTFAT,DR2TTFAT)), na.rm=TRUE),
                            sfat = rowMeans(across(c(DR1TSFAT,DR2TSFAT)), na.rm=TRUE),
                            mfat = rowMeans(across(c(DR1TMFAT,DR2TMFAT)), na.rm=TRUE),
                            pfat = rowMeans(across(c(DR1TPFAT,DR2TPFAT)), na.rm=TRUE),
                            p205 = rowMeans(across(c(DR1TP205,DR2TP205)), na.rm=TRUE),
                            p226 = rowMeans(across(c(DR1TP226,DR2TP226)), na.rm=TRUE),
                            p182 = rowMeans(across(c(DR1TP182,DR2TP182)), na.rm=TRUE),
                            p183 = rowMeans(across(c(DR1TP183,DR2TP183)), na.rm=TRUE),
                            p184 = rowMeans(across(c(DR1TP184,DR2TP184)), na.rm=TRUE),
                            p204 = rowMeans(across(c(DR1TP204,DR2TP204)), na.rm=TRUE),
                            p225 = rowMeans(across(c(DR1TP225,DR2TP225)), na.rm=TRUE),
                            sodium = rowMeans(across(c(DR1TSODI,DR2TSODI)), na.rm=TRUE)) %>% select(SEQN, energy, tfat, sfat, mfat, pfat, p205,p226,p182,p183,p184,p204,p225,sodium)
AHEI0910 <- merge(fp0910, dr0910, by="SEQN")

#### 2011-2012
fp1112_d1 <- read_sas("fped_dr1tot_1112.sas7bdat")
fp1112_d2 <- read_sas("fped_dr2tot_1112.sas7bdat")
fp1112 <- merge(fp1112_d1,fp1112_d2, by=c("SEQN", "RIAGENDR"), all = TRUE)
fp1112 <- fp1112 %>%  mutate(V_TOTAL = rowMeans(across(c(DR1T_V_TOTAL, DR2T_V_TOTAL)), na.rm=TRUE),
                             F_TOTAL = rowMeans(across(c(DR1T_F_TOTAL, DR2T_F_TOTAL)), na.rm=TRUE),
                             G_WHL = rowMeans(across(c(DR1T_G_WHOLE, DR2T_G_WHOLE)), na.rm=TRUE),
                             ADD_SUG = rowMeans(across(c(DR1T_ADD_SUGARS, DR2T_ADD_SUGARS)), na.rm=TRUE),
                             M_SOY = rowMeans(across(c(DR1T_PF_SOY, DR2T_PF_SOY)), na.rm=TRUE),
                             M_NUTSD = rowMeans(across(c(DR1T_PF_NUTSDS, DR2T_PF_NUTSDS)), na.rm=TRUE),
                             LEGUMES = rowMeans(across(c(DR1T_V_LEGUMES, DR2T_V_LEGUMES)), na.rm=TRUE),
                             M_FRANK = rowMeans(across(c(DR1T_PF_CUREDMEAT, DR2T_PF_CUREDMEAT)), na.rm=TRUE),
                             M_ORGAN = rowMeans(across(c(DR1T_PF_ORGAN, DR2T_PF_ORGAN)), na.rm=TRUE),
                             M_MEAT = rowMeans(across(c(DR1T_PF_MEAT, DR2T_PF_MEAT)), na.rm=TRUE),
                             A_BEV = rowMeans(across(c(DR1T_A_DRINKS, DR2T_A_DRINKS)), na.rm=TRUE)) %>%  select(SEQN, RIAGENDR,V_TOTAL,F_TOTAL,G_WHL,ADD_SUG,M_SOY,M_NUTSD,LEGUMES, M_FRANK, M_ORGAN, M_MEAT,A_BEV)

dr1112_d1 <- read_xpt("DR1TOT_G.XPT")
dr1112_d2 <- read_xpt("DR2TOT_G.XPT")
dr1112_d1 <- dr1112_d1 %>% select(SEQN, DR1TKCAL,DR1TTFAT,DR1TSFAT,DR1TMFAT,DR1TPFAT,DR1TP205,DR1TP226, DR1TP182, DR1TP183,DR1TP184,DR1TP204,DR1TP225,DR1TSODI)
dr1112_d2 <- dr1112_d2 %>% select(SEQN, DR2TKCAL,DR2TTFAT,DR2TSFAT,DR2TMFAT,DR2TPFAT,DR2TP205,DR2TP226, DR2TP182, DR2TP183,DR2TP184,DR2TP204,DR2TP225,DR2TSODI)

dr1112 <- merge(dr1112_d1,dr1112_d2, by='SEQN', all=TRUE)
dr1112 <- dr1112 %>% mutate(energy = rowMeans(across(c(DR1TKCAL,DR2TKCAL)), na.rm=TRUE),
                            tfat = rowMeans(across(c(DR1TTFAT,DR2TTFAT)), na.rm=TRUE),
                            sfat = rowMeans(across(c(DR1TSFAT,DR2TSFAT)), na.rm=TRUE),
                            mfat = rowMeans(across(c(DR1TMFAT,DR2TMFAT)), na.rm=TRUE),
                            pfat = rowMeans(across(c(DR1TPFAT,DR2TPFAT)), na.rm=TRUE),
                            p205 = rowMeans(across(c(DR1TP205,DR2TP205)), na.rm=TRUE),
                            p226 = rowMeans(across(c(DR1TP226,DR2TP226)), na.rm=TRUE),
                            p182 = rowMeans(across(c(DR1TP182,DR2TP182)), na.rm=TRUE),
                            p183 = rowMeans(across(c(DR1TP183,DR2TP183)), na.rm=TRUE),
                            p184 = rowMeans(across(c(DR1TP184,DR2TP184)), na.rm=TRUE),
                            p204 = rowMeans(across(c(DR1TP204,DR2TP204)), na.rm=TRUE),
                            p225 = rowMeans(across(c(DR1TP225,DR2TP225)), na.rm=TRUE),
                            sodium = rowMeans(across(c(DR1TSODI,DR2TSODI)), na.rm=TRUE)) %>% select(SEQN, energy, tfat, sfat, mfat, pfat, p205,p226,p182,p183,p184,p204,p225,sodium)
AHEI1112 <- merge(fp1112, dr1112, by="SEQN")


#### 2013-2014 
fp1314_d1 <- read_sas("fped_dr1tot_1314.sas7bdat")
fp1314_d2 <- read_sas("fped_dr2tot_1314.sas7bdat")
fp1314 <- merge(fp1314_d1,fp1314_d2, by=c("SEQN", "RIAGENDR"), all = TRUE)
fp1314 <- fp1314 %>%  mutate(V_TOTAL = rowMeans(across(c(DR1T_V_TOTAL, DR2T_V_TOTAL)), na.rm=TRUE),
                             F_TOTAL = rowMeans(across(c(DR1T_F_TOTAL, DR2T_F_TOTAL)), na.rm=TRUE),
                             G_WHL = rowMeans(across(c(DR1T_G_WHOLE, DR2T_G_WHOLE)), na.rm=TRUE),
                             ADD_SUG = rowMeans(across(c(DR1T_ADD_SUGARS, DR2T_ADD_SUGARS)), na.rm=TRUE),
                             M_SOY = rowMeans(across(c(DR1T_PF_SOY, DR2T_PF_SOY)), na.rm=TRUE),
                             M_NUTSD = rowMeans(across(c(DR1T_PF_NUTSDS, DR2T_PF_NUTSDS)), na.rm=TRUE),
                             LEGUMES = rowMeans(across(c(DR1T_V_LEGUMES, DR2T_V_LEGUMES)), na.rm=TRUE),
                             M_FRANK = rowMeans(across(c(DR1T_PF_CUREDMEAT, DR2T_PF_CUREDMEAT)), na.rm=TRUE),
                             M_ORGAN = rowMeans(across(c(DR1T_PF_ORGAN, DR2T_PF_ORGAN)), na.rm=TRUE),
                             M_MEAT = rowMeans(across(c(DR1T_PF_MEAT, DR2T_PF_MEAT)), na.rm=TRUE),
                             A_BEV = rowMeans(across(c(DR1T_A_DRINKS, DR2T_A_DRINKS)), na.rm=TRUE)) %>%  select(SEQN, RIAGENDR,V_TOTAL,F_TOTAL,G_WHL,ADD_SUG,M_SOY,M_NUTSD,LEGUMES, M_FRANK, M_ORGAN, M_MEAT,A_BEV)

dr1314_d1 <- read_xpt("DR1TOT_H.XPT")
dr1314_d2 <- read_xpt("DR2TOT_H.XPT")
dr1314_d1 <- dr1314_d1 %>% select(SEQN, DR1TKCAL,DR1TTFAT,DR1TSFAT,DR1TMFAT,DR1TPFAT,DR1TP205,DR1TP226, DR1TP182, DR1TP183,DR1TP184,DR1TP204,DR1TP225,DR1TSODI)
dr1314_d2 <- dr1314_d2 %>% select(SEQN, DR2TKCAL,DR2TTFAT,DR2TSFAT,DR2TMFAT,DR2TPFAT,DR2TP205,DR2TP226, DR2TP182, DR2TP183,DR2TP184,DR2TP204,DR2TP225,DR2TSODI)

dr1314 <- merge(dr1314_d1,dr1314_d2, by='SEQN', all=TRUE)
dr1314 <- dr1314 %>% mutate(energy = rowMeans(across(c(DR1TKCAL,DR2TKCAL)), na.rm=TRUE),
                            tfat = rowMeans(across(c(DR1TTFAT,DR2TTFAT)), na.rm=TRUE),
                            sfat = rowMeans(across(c(DR1TSFAT,DR2TSFAT)), na.rm=TRUE),
                            mfat = rowMeans(across(c(DR1TMFAT,DR2TMFAT)), na.rm=TRUE),
                            pfat = rowMeans(across(c(DR1TPFAT,DR2TPFAT)), na.rm=TRUE),
                            p205 = rowMeans(across(c(DR1TP205,DR2TP205)), na.rm=TRUE),
                            p226 = rowMeans(across(c(DR1TP226,DR2TP226)), na.rm=TRUE),
                            p182 = rowMeans(across(c(DR1TP182,DR2TP182)), na.rm=TRUE),
                            p183 = rowMeans(across(c(DR1TP183,DR2TP183)), na.rm=TRUE),
                            p184 = rowMeans(across(c(DR1TP184,DR2TP184)), na.rm=TRUE),
                            p204 = rowMeans(across(c(DR1TP204,DR2TP204)), na.rm=TRUE),
                            p225 = rowMeans(across(c(DR1TP225,DR2TP225)), na.rm=TRUE),
                            sodium = rowMeans(across(c(DR1TSODI,DR2TSODI)), na.rm=TRUE)) %>% select(SEQN, energy, tfat, sfat, mfat, pfat, p205,p226,p182,p183,p184,p204,p225,sodium)
AHEI1314 <- merge(fp1314, dr1314, by="SEQN")

#### 2015-2016 
fp1516_d1 <- read_sas("fped_dr1tot_1516.sas7bdat")
fp1516_d2 <- read_sas("fped_dr2tot_1516.sas7bdat")
fp1516 <- merge(fp1516_d1,fp1516_d2, by=c("SEQN", "RIAGENDR"), all = TRUE)
fp1516 <- fp1516 %>%  mutate(V_TOTAL = rowMeans(across(c(DR1T_V_TOTAL, DR2T_V_TOTAL)), na.rm=TRUE),
                             F_TOTAL = rowMeans(across(c(DR1T_F_TOTAL, DR2T_F_TOTAL)), na.rm=TRUE),
                             G_WHL = rowMeans(across(c(DR1T_G_WHOLE, DR2T_G_WHOLE)), na.rm=TRUE),
                             ADD_SUG = rowMeans(across(c(DR1T_ADD_SUGARS, DR2T_ADD_SUGARS)), na.rm=TRUE),
                             M_SOY = rowMeans(across(c(DR1T_PF_SOY, DR2T_PF_SOY)), na.rm=TRUE),
                             M_NUTSD = rowMeans(across(c(DR1T_PF_NUTSDS, DR2T_PF_NUTSDS)), na.rm=TRUE),
                             LEGUMES = rowMeans(across(c(DR1T_V_LEGUMES, DR2T_V_LEGUMES)), na.rm=TRUE),
                             M_FRANK = rowMeans(across(c(DR1T_PF_CUREDMEAT, DR2T_PF_CUREDMEAT)), na.rm=TRUE),
                             M_ORGAN = rowMeans(across(c(DR1T_PF_ORGAN, DR2T_PF_ORGAN)), na.rm=TRUE),
                             M_MEAT = rowMeans(across(c(DR1T_PF_MEAT, DR2T_PF_MEAT)), na.rm=TRUE),
                             A_BEV = rowMeans(across(c(DR1T_A_DRINKS, DR2T_A_DRINKS)), na.rm=TRUE)) %>%  select(SEQN, RIAGENDR,V_TOTAL,F_TOTAL,G_WHL,ADD_SUG,M_SOY,M_NUTSD,LEGUMES, M_FRANK, M_ORGAN, M_MEAT,A_BEV)

dr1516_d1 <- read_xpt("DR1TOT_I.XPT")
dr1516_d2 <- read_xpt("DR2TOT_I.XPT")
dr1516_d1 <- dr1516_d1 %>% select(SEQN, DR1TKCAL,DR1TTFAT,DR1TSFAT,DR1TMFAT,DR1TPFAT,DR1TP205,DR1TP226, DR1TP182, DR1TP183,DR1TP184,DR1TP204,DR1TP225,DR1TSODI)
dr1516_d2 <- dr1516_d2 %>% select(SEQN, DR2TKCAL,DR2TTFAT,DR2TSFAT,DR2TMFAT,DR2TPFAT,DR2TP205,DR2TP226, DR2TP182, DR2TP183,DR2TP184,DR2TP204,DR2TP225,DR2TSODI)

dr1516 <- merge(dr1516_d1,dr1516_d2, by='SEQN', all=TRUE)
dr1516 <- dr1516 %>% mutate(energy = rowMeans(across(c(DR1TKCAL,DR2TKCAL)), na.rm=TRUE),
                            tfat = rowMeans(across(c(DR1TTFAT,DR2TTFAT)), na.rm=TRUE),
                            sfat = rowMeans(across(c(DR1TSFAT,DR2TSFAT)), na.rm=TRUE),
                            mfat = rowMeans(across(c(DR1TMFAT,DR2TMFAT)), na.rm=TRUE),
                            pfat = rowMeans(across(c(DR1TPFAT,DR2TPFAT)), na.rm=TRUE),
                            p205 = rowMeans(across(c(DR1TP205,DR2TP205)), na.rm=TRUE),
                            p226 = rowMeans(across(c(DR1TP226,DR2TP226)), na.rm=TRUE),
                            p182 = rowMeans(across(c(DR1TP182,DR2TP182)), na.rm=TRUE),
                            p183 = rowMeans(across(c(DR1TP183,DR2TP183)), na.rm=TRUE),
                            p184 = rowMeans(across(c(DR1TP184,DR2TP184)), na.rm=TRUE),
                            p204 = rowMeans(across(c(DR1TP204,DR2TP204)), na.rm=TRUE),
                            p225 = rowMeans(across(c(DR1TP225,DR2TP225)), na.rm=TRUE),
                            sodium = rowMeans(across(c(DR1TSODI,DR2TSODI)), na.rm=TRUE)) %>% select(SEQN, energy, tfat, sfat, mfat, pfat, p205,p226,p182,p183,p184,p204,p225,sodium)
AHEI1516 <- merge(fp1516, dr1516, by="SEQN")


AHEI <- rbind( AHEI0506,AHEI0708,AHEI0910,AHEI1112,AHEI1314,AHEI1516)

#### Calculating AHEI score 
# next step: calculating the scores of each components
# ref: https://github.com/jamesjiadazhan/dietaryindex/blob/main/R/AHEI_NHANES_FPED.R

AHEI$G_WHL_g <- AHEI$G_WHL/0.035274
AHEI$trans_fat <- AHEI$tfat - AHEI$sfat - AHEI$mfat - AHEI$pfat
AHEI$trans_fat_energy <- AHEI$trans_fat*9
AHEI$PUFA_energy <- (AHEI$p182 + AHEI$p183 + AHEI$p184 + AHEI$p204 + AHEI$p225)*9
AHEI$N3FAT_mg <- (AHEI$p205 + AHEI$p226)*1000
# # convert to energy!!
AHEI$nuts <- AHEI$M_SOY + AHEI$M_NUTSD + AHEI$LEGUMES
AHEI$pro_red_meat <- AHEI$M_FRANK + AHEI$M_MEAT + AHEI$M_ORGAN


# creating reference values
AHEI_MIN = 0
AHEI_MAX = 10
AHEI_MIN_VEG_SERV = 0
AHEI_MAX_VEG_SERV = 5
AHEI_MIN_FRT_SERV = 0
AHEI_MAX_FRT_SERV = 4
AHEI_MIN_WGRAIN_F_SERV = 0
AHEI_MAX_WGRAIN_F_SERV = 75
AHEI_MIN_WGRAIN_M_SERV = 0
AHEI_MAX_WGRAIN_M_SERV = 90
AHEI_MIN_NUTSLEG_SERV = 0
AHEI_MAX_NUTSLEG_SERV = 1
AHEI_MIN_N3FAT_SERV = 0
AHEI_MAX_N3FAT_SERV = 250
AHEI_MIN_PUFA_SERV = 2
AHEI_MAX_PUFA_SERV = 10
AHEI_MIN_SSB_FRTJ_SERV = 1
AHEI_MAX_SSB_FRTJ_SERV = 0
AHEI_MIN_REDPROC_MEAT_SERV = 1.5
AHEI_MAX_REDPROC_MEAT_SERV = 0
AHEI_MIN_TRANS_SERV = 4
AHEI_MAX_TRANS_SERV = 0.5



SCORE_HEALTHY <- function(actual_serv, min_serv, max_serv, min_score, max_score){
  case_when(
    actual_serv >= max_serv ~ max_score,
    actual_serv <= min_serv ~ min_score,
    TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
  )
}

SCORE_UNHEALTHY <- function(actual_serv, min_serv, max_serv, min_score, max_score){
  case_when(
    actual_serv >= min_serv ~ min_score ,
    actual_serv <= max_serv ~ max_score,
    TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
  )
}

SODIUM_DECILE = quantile(AHEI$sodium, probs=seq(0, 1, by=1/11), na.rm=TRUE)


AHEI_score<- AHEI %>%
  mutate(
    AHEI_VEG = SCORE_HEALTHY(V_TOTAL, AHEI_MIN_VEG_SERV, AHEI_MAX_VEG_SERV, AHEI_MIN, AHEI_MAX),
    AHEI_FRT = SCORE_HEALTHY(F_TOTAL, AHEI_MIN_FRT_SERV, AHEI_MAX_FRT_SERV, AHEI_MIN, AHEI_MAX),
    AHEI_WGRAIN = case_when(
      # 2 is female and 1 is male
      RIAGENDR == 2 ~ SCORE_HEALTHY(G_WHL_g, AHEI_MIN_WGRAIN_F_SERV, AHEI_MAX_WGRAIN_F_SERV, AHEI_MIN, AHEI_MAX),
      RIAGENDR == 1 ~ SCORE_HEALTHY(G_WHL_g, AHEI_MIN_WGRAIN_M_SERV, AHEI_MAX_WGRAIN_M_SERV, AHEI_MIN, AHEI_MAX)
    ),
    AHEI_NUTSLEG = SCORE_HEALTHY(nuts, AHEI_MIN_NUTSLEG_SERV, AHEI_MAX_NUTSLEG_SERV, AHEI_MIN, AHEI_MAX),
    AHEI_N3FAT = SCORE_HEALTHY(N3FAT_mg, AHEI_MIN_N3FAT_SERV, AHEI_MAX_N3FAT_SERV, AHEI_MIN, AHEI_MAX),
    AHEI_PUFA = SCORE_HEALTHY(PUFA_energy, AHEI_MIN_PUFA_SERV, AHEI_MAX_PUFA_SERV, AHEI_MIN, AHEI_MAX),
    
    AHEI_SSB_FRTJ = SCORE_UNHEALTHY(ADD_SUG, AHEI_MIN_SSB_FRTJ_SERV, AHEI_MAX_SSB_FRTJ_SERV, AHEI_MIN, AHEI_MAX),
    AHEI_REDPROC_MEAT = SCORE_UNHEALTHY(pro_red_meat, AHEI_MIN_REDPROC_MEAT_SERV, AHEI_MAX_REDPROC_MEAT_SERV, AHEI_MIN, AHEI_MAX),
    AHEI_TRANS = SCORE_UNHEALTHY(trans_fat_energy, AHEI_MIN_TRANS_SERV, AHEI_MAX_TRANS_SERV, AHEI_MIN, AHEI_MAX),
    
    AHEI_SODIUM = case_when(
      sodium <= SODIUM_DECILE[12] & sodium >= SODIUM_DECILE[11] ~ 0,
      sodium <= SODIUM_DECILE[11] & sodium >= SODIUM_DECILE[10] ~ 1,
      sodium < SODIUM_DECILE[10] & sodium >= SODIUM_DECILE[9] ~ 2,
      sodium < SODIUM_DECILE[9] & sodium >= SODIUM_DECILE[8] ~ 3,
      sodium < SODIUM_DECILE[8] & sodium >= SODIUM_DECILE[7] ~ 4,
      sodium < SODIUM_DECILE[7] & sodium >= SODIUM_DECILE[6] ~ 5,
      sodium < SODIUM_DECILE[6] & sodium >= SODIUM_DECILE[5] ~ 6,
      sodium < SODIUM_DECILE[5] & sodium >= SODIUM_DECILE[4] ~ 7,
      sodium < SODIUM_DECILE[4] & sodium >= SODIUM_DECILE[3] ~ 8,
      sodium < SODIUM_DECILE[3] & sodium >= SODIUM_DECILE[2] ~ 9,
      sodium < SODIUM_DECILE[2] & sodium >= SODIUM_DECILE[1] ~ 10
    ),
    AHEI_ALCOHOL = 
      case_when(
        RIAGENDR == 2 & A_BEV >= 2.5 ~ 0,
        RIAGENDR == 2 & A_BEV < 2.5 & A_BEV > 1.5 ~ 0 + (A_BEV-2.5)*10/(1.5-2.5),
        RIAGENDR == 2 & A_BEV <= 1.5 & A_BEV >= 0.5 ~ 10,
        RIAGENDR == 2 & A_BEV < 0.5 ~  0 + (A_BEV-0)*10/(0.5-0),
        RIAGENDR == 2 & A_BEV <= 0.125 ~ 2.5,
        RIAGENDR == 1 & A_BEV >= 3.5 ~ 0,
        RIAGENDR == 1 & A_BEV < 3.5 & A_BEV > 2 ~ 0 + (A_BEV-2.5)*10/(1.5-2.5),
        RIAGENDR == 1 & A_BEV <= 2 & A_BEV >= 0.5 ~ 10,
        RIAGENDR == 1 & A_BEV < 0.5 ~ (A_BEV-0)*10/(0.5-0),
        RIAGENDR == 1 & A_BEV <= 0.125 ~ 2.5
      )
    ,
    AHEI_ALL = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
      AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM + AHEI_ALCOHOL,
    
    AHEI_NOETOH = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
      AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM
  ) %>% select(SEQN, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN, AHEI_NUTSLEG, AHEI_N3FAT,
               AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_TRANS, AHEI_SODIUM, AHEI_ALCOHOL)

#### merging covariates 
covariate <- merge(covariate_nodiet, AHEI_score, by ='SEQN', all = TRUE)

#### medical conditions - CVD & malignancy ####
# CVD: self-reported history of coronary heart disease, angina, heart attack, or stroke

# 2005-2006
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/MCQ_D.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
med_05 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

med_05 <- med_05 %>% mutate(chd = case_when(MCQ160C==1 ~ 1,
                                            MCQ160C %in% c(2,7,9) ~ 0,
                                            TRUE ~ NA_integer_),
                            angina = case_when(MCQ160D==1 ~ 1,
                                               MCQ160D %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_),
                            heart_attack = case_when(MCQ160E==1 ~ 1,
                                                     MCQ160E %in% c(2,7,9) ~ 0,
                                                     TRUE ~ NA_integer_),
                            stroke = case_when(MCQ160F==1 ~ 1,
                                               MCQ160F %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_),
                            cancer = case_when(MCQ220==1 ~ 1,
                                               MCQ220 %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_)) %>% select(SEQN, chd, angina, heart_attack, stroke, cancer)

# 2007-2008
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/MCQ_E.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
med_07 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

med_07 <- med_07 %>% mutate(chd = case_when(MCQ160C==1 ~ 1,
                                            MCQ160C %in% c(2,7,9) ~ 0,
                                            TRUE ~ NA_integer_),
                            angina = case_when(MCQ160D==1 ~ 1,
                                               MCQ160D %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_),
                            heart_attack = case_when(MCQ160E==1 ~ 1,
                                                     MCQ160E %in% c(2,7,9) ~ 0,
                                                     TRUE ~ NA_integer_),
                            stroke = case_when(MCQ160F==1 ~ 1,
                                               MCQ160F %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_),
                            cancer = case_when(MCQ220==1 ~ 1,
                                               MCQ220 %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_)) %>% select(SEQN, chd, angina, heart_attack, stroke, cancer)

# 2009-2010
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/MCQ_F.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
med_09 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

med_09 <- med_09 %>% mutate(chd = case_when(MCQ160C==1 ~ 1,
                                            MCQ160C %in% c(2,7,9) ~ 0,
                                            TRUE ~ NA_integer_),
                            angina = case_when(MCQ160D==1 ~ 1,
                                               MCQ160D %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_),
                            heart_attack = case_when(MCQ160E==1 ~ 1,
                                                     MCQ160E %in% c(2,7,9) ~ 0,
                                                     TRUE ~ NA_integer_),
                            stroke = case_when(MCQ160F==1 ~ 1,
                                               MCQ160F %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_),
                            cancer = case_when(MCQ220==1 ~ 1,
                                               MCQ220 %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_)) %>% select(SEQN, chd, angina, heart_attack, stroke, cancer)

# 2011-2012
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/MCQ_G.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
med_11 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

med_11 <- med_11 %>% mutate(chd = case_when(MCQ160C==1 ~ 1,
                                            MCQ160C %in% c(2,7,9) ~ 0,
                                            TRUE ~ NA_integer_),
                            angina = case_when(MCQ160D==1 ~ 1,
                                               MCQ160D %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_),
                            heart_attack = case_when(MCQ160E==1 ~ 1,
                                                     MCQ160E %in% c(2,7,9) ~ 0,
                                                     TRUE ~ NA_integer_),
                            stroke = case_when(MCQ160F==1 ~ 1,
                                               MCQ160F %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_),
                            cancer = case_when(MCQ220==1 ~ 1,
                                               MCQ220 %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_)) %>% select(SEQN, chd, angina, heart_attack, stroke, cancer)

# 2013-2014
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/MCQ_H.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
med_13 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

med_13 <- med_13 %>% mutate(chd = case_when(MCQ160C==1 ~ 1,
                                            MCQ160C %in% c(2,7,9) ~ 0,
                                            TRUE ~ NA_integer_),
                            angina = case_when(MCQ160D==1 ~ 1,
                                               MCQ160D %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_),
                            heart_attack = case_when(MCQ160E==1 ~ 1,
                                                     MCQ160E %in% c(2,7,9) ~ 0,
                                                     TRUE ~ NA_integer_),
                            stroke = case_when(MCQ160F==1 ~ 1,
                                               MCQ160F %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_),
                            cancer = case_when(MCQ220==1 ~ 1,
                                               MCQ220 %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_)) %>% select(SEQN, chd, angina, heart_attack, stroke, cancer)

# 2015-2016
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/MCQ_I.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
med_15 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

med_15 <- med_15 %>% mutate(chd = case_when(MCQ160C==1 ~ 1,
                                            MCQ160C %in% c(2,7,9) ~ 0,
                                            TRUE ~ NA_integer_),
                            angina = case_when(MCQ160D==1 ~ 1,
                                               MCQ160D %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_),
                            heart_attack = case_when(MCQ160E==1 ~ 1,
                                                     MCQ160E %in% c(2,7,9) ~ 0,
                                                     TRUE ~ NA_integer_),
                            stroke = case_when(MCQ160F==1 ~ 1,
                                               MCQ160F %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_),
                            cancer = case_when(MCQ220==1 ~ 1,
                                               MCQ220 %in% c(2,7,9) ~ 0,
                                               TRUE ~ NA_integer_)) %>% select(SEQN, chd, angina, heart_attack, stroke, cancer)


med <- rbind(med_05, med_07, med_09, med_11, med_13, med_15)

covariate3 <- merge(med, covariate,by='SEQN', all.y=TRUE)

#### alcohol consumption ####
# 2005-2006
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DR1TOT_D.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
alco_05_1 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DR2TOT_D.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
alco_05_2 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

alco_05 <- merge(alco_05_1, alco_05_2, by='SEQN', all.x = TRUE)  %>% mutate(alco = case_when(is.na(DR2TALCO) ~ DR1TALCO,
                                                                                             TRUE ~ (DR1TALCO + DR2TALCO)/2)) %>% select(SEQN, alco)



# 2007-2008
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DR1TOT_E.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
alco_07_1 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DR2TOT_E.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
alco_07_2 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

alco_07 <- merge(alco_07_1, alco_07_2, by='SEQN', all.x = TRUE)  %>% mutate(alco = case_when(is.na(DR2TALCO) ~ DR1TALCO,
                                                                                             TRUE ~ (DR1TALCO + DR2TALCO)/2)) %>% select(SEQN, alco)


# 2009-2010
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DR1TOT_F.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
alco_09_1 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DR2TOT_F.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
alco_09_2 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

alco_09 <- merge(alco_09_1, alco_09_2, by='SEQN', all.x = TRUE)  %>% mutate(alco = case_when(is.na(DR2TALCO) ~ DR1TALCO,
                                                                                             TRUE ~ (DR1TALCO + DR2TALCO)/2)) %>% select(SEQN, alco)


# 2011-2012
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DR1TOT_G.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
alco_11_1 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DR2TOT_G.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
alco_11_2 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

alco_11 <- merge(alco_11_1, alco_11_2, by='SEQN', all.x = TRUE)  %>% mutate(alco = case_when(is.na(DR2TALCO) ~ DR1TALCO,
                                                                                             TRUE ~ (DR1TALCO + DR2TALCO)/2)) %>% select(SEQN, alco)


# 2013-2014
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DR1TOT_H.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
alco_13_1 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DR2TOT_H.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
alco_13_2 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

alco_13 <- merge(alco_13_1, alco_13_2, by='SEQN', all.x = TRUE)  %>% mutate(alco = case_when(is.na(DR2TALCO) ~ DR1TALCO,
                                                                                             TRUE ~ (DR1TALCO + DR2TALCO)/2)) %>% select(SEQN, alco)


# 2015-2016
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1TOT_I.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
alco_15_1 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR2TOT_I.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
alco_15_2 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

alco_15 <- merge(alco_15_1, alco_15_2, by='SEQN', all.x = TRUE)  %>% mutate(alco = case_when(is.na(DR2TALCO) ~ DR1TALCO,
                                                                                             TRUE ~ (DR1TALCO + DR2TALCO)/2)) %>% select(SEQN, alco)


alco <- rbind(alco_05,alco_07, alco_09, alco_11, alco_13,alco_15)

covariate4 <- merge(covariate3, alco, by='SEQN', all.x=TRUE)

#### MET variable  ####
# 2005-2006
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2005/DataFiles/PAQ_D.xpt", tf <- tempfile(), mode="wb")
PA_05 <- foreign::read.xport(tf)

PA_05_2 <- PA_05 %>% drop_na(PAD020) %>% mutate(PAD020 = case_when(PAD020 == 1 ~ 1,
                                               PAD020 %in% c(2,3,7,9) ~ 0,
                                               TRUE ~ NA_integer_),
                            PAQ100 = case_when(PAQ100 ==1 ~ 1,
                                               PAQ100 %in% c(2,3,7,9) ~ 0,
                                               TRUE ~ NA_integer_),
                            PAQ180_1 = case_when(PAQ180 == 1 ~ 1,
                                                 PAQ180 %in% c(2,3,4,7,9) ~ 0,
                                                 TRUE ~ NA_integer_),
                            PAQ180_2 = case_when(PAQ180 == 2 ~ 1,
                                                 PAQ180 %in% c(1,3,4,7,9) ~ 0,
                                                 TRUE ~ NA_integer_),
                            PAQ180_3 = case_when(PAQ180 == 3 ~ 1,
                                                 PAQ180 %in% c(1,2,4,7,9) ~ 0,
                                                 TRUE ~ NA_integer_),
                            PAQ180_4 = case_when(PAQ180 == 4 ~ 1,
                                                 PAQ180 %in% c(1,2,3,7,9) ~ 0,
                                                 TRUE ~ NA_integer_),
                            PAD440 = case_when(PAD440 == 1 ~ 1,
                                               PAD440 %in% c(2,3,7,9) ~ 0,
                                               TRUE ~ NA_integer_),
                            PAQ560 = case_when(PAQ560 < 78 ~ PAQ560,
                                               TRUE ~ NA_integer_),
                            PAD590 = case_when(PAD590 %in% c(1,2,3,4,5) ~ PAD590,
                                               PAD590 %in% c(0,6) ~ 0,
                                               TRUE ~ NA_integer_),
                            PAD600 = case_when(PAD600 %in% c(1,2,3,4,5) ~ PAD600,
                                               PAD600 %in% c(0,6) ~ 0,
                                               TRUE ~ NA_integer_)) %>% replace_na(list(
                                                 PAD020 = 0, PAQ100 = 0, PAQ180_1 = 0, PAQ180_2 = 0, PAQ180_3 = 0, PAQ180_4 = 0,
                                                 PAD440 = 0, PAQ560 = 0, PAD590 = 0, PAD600 = 0
                                               ))%>%
  mutate(MET = PAD020 *4 + PAQ100*4.5 + PAQ180_1 *1.4 + PAQ180_2 *1.5 + PAQ180_3 *1.6 + PAQ180_4 *1.8 + PAD440*4 + PAQ560*7 + PAD590*1 + PAD600*1.5) %>%
  filter(MET != 0) %>% 
  mutate(MET_quar = cut(MET, quantile(MET, probs = seq(0,1,1/4)), labels = c(1,2,3,4),include.lowest = TRUE))%>% select(SEQN, MET, MET_quar)

# 2007-2008
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2007/DataFiles/PAQ_E.xpt", tf <- tempfile(), mode="wb")
PA_07 <- foreign::read.xport(tf)

PA_07_2 <- PA_07 %>% drop_na(PAQ605) %>% mutate(PAD615 = case_when(PAD615<961 ~ PAD615,
                                               TRUE ~ NA_integer_),
                            PAD630 = case_when(PAD630<1441 ~ PAD630,
                                                 TRUE ~ NA_integer_),
                            PAD645 = case_when(PAD645< 601 ~ PAD645,
                                               TRUE ~ NA_integer_),
                            PAD660 = case_when(PAD660<991 ~ PAD660,
                                               TRUE ~ NA_integer_),
                            PAD675 = case_when(PAD675<721 ~ PAD675,
                                               TRUE ~ NA_integer_)) %>% replace_na(list(
                                                 PAD615 = 0, PAD630 = 0, PAD645 = 0, PAD660 = 0, PAD675 = 0)) %>%
  mutate(MET = PAD615*8 + PAD630*4 + PAD645*4 + PAD660*8 + PAD675*4) %>% filter(MET != 0) %>%
  mutate(MET_quar = cut(MET, quantile(MET, probs = seq(0,1,1/4)), labels = c(1,2,3,4),include.lowest = TRUE)) %>% select(SEQN, MET, MET_quar)
# almost 2/3 missing in PA

# 2009-2010
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2009/DataFiles/PAQ_F.xpt", tf <- tempfile(), mode="wb")
PA_09 <- foreign::read.xport(tf)

PA_09_2 <- PA_09 %>% drop_na(PAQ605) %>% mutate(PAD615 = case_when(PAD615<961 ~ PAD615,
                                                                   TRUE ~ NA_integer_),
                                                PAD630 = case_when(PAD630<1441 ~ PAD630,
                                                                   TRUE ~ NA_integer_),
                                                PAD645 = case_when(PAD645< 601 ~ PAD645,
                                                                   TRUE ~ NA_integer_),
                                                PAD660 = case_when(PAD660<991 ~ PAD660,
                                                                   TRUE ~ NA_integer_),
                                                PAD675 = case_when(PAD675<721 ~ PAD675,
                                                                   TRUE ~ NA_integer_)) %>% replace_na(list(
                                                                     PAD615 = 0, PAD630 = 0, PAD645 = 0, PAD660 = 0, PAD675 = 0)) %>%
  mutate(MET = PAD615*8 + PAD630*4 + PAD645*4 + PAD660*8 + PAD675*4) %>% filter(MET != 0) %>%
  mutate(MET_quar = cut(MET, quantile(MET, probs = seq(0,1,1/4)), labels = c(1,2,3,4),include.lowest = TRUE)) %>% select(SEQN, MET, MET_quar)

# 2011-2012
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/PAQ_G.xpt", tf <- tempfile(), mode="wb")
PA_11 <- foreign::read.xport(tf)

PA_11_2 <- PA_11 %>% drop_na(PAQ605) %>% mutate(PAD615 = case_when(PAD615<961 ~ PAD615,
                                                                   TRUE ~ NA_integer_),
                                                PAD630 = case_when(PAD630<1441 ~ PAD630,
                                                                   TRUE ~ NA_integer_),
                                                PAD645 = case_when(PAD645< 601 ~ PAD645,
                                                                   TRUE ~ NA_integer_),
                                                PAD660 = case_when(PAD660<991 ~ PAD660,
                                                                   TRUE ~ NA_integer_),
                                                PAD675 = case_when(PAD675<721 ~ PAD675,
                                                                   TRUE ~ NA_integer_)) %>% replace_na(list(
                                                                     PAD615 = 0, PAD630 = 0, PAD645 = 0, PAD660 = 0, PAD675 = 0)) %>%
  mutate(MET = PAD615*8 + PAD630*4 + PAD645*4 + PAD660*8 + PAD675*4) %>% filter(MET != 0) %>%
  mutate(MET_quar = cut(MET, quantile(MET, probs = seq(0,1,1/4)), labels = c(1,2,3,4),include.lowest = TRUE)) %>% select(SEQN, MET, MET_quar)

# 2013-2014
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/PAQ_H.xpt", tf <- tempfile(), mode="wb")
PA_13 <- foreign::read.xport(tf)

PA_13_2 <- PA_13 %>% drop_na(PAQ605) %>% mutate(PAD615 = case_when(PAD615<961 ~ PAD615,
                                                                   TRUE ~ NA_integer_),
                                                PAD630 = case_when(PAD630<1441 ~ PAD630,
                                                                   TRUE ~ NA_integer_),
                                                PAD645 = case_when(PAD645< 601 ~ PAD645,
                                                                   TRUE ~ NA_integer_),
                                                PAD660 = case_when(PAD660<991 ~ PAD660,
                                                                   TRUE ~ NA_integer_),
                                                PAD675 = case_when(PAD675<721 ~ PAD675,
                                                                   TRUE ~ NA_integer_)) %>% replace_na(list(
                                                                     PAD615 = 0, PAD630 = 0, PAD645 = 0, PAD660 = 0, PAD675 = 0)) %>%
  mutate(MET = PAD615*8 + PAD630*4 + PAD645*4 + PAD660*8 + PAD675*4) %>% filter(MET != 0) %>%
  mutate(MET_quar = cut(MET, quantile(MET, probs = seq(0,1,1/4)), labels = c(1,2,3,4),include.lowest = TRUE)) %>% select(SEQN, MET, MET_quar)

# 2015-2016
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/PAQ_I.xpt", tf <- tempfile(), mode="wb")
PA_15 <- foreign::read.xport(tf)

PA_15_2 <- PA_15 %>% drop_na(PAQ605) %>% mutate(PAD615 = case_when(PAD615<961 ~ PAD615,
                                                                   TRUE ~ NA_integer_),
                                                PAD630 = case_when(PAD630<1441 ~ PAD630,
                                                                   TRUE ~ NA_integer_),
                                                PAD645 = case_when(PAD645< 601 ~ PAD645,
                                                                   TRUE ~ NA_integer_),
                                                PAD660 = case_when(PAD660<991 ~ PAD660,
                                                                   TRUE ~ NA_integer_),
                                                PAD675 = case_when(PAD675<721 ~ PAD675,
                                                                   TRUE ~ NA_integer_)) %>% replace_na(list(
                                                                     PAD615 = 0, PAD630 = 0, PAD645 = 0, PAD660 = 0, PAD675 = 0)) %>%
  mutate(MET = PAD615*8 + PAD630*4 + PAD645*4 + PAD660*8 + PAD675*4) %>% filter(MET != 0) %>%
  mutate(MET_quar = cut(MET, quantile(MET, probs = seq(0,1,1/4)), labels = c(1,2,3,4),include.lowest = TRUE)) %>% select(SEQN, MET, MET_quar)


PA <- rbind(PA_05_2, PA_07_2, PA_09_2, PA_11_2, PA_13_2, PA_15_2)

covariate_total <- merge(covariate4, PA, by='SEQN',all.x = TRUE)
# write.csv(covariate_total, file = 'covariate.csv', row.names=FALSE)


