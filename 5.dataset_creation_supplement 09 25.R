library(haven)
library(httr)
library(tidyverse)

#### 24 hour recall supplment use
# day 1 + day 2 use average
# available 2007-2016

# keep vitD, vitB6, vitB12, and folic acid data 

# 2007-2008 
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DS1TOT_E.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
supp_07_1 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

supp_07_1 <- supp_07_1 %>% select(SEQN, DRDINT, DS1DS, DS1TFA, DS1TVB6, DS1TVB12,DS1TVD)
# DRDINT: whether the sample person has intake data for one or two days. 1 - Day 1 only; 2 - Day 1 + Day 2
# DS1DS: Any Dietary Supplements taken in the past 24 hour?  1-yes, 2-no, 7-refused, 9-don't know
# FA is folic acid in mcg not DFE

url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DS2TOT_E.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
supp_07_2 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

supp_07_2 <- supp_07_2 %>% select(SEQN, DS2DS, DS2TFA, DS2TVB6, DS2TVB12,DS2TVD)

supp_07_to_clean <- merge(supp_07_1, supp_07_2,by='SEQN')
supp_07_onlyday1 <- supp_07_to_clean %>% filter(DRDINT ==1) %>% 
  replace_na(list(DS1TFA = 0, 
                  DS1TVD = 0,
                  DS1TVB6 = 0,
                  DS1TVB12 = 0)) %>% 
  rename(FA_supp=DS1TFA,
         vitD_supp=DS1TVD,
         vitB6_supp=DS1TVB6,
         vitB12_supp=DS1TVB12) %>% 
  mutate(any_supp=case_when(DS1DS == 1 ~ 1,
                            DS1DS %in% c(2,7,9) ~ 0,
                            TRUE ~ NA_integer_)) %>%
  select(SEQN, any_supp, FA_supp, vitD_supp,vitB6_supp,vitB12_supp)

supp_07_2day <- supp_07_to_clean %>% filter(DRDINT == 2) %>% 
  replace_na(list(DS1TFA = 0, DS2TFA = 0, 
                  DS1TVD = 0, DS2TVD = 0, 
                  DS1TVB6 = 0, DS2TVB6 = 0, 
                  DS1TVB12 = 0, DS2TVB12 = 0)) %>%
  mutate(FA_supp=(DS1TFA+DS2TFA)/2,
         vitD_supp=(DS1TVD+DS2TVD)/2,
         vitB6_supp=(DS1TVB6+DS2TVB6)/2,
         vitB12_supp=(DS1TVB12+DS2TVB12)/2) %>% 
  mutate(any_supp=case_when(DS1DS == 1 ~ 1,
                            DS1DS %in% c(2,7,9) ~ 0,
                            TRUE ~ NA_integer_)) %>%
  select(SEQN,any_supp, FA_supp, vitD_supp,vitB6_supp,vitB12_supp)

supp_07 <- rbind(supp_07_onlyday1,supp_07_2day)

# 2009-2010
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DS1TOT_F.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
supp_09_1 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

supp_09_1 <- supp_09_1 %>% select(SEQN, DRDINT, DS1DS, DS1TFA, DS1TVB6, DS1TVB12,DS1TVD)
# DRDINT: whether the sample person has intake data for one or two days. 1 - Day 1 only; 2 - Day 1 + Day 2
# DS1DS: Any Dietary Supplements taken in the past 24 hour?  1-yes, 2-no, 7-refused, 9-don't know
# FA is folic acid in mcg not DFE

url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DS2TOT_F.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
supp_09_2 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

supp_09_2 <- supp_09_2 %>% select(SEQN, DS2DS, DS2TFA, DS2TVB6, DS2TVB12,DS2TVD)

supp_09_to_clean <- merge(supp_09_1, supp_09_2,by='SEQN')
supp_09_onlyday1 <- supp_09_to_clean %>% filter(DRDINT ==1) %>% 
  replace_na(list(DS1TFA = 0, 
                  DS1TVD = 0,
                  DS1TVB6 = 0,
                  DS1TVB12 = 0)) %>% 
  rename(FA_supp=DS1TFA,
         vitD_supp=DS1TVD,
         vitB6_supp=DS1TVB6,
         vitB12_supp=DS1TVB12) %>% 
  mutate(any_supp=case_when(DS1DS == 1 ~ 1,
                            DS1DS %in% c(2,7,9) ~ 0,
                            TRUE ~ NA_integer_)) %>%
  select(SEQN, any_supp, FA_supp, vitD_supp,vitB6_supp,vitB12_supp)

supp_09_2day <- supp_09_to_clean %>% filter(DRDINT == 2) %>% 
  replace_na(list(DS1TFA = 0, DS2TFA = 0, 
                  DS1TVD = 0, DS2TVD = 0, 
                  DS1TVB6 = 0, DS2TVB6 = 0, 
                  DS1TVB12 = 0, DS2TVB12 = 0)) %>%
  mutate(FA_supp=(DS1TFA+DS2TFA)/2,
         vitD_supp=(DS1TVD+DS2TVD)/2,
         vitB6_supp=(DS1TVB6+DS2TVB6)/2,
         vitB12_supp=(DS1TVB12+DS2TVB12)/2) %>% 
  mutate(any_supp=case_when(DS1DS == 1 ~ 1,
                            DS1DS %in% c(2,7,9) ~ 0,
                            TRUE ~ NA_integer_)) %>%
  select(SEQN,any_supp, FA_supp, vitD_supp,vitB6_supp,vitB12_supp)

supp_09 <- rbind(supp_09_onlyday1,supp_09_2day)

# 2011-2012
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DS1TOT_G.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
supp_11_1 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

supp_11_1 <- supp_11_1 %>% select(SEQN, DRDINT, DS1DS, DS1TFA, DS1TVB6, DS1TVB12,DS1TVD)
# DRDINT: whether the sample person has intake data for one or two days. 1 - Day 1 only; 2 - Day 1 + Day 2
# DS1DS: Any Dietary Supplements taken in the past 24 hour?  1-yes, 2-no, 7-refused, 9-don't know
# FA is folic acid in mcg not DFE

url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DS2TOT_G.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
supp_11_2 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

supp_11_2 <- supp_11_2 %>% select(SEQN, DS2DS, DS2TFA, DS2TVB6, DS2TVB12,DS2TVD)

supp_11_to_clean <- merge(supp_11_1, supp_11_2,by='SEQN')
supp_11_onlyday1 <- supp_11_to_clean %>% filter(DRDINT ==1) %>% 
  replace_na(list(DS1TFA = 0, 
                  DS1TVD = 0,
                  DS1TVB6 = 0,
                  DS1TVB12 = 0)) %>% 
  rename(FA_supp=DS1TFA,
         vitD_supp=DS1TVD,
         vitB6_supp=DS1TVB6,
         vitB12_supp=DS1TVB12) %>% 
  mutate(any_supp=case_when(DS1DS == 1 ~ 1,
                            DS1DS %in% c(2,7,9) ~ 0,
                            TRUE ~ NA_integer_)) %>%
  select(SEQN, any_supp, FA_supp, vitD_supp,vitB6_supp,vitB12_supp)

supp_11_2day <- supp_11_to_clean %>% filter(DRDINT == 2) %>% 
  replace_na(list(DS1TFA = 0, DS2TFA = 0, 
                  DS1TVD = 0, DS2TVD = 0, 
                  DS1TVB6 = 0, DS2TVB6 = 0, 
                  DS1TVB12 = 0, DS2TVB12 = 0)) %>%
  mutate(FA_supp=(DS1TFA+DS2TFA)/2,
         vitD_supp=(DS1TVD+DS2TVD)/2,
         vitB6_supp=(DS1TVB6+DS2TVB6)/2,
         vitB12_supp=(DS1TVB12+DS2TVB12)/2) %>% 
  mutate(any_supp=case_when(DS1DS == 1 ~ 1,
                            DS1DS %in% c(2,7,9) ~ 0,
                            TRUE ~ NA_integer_)) %>%
  select(SEQN,any_supp, FA_supp, vitD_supp,vitB6_supp,vitB12_supp)

supp_11 <- rbind(supp_11_onlyday1,supp_11_2day)



# 2013-2014
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DS1TOT_H.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
supp_13_1 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

supp_13_1 <- supp_13_1 %>% select(SEQN, DRDINT, DS1DS, DS1TFA, DS1TVB6, DS1TVB12,DS1TVD)
# DRDINT: whether the sample person has intake data for one or two days. 1 - Day 1 only; 2 - Day 1 + Day 2
# DS1DS: Any Dietary Supplements taken in the past 24 hour?  1-yes, 2-no, 7-refused, 9-don't know
# FA is folic acid in mcg not DFE

url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DS2TOT_H.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
supp_13_2 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

supp_13_2 <- supp_13_2 %>% select(SEQN, DS2DS, DS2TFA, DS2TVB6, DS2TVB12,DS2TVD)

supp_13_to_clean <- merge(supp_13_1, supp_13_2,by='SEQN')
supp_13_onlyday1 <- supp_13_to_clean %>% filter(DRDINT ==1) %>% 
  replace_na(list(DS1TFA = 0, 
                  DS1TVD = 0,
                  DS1TVB6 = 0,
                  DS1TVB12 = 0)) %>% 
  rename(FA_supp=DS1TFA,
         vitD_supp=DS1TVD,
         vitB6_supp=DS1TVB6,
         vitB12_supp=DS1TVB12) %>% 
  mutate(any_supp=case_when(DS1DS == 1 ~ 1,
                            DS1DS %in% c(2,7,9) ~ 0,
                            TRUE ~ NA_integer_)) %>%
  select(SEQN, any_supp, FA_supp, vitD_supp,vitB6_supp,vitB12_supp)

supp_13_2day <- supp_13_to_clean %>% filter(DRDINT == 2) %>% 
  replace_na(list(DS1TFA = 0, DS2TFA = 0, 
                  DS1TVD = 0, DS2TVD = 0, 
                  DS1TVB6 = 0, DS2TVB6 = 0, 
                  DS1TVB12 = 0, DS2TVB12 = 0)) %>%
  mutate(FA_supp=(DS1TFA+DS2TFA)/2,
         vitD_supp=(DS1TVD+DS2TVD)/2,
         vitB6_supp=(DS1TVB6+DS2TVB6)/2,
         vitB12_supp=(DS1TVB12+DS2TVB12)/2) %>% 
  mutate(any_supp=case_when(DS1DS == 1 ~ 1,
                            DS1DS %in% c(2,7,9) ~ 0,
                            TRUE ~ NA_integer_)) %>%
  select(SEQN,any_supp, FA_supp, vitD_supp,vitB6_supp,vitB12_supp)

supp_13 <- rbind(supp_13_onlyday1,supp_13_2day)



# 2015-2016
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DS1TOT_I.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
supp_15_1 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

supp_15_1 <- supp_15_1 %>% select(SEQN, DRDINT, DS1DS, DS1TFA, DS1TVB6, DS1TVB12,DS1TVD)
# DRDINT: whether the sample person has intake data for one or two days. 1 - Day 1 only; 2 - Day 1 + Day 2
# DS1DS: Any Dietary Supplements taken in the past 24 hour?  1-yes, 2-no, 7-refused, 9-don't know
# FA is folic acid in mcg not DFE

url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DS2TOT_I.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
supp_15_2 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

supp_15_2 <- supp_15_2 %>% select(SEQN, DS2DS, DS2TFA, DS2TVB6, DS2TVB12,DS2TVD)

supp_15_to_clean <- merge(supp_15_1, supp_15_2,by='SEQN')
supp_15_onlyday1 <- supp_15_to_clean %>% filter(DRDINT ==1) %>% 
  replace_na(list(DS1TFA = 0, 
                  DS1TVD = 0,
                  DS1TVB6 = 0,
                  DS1TVB12 = 0)) %>% 
  rename(FA_supp=DS1TFA,
         vitD_supp=DS1TVD,
         vitB6_supp=DS1TVB6,
         vitB12_supp=DS1TVB12) %>% 
  mutate(any_supp=case_when(DS1DS == 1 ~ 1,
                            DS1DS %in% c(2,7,9) ~ 0,
                            TRUE ~ NA_integer_)) %>%
  select(SEQN, any_supp, FA_supp, vitD_supp,vitB6_supp,vitB12_supp)

supp_15_2day <- supp_15_to_clean %>% filter(DRDINT == 2) %>% 
  replace_na(list(DS1TFA = 0, DS2TFA = 0, 
                  DS1TVD = 0, DS2TVD = 0, 
                  DS1TVB6 = 0, DS2TVB6 = 0, 
                  DS1TVB12 = 0, DS2TVB12 = 0)) %>%
  mutate(FA_supp=(DS1TFA+DS2TFA)/2,
         vitD_supp=(DS1TVD+DS2TVD)/2,
         vitB6_supp=(DS1TVB6+DS2TVB6)/2,
         vitB12_supp=(DS1TVB12+DS2TVB12)/2) %>% 
  mutate(any_supp=case_when(DS1DS == 1 ~ 1,
                            DS1DS %in% c(2,7,9) ~ 0,
                            TRUE ~ NA_integer_)) %>%
  select(SEQN,any_supp, FA_supp, vitD_supp,vitB6_supp,vitB12_supp)

supp_15 <- rbind(supp_15_onlyday1,supp_15_2day)

supp <- rbind(supp_07, supp_09, supp_11, supp_13, supp_15)

# write.csv(supp, file='supplement.csv', row.names = FALSE)
