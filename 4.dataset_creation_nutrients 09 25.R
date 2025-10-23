library(haven)
library(httr)
library(tidyverse)
#### vitamins #####

###### vitamin D ######
# 2005-2006
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/VID_D.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
vitD_05 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

# 2007-2008
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/VID_E.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
vitD_07 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

# 2009-2010
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/VID_F.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
vitD_09 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

# 2011-2012
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/VID_G.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
vitD_11 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)


# 2013-2014
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/VID_H.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
vitD_13 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

# 2015-2016
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/VID_I.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
vitD_15 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)


# combining data from different cycles
vitD_05 <- vitD_05 %>% mutate(cycle = '2005-2006')
vitD_07 <- vitD_07 %>% mutate(cycle = '2007-2008')
vitD_09 <- vitD_09 %>% mutate(cycle = '2009-2010')
vitD_11 <- vitD_11 %>% mutate(cycle = '2011-2012')
vitD_13 <- vitD_13 %>% mutate(cycle = '2013-2014')
vitD_15 <- vitD_15 %>% mutate(cycle = '2015-2016')
vitD <- bind_rows(vitD_05, vitD_07, vitD_09, vitD_11, vitD_13, vitD_15)
vitD <- vitD %>%
  mutate(across(everything(), ~replace_na(., NA))) # Replace missing values with NA to make sure missingness is NA

vitD_2 <- vitD %>% mutate(vitD = case_when (cycle %in% c('2005-2006') ~ LBDVIDMS,
                                            TRUE ~ LBXVIDMS)) %>% select (SEQN, cycle, vitD) 
# unit of vitD is nmol/L

###### vitamin B12 ######
# 2005-2006
# this data only contain vitamin B12
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/B12_D.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
vitB12_05 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

# 2011-2012
# this data only contain vitamin B12
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/VITB12_G.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
vitB12_11 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

# 2013-2014
# this data only contain vitamin B12
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/VITB12_H.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
vitB12_13 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

# combining data from different cycles
vitB12_05 <- vitB12_05 %>% mutate(cycle = '2005-2006')
vitB12_11 <- vitB12_11 %>% mutate(cycle = '2011-2012')
vitB12_13 <- vitB12_13 %>% mutate(cycle = '2013-2014')
vitB12 <- bind_rows(vitB12_05, vitB12_11, vitB12_13)
vitB12 <- vitB12 %>%
  mutate(across(everything(), ~replace_na(., NA))) # Replace missing values with NA to make sure missingness is NA

vitB12_2 <- vitB12 %>% rename(vitB12_2 = LBXB12,
                              vitB12_3 = LBDB12) %>% mutate(vitB12 = case_when(cycle=='2013-2014' ~ vitB12_3,
                                                                               TRUE ~ vitB12_2)) %>% select(SEQN, cycle, vitB12)

# unit of serum vitB12 = pg/mL

###### vitamin B6 ######
# 2005-2006
# active form of vitamin B6 + metabolite
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/VIT_B6_D.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
vitB6_05 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

# 2007-2008
# active form of vitamin B6 + metabolite
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/VIT_B6_E.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
vitB6_07 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

# 2009-2010
# active form of vitamin B6 + metabolite
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/VIT_B6_F.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
vitB6_09 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

# combining data from different cycles
vitB6_05 <- vitB6_05 %>% mutate(cycle = '2005-2006')
vitB6_07 <- vitB6_07 %>% mutate(cycle = '2007-2008')
vitB6_09 <- vitB6_09 %>% mutate(cycle = '2009-2010')
vitB6 <- bind_rows(vitB6_05, vitB6_07, vitB6_09)
vitB6 <- vitB6 %>%
  mutate(across(everything(), ~replace_na(., NA))) # Replace missing values with NA to make sure missingness is NA

vitB6_2 <- vitB6 %>% rename(vitB6 = LBXPLP) %>% select(SEQN, cycle, vitB6)

# unit is nmol/L

##### folate #####
# 2005-2006
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/FOLATE_D.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
folate_05 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

folate_05 <- folate_05 %>% rename(fol_rbc = LBXRBF) %>% select(SEQN, fol_rbc)
# ng/mL for RBC folate 

# 2007-2008
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/FOLATE_E.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
folate_07 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

folate_07 <- folate_07 %>% rename(fol_rbc = LBDRBF) %>% select(SEQN, fol_rbc)
# different variable names in the original folate files

# 2009-2010
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/FOLATE_F.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
folate_09 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)

folate_09 <- folate_09 %>% rename(fol_rbc = LBDRBF) %>% select(SEQN, fol_rbc)
# different variable names in the original folate files

# 2011-2012
# starting this cycle, serum and rbc folate were stored in separate files
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/FOLATE_G.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
folate_rbc_11 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)


folate_11 <- folate_rbc_11 %>% rename(fol_rbc = LBDRFO) %>% select(SEQN, fol_rbc)

# 2013-2014
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/FOLATE_H.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
folate_rbc_13 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)


folate_13 <- folate_rbc_13 %>% rename(fol_rbc = LBDRFO) %>% select(SEQN, fol_rbc)


# 2015-2016
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/FOLATE_I.xpt"
temp_file <- tempfile(fileext = ".xpt")
# Download the file to a temporary location
GET(url, write_disk(temp_file))
# Read the XPT file from the temporary location
folate_rbc_15 <- read_xpt(temp_file)
# Clean up the temporary file
unlink(temp_file)


folate_15 <- folate_rbc_15 %>% rename(fol_rbc = LBDRFO) %>% select(SEQN, fol_rbc)
# merge all datasets
# combining data from different cycles
folate_05 <- folate_05 %>% mutate(cycle = '2005-2006')
folate_07 <- folate_07 %>% mutate(cycle = '2007-2008')
folate_09 <- folate_09 %>% mutate(cycle = '2009-2010')
folate_11 <- folate_11 %>% mutate(cycle = '2011-2012')
folate_13 <- folate_13 %>% mutate(cycle = '2013-2014')
folate_15 <- folate_15 %>% mutate(cycle = '2015-2016')
folate <- bind_rows(folate_05, folate_07, folate_09, folate_11, folate_13, folate_15)
folate <- folate %>%
  mutate(across(everything(), ~replace_na(., NA))) # Replace missing values with NA to make sure missingness is NA

#### summary ####
data <- merge(vitD_2,vitB12_2, by=c('SEQN','cycle'), all = TRUE)
data2 <- merge(data, vitB6_2, by=c('SEQN','cycle'), all = TRUE)
data3 <- merge(data2, folate, by=c('SEQN','cycle'), all = TRUE)
nutrients <- data3
# write.csv(nutrients, file = 'nutrients_all.csv',row.names = FALSE)
