library(haven)
library(httr)
library(tidyverse)
#### urinary phthalates #####
# 2005-2006
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/PHTHTE_D.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
phthalates_05 <- read_xpt(temp_file)
unlink(temp_file)

phthalates_05 <- phthalates_05 %>% mutate(cycle = '2005-2006') %>% 
  rename(MBP = URXMBP,
         MCP = URXMCP,
         MEP = URXMEP,
         MEHP = URXMHP,
         MNP = URXMNP,
         MOP = URXMOP,
         MBzP = URXMZP,
         MNMP = URXMNM,
         MVPP = URXMC1,
         MEHHP = URXMHH,
         MEOHP = URXMOH,
         MIBP = URXMIB,
         MECPP = URXECP,
         MCNP = URXCNP,
         MCOP = URXCOP,
         
         MBP_LOD = URDMBPLC,
         MCP_LOD = URDMCPLC,
         MEP_LOD = URDMEPLC,
         MEHP_LOD = URDMHPLC,
         MNP_LOD = URDMNPLC,
         MOP_LOD = URDMOPLC,
         MBzP_LOD = URDMZPLC,
         MNMP_LOD = URDMNMLC,
         MVPP_LOD = URDMC1LC,
         MEHHP_LOD = URDMHHLC,
         MEOHP_LOD = URDMOHLC,
         MIBP_LOD = URDMIBLC,
         MECPP_LOD = URDECPLC,
         MCNP_LOD = URDCNPLC,
         MCOP_LOD = URDCOPLC,
         
         weight_phth = WTSB2YR) %>% select(SEQN, cycle, weight_phth, MBP, MCP, MEP, MEHP, MNP, MOP,
                                          MBzP, MNMP, MVPP, MEHHP, MEOHP, MIBP, MECPP, MCNP, MCOP,
                                          MBP_LOD, MCP_LOD, MEP_LOD, MEHP_LOD, MNP_LOD, MOP_LOD,
                                          MBzP_LOD, MNMP_LOD, MVPP_LOD, MEHHP_LOD, MEOHP_LOD, MIBP_LOD, MECPP_LOD,
                                         MCNP_LOD, MCOP_LOD)

# 2007-2008
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/PHTHTE_E.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
phthalates_07 <- read_xpt(temp_file)
unlink(temp_file)

phthalates_07 <- phthalates_07 %>% mutate(cycle = '2007-2008') %>% 
  rename(MBP = URXMBP,
         MCP = URXMCP,
         MEP = URXMEP,
         MEHP = URXMHP,
         MNP = URXMNP,
         MOP = URXMOP,
         MBzP = URXMZP,
         MNMP = URXMNM,
         MVPP = URXMC1,
         MEHHP = URXMHH,
         MEOHP = URXMOH,
         MIBP = URXMIB,
         MECPP = URXECP,
         MCNP = URXCNP,
         MCOP = URXCOP,
         
         MBP_LOD = URDMBPLC,
         MCP_LOD = URDMCPLC,
         MEP_LOD = URDMEPLC,
         MEHP_LOD = URDMHPLC,
         MNP_LOD = URDMNPLC,
         MOP_LOD = URDMOPLC,
         MBzP_LOD = URDMZPLC,
         MNMP_LOD = URDMNMLC,
         MVPP_LOD = URDMC1LC,
         MEHHP_LOD = URDMHHLC,
         MEOHP_LOD = URDMOHLC,
         MIBP_LOD = URDMIBLC,
         MECPP_LOD = URDECPLC,
         MCNP_LOD = URDCNPLC,
         MCOP_LOD = URDCOPLC,
         
         weight_phth = WTSB2YR) %>% select(SEQN, cycle, weight_phth, MBP, MCP, MEP, MEHP, MNP, MOP,
                                         MBzP, MNMP, MVPP, MEHHP, MEOHP, MIBP, MECPP, MCNP, MCOP,
                                         MBP_LOD, MCP_LOD, MEP_LOD, MEHP_LOD, MNP_LOD, MOP_LOD,
                                         MBzP_LOD, MNMP_LOD, MVPP_LOD, MEHHP_LOD, MEOHP_LOD, MIBP_LOD, MECPP_LOD,
                                         MCNP_LOD, MCOP_LOD)

# 2009-2010
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/PHTHTE_F.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
phthalates_09 <- read_xpt(temp_file)
unlink(temp_file)

phthalates_09 <- phthalates_09 %>% mutate(cycle = '2009-2010') %>% 
  rename(MBP = URXMBP,
         MCP = URXMCP,
         MEP = URXMEP,
         MEHP = URXMHP,
         MNP = URXMNP,
         MOP = URXMOP,
         MBzP = URXMZP,
         MNMP = URXMNM,
         MVPP = URXMC1,
         MEHHP = URXMHH,
         MEOHP = URXMOH,
         MIBP = URXMIB,
         MECPP = URXECP,
         MCNP = URXCNP,
         MCOP = URXCOP,
         
         MBP_LOD = URDMBPLC,
         MCP_LOD = URDMCPLC,
         MEP_LOD = URDMEPLC,
         MEHP_LOD = URDMHPLC,
         MNP_LOD = URDMNPLC,
         MOP_LOD = URDMOPLC,
         MBzP_LOD = URDMZPLC,
         MNMP_LOD = URDMNMLC,
         MVPP_LOD = URDMC1LC,
         MEHHP_LOD = URDMHHLC,
         MEOHP_LOD = URDMOHLC,
         MIBP_LOD = URDMIBLC,
         MECPP_LOD = URDECPLC,
         MCNP_LOD = URDCNPLC,
         MCOP_LOD = URDCOPLC,
         
         weight_phth = WTSB2YR) %>% select(SEQN, cycle, weight_phth, MBP, MCP, MEP, MEHP, MNP, MOP,
                                         MBzP, MNMP, MVPP, MEHHP, MEOHP, MIBP, MECPP, MCNP, MCOP,
                                         MBP_LOD, MCP_LOD, MEP_LOD, MEHP_LOD, MNP_LOD, MOP_LOD,
                                         MBzP_LOD, MNMP_LOD, MVPP_LOD, MEHHP_LOD, MEOHP_LOD, MIBP_LOD, MECPP_LOD,
                                         MCNP_LOD, MCOP_LOD)

# 2011-2012
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PHTHTE_G.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
phthalates_11 <- read_xpt(temp_file)
unlink(temp_file)

phthalates_11 <- phthalates_11 %>% mutate(cycle = '2011-2012') %>% 
  rename(MBP = URXMBP,
         # MCP = URXMCP,
         MEP = URXMEP,
         MEHP = URXMHP,
         MNP = URXMNP,
         # MOP = URXMOP,
         MBzP = URXMZP,
         MNMP = URXMNM,
         MVPP = URXMC1,
         MEHHP = URXMHH,
         MEOHP = URXMOH,
         MIBP = URXMIB,
         MECPP = URXECP,
         MCNP = URXCNP,
         MCOP = URXCOP,
         MHNC = URXMHNC,
         
         MBP_LOD = URDMBPLC,
         # MCP_LOD = URDMCPLC,
         MEP_LOD = URDMEPLC,
         MEHP_LOD = URDMHPLC,
         MNP_LOD = URDMNPLC,
         # MOP_LOD = URDMOPLC,
         MBzP_LOD = URDMZPLC,
         MNMP_LOD = URDMNMLC,
         MVPP_LOD = URDMC1LC,
         MEHHP_LOD = URDMHHLC,
         MEOHP_LOD = URDMOHLC,
         MIBP_LOD = URDMIBLC,
         MECPP_LOD = URDECPLC,
         MCNP_LOD = URDCNPLC,
         MCOP_LOD = URDCOPLC,
         MHNC_LOD = URDMCHLC,
         weight_phth = WTSA2YR) %>% select(SEQN, cycle, weight_phth, MBP, MEP, MEHP, MNP,
                                         MBzP, MNMP, MVPP, MEHHP, MEOHP, MIBP, MECPP, MCNP, MCOP, MHNC,
                                         MBP_LOD, MEP_LOD, MEHP_LOD, MNP_LOD,
                                         MBzP_LOD, MNMP_LOD, MVPP_LOD, MEHHP_LOD, MEOHP_LOD, MIBP_LOD, MECPP_LOD,
                                         MCNP_LOD, MCOP_LOD, MHNC_LOD)

# 2013-2014
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/PHTHTE_H.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
phthalates_13 <- read_xpt(temp_file)
unlink(temp_file)

phthalates_13 <- phthalates_13 %>% mutate(cycle = '2013-2014') %>% 
  rename(MBP = URXMBP,
         # MCP = URXMCP,
         MEP = URXMEP,
         MEHP = URXMHP,
         MNP = URXMNP,
         # MOP = URXMOP,
         MBzP = URXMZP,
         # MNMP = URXMNM,
         MVPP = URXMC1,
         MEHHP = URXMHH,
         MEOHP = URXMOH,
         MIBP = URXMIB,
         MECPP = URXECP,
         MCNP = URXCNP,
         MCOP = URXCOP,
         MHNC = URXMHNC,
         
         MBP_LOD = URDMBPLC,
         # MCP_LOD = URDMCPLC,
         MEP_LOD = URDMEPLC,
         MEHP_LOD = URDMHPLC,
         MNP_LOD = URDMNPLC,
         # MOP_LOD = URDMOPLC,
         MBzP_LOD = URDMZPLC,
         # MNMP_LOD = URDMNMLC,
         MVPP_LOD = URDMC1LC,
         MEHHP_LOD = URDMHHLC,
         MEOHP_LOD = URDMOHLC,
         MIBP_LOD = URDMIBLC,
         MECPP_LOD = URDECPLC,
         MCNP_LOD = URDCNPLC,
         MCOP_LOD = URDCOPLC,
         MHNC_LOD = URDMCHLC,
         weight_phth = WTSB2YR) %>% select(SEQN, cycle, weight_phth, MBP, MEP, MEHP, MNP,
                                         MBzP, MVPP, MEHHP, MEOHP, MIBP, MECPP, MCNP, MCOP, MHNC,
                                         MBP_LOD, MEP_LOD, MEHP_LOD, MNP_LOD,
                                         MBzP_LOD, MVPP_LOD, MEHHP_LOD, MEOHP_LOD, MIBP_LOD, MECPP_LOD,
                                         MCNP_LOD, MCOP_LOD, MHNC_LOD)


# 2015 - 2016
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/PHTHTE_I.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
phthalates_15 <- read_xpt(temp_file)
unlink(temp_file)

phthalates_15 <- phthalates_15 %>% mutate(cycle = '2015-2016') %>% 
  rename(MBP = URXMBP,
         # MCP = URXMCP,
         MEP = URXMEP,
         MEHP = URXMHP,
         MNP = URXMNP,
         # MOP = URXMOP,
         MBzP = URXMZP,
         # MNMP = URXMNM,
         MVPP = URXMC1,
         MEHHP = URXMHH,
         MEOHP = URXMOH,
         MIBP = URXMIB,
         MECPP = URXECP,
         MCNP = URXCNP,
         MCOP = URXCOP,
         MHNC = URXMHNC,
         MHIBP = URXHIBP,
         MCOCH = URXMCOH,
         MHBP = URXMHBP,

         
         MBP_LOD = URDMBPLC,
         # MCP_LOD = URDMCPLC,
         MEP_LOD = URDMEPLC,
         MEHP_LOD = URDMHPLC,
         MNP_LOD = URDMNPLC,
         # MOP_LOD = URDMOPLC,
         MBzP_LOD = URDMZPLC,
         # MNMP_LOD = URDMNMLC,
         MVPP_LOD = URDMC1LC,
         MEHHP_LOD = URDMHHLC,
         MEOHP_LOD = URDMOHLC,
         MIBP_LOD = URDMIBLC,
         MECPP_LOD = URDECPLC,
         MCNP_LOD = URDCNPLC,
         MCOP_LOD = URDCOPLC,
         MHNC_LOD = URDMCHLC,
         MHIBP_LOD = URDHIBLC,
         MCOCH_LOD = URDMCOLC,
         MHBP_LOD = URDMHBLC,
         
         weight_phth = WTSB2YR) %>% select(SEQN, cycle, weight_phth, MBP, MEP, MEHP, MNP,
                                         MBzP, MVPP, MEHHP, MEOHP, MIBP, MECPP, MCNP, MCOP, MHNC,MHIBP, MCOCH, MHBP,
                                         MBP_LOD, MEP_LOD, MEHP_LOD, MNP_LOD,
                                         MBzP_LOD, MVPP_LOD, MEHHP_LOD, MEOHP_LOD, MIBP_LOD, MECPP_LOD,
                                         MCNP_LOD, MCOP_LOD, MHNC_LOD,
                                         MHIBP_LOD, MCOCH_LOD, MHBP_LOD)


## merge all datasets
phthalates <- bind_rows(phthalates_05, phthalates_07,
                        phthalates_09, phthalates_11, phthalates_13, phthalates_15)
phthalates <- phthalates %>%
  mutate(across(everything(), ~replace_na(., NA))) # Replace missing values with NA to make sure missingness is NA

#### urinary phenol #####
# 2005-2006
# subsample B
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/EPH_D.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
phenols_05 <- read_xpt(temp_file)
unlink(temp_file)

phenols_05 <- phenols_05 %>% mutate(cycle='2005-2006') %>%
  rename(BPA = URXBPH,
         BPA_LOD = URDBPHLC,
         weight_phenol = WTSB2YR) %>% select(SEQN,weight_phenol,cycle, BPA, BPA_LOD)

# 2007-2008
# subsample B
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/EPH_E.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
phenols_07 <- read_xpt(temp_file)
unlink(temp_file)

phenols_07 <- phenols_07 %>% mutate(cycle='2007-2008') %>%
  rename(BPA = URXBPH,
         BPA_LOD = URDBPHLC,
         
         weight_phenol = WTSB2YR) %>% select(SEQN,weight_phenol,cycle, BPA, BPA_LOD)


# 2009-2010
# subsample B
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/EPH_F.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
phenols_09 <- read_xpt(temp_file)
unlink(temp_file)

phenols_09 <- phenols_09 %>% mutate(cycle='2009-2010') %>%
  rename(BPA = URXBPH,
         BPA_LOD = URDBPHLC,
         weight_phenol = WTSB2YR) %>% select(SEQN,weight_phenol,cycle, BPA, BPA_LOD)

# 2011-2012
# subsample A
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/EPH_G.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
phenols_11 <- read_xpt(temp_file)
unlink(temp_file)

phenols_11 <- phenols_11 %>% mutate(cycle='2011-2012') %>%
  rename(BPA = URXBPH,
         BPA_LOD = URDBPHLC,
         
         weight_phenol = WTSA2YR
         ) %>% select(SEQN,weight_phenol,cycle, BPA, BPA_LOD)

# 2013-2014
# subsample B
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/EPHPP_H.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
phenols_13 <- read_xpt(temp_file)
unlink(temp_file)

phenols_13 <- phenols_13 %>% mutate(cycle='2013-2014') %>%
  rename(BPA = URXBPH,
         BPA_LOD = URDBPHLC,
         weight_phenol = WTSB2YR) %>% select(SEQN,weight_phenol,cycle, BPA, BPA_LOD)


# 2015-2016
# subsample B
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/EPHPP_I.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
phenols_15 <- read_xpt(temp_file)
unlink(temp_file)

phenols_15 <- phenols_15 %>% mutate(cycle='2015-2016') %>%
  rename(BPA = URXBPH,
         BPA_LOD = URDBPHLC,
         weight_phenol = WTSB2YR) %>% select(SEQN,weight_phenol, cycle, BPA, BPA_LOD)

## merge data
phenols <- bind_rows(phenols_05, phenols_07, phenols_09,
                     phenols_11, phenols_13, phenols_15)
phenols <- phenols %>%
  mutate(across(everything(), ~replace_na(., NA))) # Replace missing values with NA to make sure missingness is NA

#### summary ####
colnames(phthalates)
colnames(phenols)

chemicals <- merge(phthalates, phenols, by=c('SEQN','cycle'), all = TRUE)

#### urinary creatinine ####
# 2005-2006
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/ALB_CR_D.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
crea_05 <- read_xpt(temp_file)
unlink(temp_file)
crea_05 <- crea_05 %>% mutate(cycle = '2005-2006') %>% 
  rename(creatinine = URXUCR,
         albumin = URXUMA) %>% select(SEQN, cycle, creatinine,albumin)

# 2007-2008
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/ALB_CR_E.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
crea_07 <- read_xpt(temp_file)
unlink(temp_file)
crea_07 <- crea_07 %>% mutate(cycle = '2007-2008') %>% 
  rename(creatinine = URXUCR,
         albumin = URXUMA) %>% select(SEQN, cycle, creatinine,albumin)

# 2009-2010
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/ALB_CR_F.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
crea_09 <- read_xpt(temp_file)
unlink(temp_file)
crea_09 <- crea_09 %>% mutate(cycle = '2009-2010') %>% 
  rename(creatinine = URXUCR,
         albumin = URXUMA) %>% select(SEQN, cycle, creatinine,albumin)

# 2011-2012
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/ALB_CR_G.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
crea_11 <- read_xpt(temp_file)
unlink(temp_file)
crea_11 <- crea_11 %>% mutate(cycle = '2011-2012') %>% 
  rename(creatinine = URXUCR,
         albumin = URXUMA) %>% select(SEQN, cycle, creatinine,albumin)

# 2013-2014
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/ALB_CR_H.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
crea_13 <- read_xpt(temp_file)
unlink(temp_file)
crea_13 <- crea_13 %>% mutate(cycle = '2013-2014') %>% 
  rename(creatinine = URXUCR,
         albumin = URXUMA) %>% select(SEQN, cycle, creatinine,albumin)

# 2015-2016
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/ALB_CR_I.xpt"
temp_file <- tempfile(fileext = ".xpt")
GET(url, write_disk(temp_file))
crea_15 <- read_xpt(temp_file)
unlink(temp_file)
crea_15 <- crea_15 %>% mutate(cycle = '2015-2016') %>% 
  rename(creatinine = URXUCR,
         albumin = URXUMA) %>% select(SEQN, cycle, creatinine,albumin)


crea <- bind_rows( crea_05,crea_07,crea_09,crea_11,crea_13,crea_15)
crea <- crea %>%
  mutate(across(everything(), ~replace_na(., NA))) # Replace missing values with NA to make sure missingness is NA

chemicals_3 <- merge(chemicals, crea, by = c('SEQN','cycle'))
# write.csv(chemicals_3, file = 'chemicals_all.csv',row.names = FALSE)

