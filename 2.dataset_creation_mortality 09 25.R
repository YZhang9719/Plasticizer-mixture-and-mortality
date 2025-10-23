#### combining mortality data ####
# ** MORTALITY FOLLOW-UP THROUGH DECEMBER 31, 2019 **
library(readr)
library(dplyr)

setwd("Directory of downloaded mortality data")

# 2005-2006
srvyin <- paste("NHANES_2005_2006_MORT_2019_PUBLIC.dat")   
srvyout <- "death_05" 
dsn <- read_fwf(file=srvyin,
                col_types = "iiiiiiii",
                fwf_cols(SEQN = c(1,6),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = c("", ".")
)
dsn <- dsn %>% filter(eligstat==1) %>% select(SEQN, mortstat,ucod_leading,permth_exm)
assign(paste0(srvyout), dsn)

# 2007-2008
srvyin <- paste("NHANES_2007_2008_MORT_2019_PUBLIC.dat")   
srvyout <- "death_07" 
dsn <- read_fwf(file=srvyin,
                col_types = "iiiiiiii",
                fwf_cols(SEQN = c(1,6),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = c("", ".")
)
dsn <- dsn %>% filter(eligstat==1) %>% select(SEQN, mortstat,ucod_leading,permth_exm)
assign(paste0(srvyout), dsn)

# 2009-2010
srvyin <- paste("NHANES_2009_2010_MORT_2019_PUBLIC.dat")   
srvyout <- "death_09" 
dsn <- read_fwf(file=srvyin,
                col_types = "iiiiiiii",
                fwf_cols(SEQN = c(1,6),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = c("", ".")
)
dsn <- dsn %>% filter(eligstat==1) %>% select(SEQN, mortstat,ucod_leading,permth_exm)
assign(paste0(srvyout), dsn)

# 2011-2012
srvyin <- paste("NHANES_2011_2012_MORT_2019_PUBLIC.dat")   
srvyout <- "death_11" 
dsn <- read_fwf(file=srvyin,
                col_types = "iiiiiiii",
                fwf_cols(SEQN = c(1,6),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = c("", ".")
)
dsn <- dsn %>% filter(eligstat==1) %>% select(SEQN, mortstat,ucod_leading,permth_exm)
assign(paste0(srvyout), dsn)

# 2013-2014
srvyin <- paste("NHANES_2013_2014_MORT_2019_PUBLIC.dat")   
srvyout <- "death_13" 
dsn <- read_fwf(file=srvyin,
                col_types = "iiiiiiii",
                fwf_cols(SEQN = c(1,6),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = c("", ".")
)
dsn <- dsn %>% filter(eligstat==1) %>% select(SEQN, mortstat,ucod_leading,permth_exm)
assign(paste0(srvyout), dsn)

# 2015-2016
srvyin <- paste("NHANES_2015_2016_MORT_2019_PUBLIC.dat")   
srvyout <- "death_15" 
dsn <- read_fwf(file=srvyin,
                col_types = "iiiiiiii",
                fwf_cols(SEQN = c(1,6),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = c("", ".")
)
dsn <- dsn %>% filter(eligstat==1) %>% select(SEQN, mortstat,ucod_leading,permth_exm)
assign(paste0(srvyout), dsn)


death <- rbind(death_05, death_07, death_09, death_11, death_13, death_15)

# write.csv(death,'Mortality_2003_2016.csv', row.names=FALSE)

