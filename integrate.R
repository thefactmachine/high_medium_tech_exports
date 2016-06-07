
# clear working space
rm(list = ls())

# load libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(WriteXLS)

# set some options
options(stringsAsFactors = FALSE)
options(scipen=999)


## PPL using MAC, outside of MBIE envirnment does not need to run it.
if( Sys.info()['sysname'] == 'Windows' ){
  ## set up R environment ---------------------------------------------------
  source("P:/R/common.Rprofile")
}


# function to read in source (i.e. raw data)
source("R/fn_read_me_data.R")

# reads and prepares the HS10 to anzsic to concordance.
source("R/fn_read_hs10_to_anzsic.R")

# this function creates an HS10 to high tech / medium tech lookup
source("R/fn_create_HS10_tech_lu.R")

# this function creates a category called "Other" this is stuff
# in the HS10_to_ANZSIC mapping but not in the "tech_lookups.csv"
source("R/fn_create_df_other.R")

# this function creates a data.frame called "df_unclassified_lu"
# it contains all the HS10 codes in the raw data but not in "df_hS10_to_anzsic"
source("R/fn_create_unmatched_lu.R")

# ========================
# End of the starting stuff...wir lassen programming:
# =======================

# read in and process raw merchandise exports (me) data
df_me_exports <- fn_read_me_data()

df_hS10_to_anzsic <- fn_read_hs10_to_anzsic()
# ASSERT: primary key = HS10
stopifnot(length(unique(df_hS10_to_anzsic$HS10)) == nrow(df_hS10_to_anzsic))

df_HS10_tech_lu <- fn_create_HS10_tech_lu(df_hS10_to_anzsic)
# ASSERT: primary key = HS10
stopifnot(length(unique(df_HS10_tech_lu$HS10)) == nrow(df_HS10_tech_lu))

# create data.frame of in(df_hS10_to_anzsic) but_not_in(df_HS10_tech_lu)
df_other_lu <- fn_create_df_other(df_hS10_to_anzsic, df_HS10_tech_lu)
# ASSERT: primary key = HS10
stopifnot(length(unique(df_other_lu$HS10)) == nrow(df_other_lu))

# ASSERT: df_hS10_to_anzsic has been partitioned into: df_HS10_tech_lu + df_other_lu
stopifnot(nrow(df_hS10_to_anzsic) == (nrow(df_HS10_tech_lu) + nrow(df_other_lu)))

df_unclassified <- fn_create_unmatched_lu(df_me_exports, df_hS10_to_anzsic)
# ASSERT: primary key = HS10
stopifnot(length(unique(df_unclassified$HS10)) == nrow(df_unclassified))

# create a master lookup data.frame
df_master_lu <- bind_rows(df_HS10_tech_lu, df_other_lu, df_unclassified)

# ASSERT: df_master_lu contains all the HS10 codes in the original data
stopifnot(sum(!unique(df_me_exports$HS10) %in% df_master_lu$HS10) == 0)
# ASSERT: primary key = HS10
length(unique(df_master_lu$HS10)) == nrow(df_master_lu)

# NOW, that we have created df_master_lu, keep only the necessary stuff
rm(list = ls()[!(ls() %in% c('df_master_lu','df_me_exports'))])


# combine the original data with the lookup information
df_combined <- inner_join(df_me_exports, df_master_lu, by = c("HS10" = "HS10")) %>% 
                rename(date = Date, year = Year, total = Total) %>% ungroup()

# Assert that the value of all years before 2015 equals a known total
stopifnot(df_combined %>% filter(year < 2015) %>% summarise(total = sum(total)) %>% .$total == 572063404521)

df_by_year_by_tech <- df_combined %>% group_by(year, sort, tech_type, main, sub) %>% 
                      summarise(total = sum(total)) %>% 
                      arrange(year, sort) %>% select(-sort) %>%
                      spread(year, total) %>% as.data.frame()

WriteXLS("df_by_year_by_tech", "outputs/output.xlsx")

